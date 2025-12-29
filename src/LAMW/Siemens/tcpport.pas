unit TCPPort;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, tcpsocketclient, S7Transport, AndroidLog;

type
  TS7FrameReceived = procedure(Sender: TObject; const Frame: TBytes) of object;

  { TPortTCP }

  TPortTCP = class(TComponent)
  private
    FSocket: jTCPSocketClient;
    FTransport: TS7Transport;
    FHost: string;
    FPort: Integer;
    FOnFrame: TS7FrameReceived;
    FRack: Integer;
    FSlot: Integer;
    FConnType: Byte; // 1=PG, 2=OP
    FOnS7Connected: TNotifyEvent;
    FOwnsSocket: Boolean;
    function GetInitialized: Boolean;
    procedure SocketConnected(Sender: TObject);
    procedure SocketDisconnected(Sender: TObject);
    procedure TransportFrameReceived(Sender: TObject; const Frame: TBytes);
    procedure TransportS7Connected(Sender: TObject);
    procedure SetConnType(Value: Byte);
  public
    constructor Create(AOwner: TComponent; ASocket: jTCPSocketClient = nil); reintroduce;
    destructor Destroy; override;
    procedure Connect(const Host: string; Port: Integer); overload;
    procedure Connect; overload;
    procedure Disconnect;
    procedure StartHandshake;
    procedure Send(const Data: TBytes);
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property OnFrameReceived: TS7FrameReceived read FOnFrame write FOnFrame;
    property OnS7Connected: TNotifyEvent read FOnS7Connected write FOnS7Connected;
    property Rack: Integer read FRack write FRack;
    property Slot: Integer read FSlot write FSlot;
    property ConnType: Byte read FConnType write SetConnType;
    property Initialized: Boolean read GetInitialized;
  end;

implementation

procedure BytesFromJBytes(var Dest: TBytes; const Source: TDynArrayOfJByte);
var
  i: Integer;
begin
  SetLength(Dest, Length(Source));
  for i := 0 to High(Source) do
    Dest[i] := Byte(Source[i]);
end;

{ TPortTCP }

constructor TPortTCP.Create(AOwner: TComponent; ASocket: jTCPSocketClient);
begin
  inherited Create(AOwner);
  // 1. Setup Socket
  if Assigned(ASocket) then
  begin
    FSocket := ASocket;
    FOwnsSocket := False;
  end
  else
  begin
    FSocket := jTCPSocketClient.Create(AOwner); // Use AOwner (Form) to ensure Context
    FOwnsSocket := True;
  end;

  FSocket.OnConnected := SocketConnected;
  FSocket.OnDisconnected := SocketDisconnected;
  // NOTE: We do NOT set OnBytesReceived here. TS7Transport takes control of it.

  // 2. Create Transport (Passing Socket)
  FTransport := TS7Transport.Create(Self, FSocket);
  FTransport.OnFrameReceived := TransportFrameReceived;
  FTransport.OnS7Connected := TransportS7Connected;

  // 3. Defaults
  FRack := 0;
  FSlot := 1;
  FConnType := 2; // Default OP
end;

destructor TPortTCP.Destroy;
begin
  if Assigned(FSocket) then
  begin
    if FSocket.IsConnected then
      FSocket.CloseConnection;

    // Unhook events to prevent crashes if Socket survives this object
    FSocket.OnConnected := nil;
    FSocket.OnDisconnected := nil;
    FSocket.OnBytesReceived := nil;
  end;
  
  FreeAndNil(FTransport);
  
  if FOwnsSocket then
    FreeAndNil(FSocket);
    
  inherited Destroy;
end;procedure TPortTCP.Connect(const Host: string; Port: Integer);
begin
  FHost := Host;
  FPort := Port;
  Connect;
end;

procedure TPortTCP.Connect;
begin
  if (FHost = '') or (FPort = 0) then
  begin
    AndroidLog.LogD('PLC', 'Error: Host or Port not set');
    Exit;
  end;

  // Update TSAP before connecting
  if Assigned(FTransport) then
    FTransport.SetTSAP($0100, (FConnType shl 8) + ((FRack shl 5) + FSlot));

  AndroidLog.LogD('PLC', 'Connecting to ' + FHost + ':' + IntToStr(FPort));
  FSocket.ConnectAsync(FHost, FPort);
end;

procedure TPortTCP.Disconnect;
begin
  if Assigned(FSocket) and FSocket.IsConnected then
    FSocket.CloseConnection;
end;

procedure TPortTCP.StartHandshake;
begin
  if Assigned(FTransport) then
    FTransport.StartHandshake;
end;

procedure TPortTCP.Send(const Data: TBytes);
begin
  if Assigned(FTransport) then
    FTransport.Send(Data);
end;

procedure TPortTCP.SocketConnected(Sender: TObject);
begin
  AndroidLog.LogD('PLC', 'Socket Connected');
  // Initiate S7 Handshake immediately after TCP connection
  if Assigned(FTransport) then
    FTransport.StartHandshake;
end;

procedure TPortTCP.SocketDisconnected(Sender: TObject);
begin
  AndroidLog.LogD('PLC', 'Socket Disconnected');
end;

procedure TPortTCP.TransportFrameReceived(Sender: TObject; const Frame: TBytes);
begin
  // Forward S7 PDU to Driver/User
  if Assigned(FOnFrame) then
    FOnFrame(Self, Frame);
end;

procedure TPortTCP.TransportS7Connected(Sender: TObject);
begin
  AndroidLog.LogD('PLC', 'S7 Protocol Connected');
  // Forward Connection Event to Driver/User
  if Assigned(FOnS7Connected) then
    FOnS7Connected(Self);
end;

function TPortTCP.GetInitialized: Boolean;
begin
  Result := Assigned(FSocket) and FSocket.IsConnected;
end;

procedure TPortTCP.SetConnType(Value: Byte);
begin
  if (Value in [1, 2]) and (Value <> FConnType) then
  begin
    FConnType := Value;
    if Assigned(FTransport) then
      FTransport.SetTSAP($0100, (FConnType shl 8) + ((FRack shl 5) + FSlot));
  end;
end;

end.
