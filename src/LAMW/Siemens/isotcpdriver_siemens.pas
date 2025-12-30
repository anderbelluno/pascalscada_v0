unit ISOTCPDriver_Siemens;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, TCPPort, S7Transport, AndroidLog;

type
  THandlerItem = class
    Handler: TS7FrameReceived;
  end;

  TISOTCPDriver = class(TComponent)
  private
    FPort: TPortTCP;
    FOnFrame: TS7FrameReceived;
    FOnS7Connected: TNotifyEvent;
    FFrameListeners: array of TS7FrameReceived;
    FRequestQueue: TList;
    FNextHandler: TS7FrameReceived;
    procedure PortOnFrame(Sender: TObject; const Frame: TBytes);
    procedure SetOnS7Connected(Value: TNotifyEvent);
    function SameListener(a, b: TS7FrameReceived): Boolean;
  public
    constructor Create(AOwner: TComponent; APort: TPortTCP); reintroduce;
    destructor Destroy; override;
    function SetHost(const AHost: string): TISOTCPDriver;
    function SetPort(const APortNum: Integer): TISOTCPDriver;
    function SetRack(const ARack: Integer): TISOTCPDriver;
    function SetSlot(const ASlot: Integer): TISOTCPDriver;
    function Connect: TISOTCPDriver;
    function Disconnect: TISOTCPDriver;
    procedure StartHandshake;
    procedure SendPDU(const Data: TBytes); overload;
    procedure SendPDU(const Data: TBytes; Handler: TS7FrameReceived); overload;
    property OnFrameReceived: TS7FrameReceived read FOnFrame write FOnFrame;
    property OnS7Connected: TNotifyEvent read FOnS7Connected write SetOnS7Connected;
    function AddFrameListener(L: TS7FrameReceived): TISOTCPDriver;
    function RemoveFrameListener(L: TS7FrameReceived): TISOTCPDriver;
    function SetActiveHandler(L: TS7FrameReceived): TISOTCPDriver;
  end;

implementation

constructor TISOTCPDriver.Create(AOwner: TComponent; APort: TPortTCP);
begin
  inherited Create(AOwner);
  FPort := APort;
  FPort.OnFrameReceived := PortOnFrame;
  FRequestQueue := TList.Create;
end;

destructor TISOTCPDriver.Destroy;
var
  i: Integer;
begin
  if Assigned(FRequestQueue) then
  begin
    for i := 0 to FRequestQueue.Count - 1 do
      TObject(FRequestQueue[i]).Free;
    FRequestQueue.Free;
  end;
  inherited Destroy;
end;

function TISOTCPDriver.SetHost(const AHost: string): TISOTCPDriver;
begin
  FPort.Host := AHost;
  Result     := Self;
end;

function TISOTCPDriver.SetPort(const APortNum: Integer): TISOTCPDriver;
begin
  FPort.Port := APortNum;
  Result     := Self;
end;

function TISOTCPDriver.SetRack(const ARack: Integer): TISOTCPDriver;
begin
  FPort.Rack := ARack;
  Result     := Self;
end;

function TISOTCPDriver.SetSlot(const ASlot: Integer): TISOTCPDriver;
begin
  FPort.Slot := ASlot;
  Result     := Self;
end;

function TISOTCPDriver.Connect: TISOTCPDriver;
begin
  LogD('PLC', 'Driver.Connect');
  FPort.Connect;
  Result := Self;
end;

function TISOTCPDriver.Disconnect: TISOTCPDriver;
begin
  FPort.Disconnect;
  Result := Self;
end;

procedure TISOTCPDriver.StartHandshake;
begin
  FPort.StartHandshake;
end;

procedure TISOTCPDriver.SendPDU(const Data: TBytes);
begin
  SendPDU(Data, FNextHandler);
  FNextHandler := nil;
end;

procedure TISOTCPDriver.SendPDU(const Data: TBytes; Handler: TS7FrameReceived);
var
  FullPDU: TBytes;
  Len, ParamLen: Integer;
  Item: THandlerItem;
begin
  if Assigned(Handler) then
  begin
    Item := THandlerItem.Create;
    Item.Handler := Handler;
    FRequestQueue.Add(Item);
  end;

  ParamLen := Length(Data);
  // TPKT(4) + COTP(3) + S7Header(10) + Param
  Len := 4 + 3 + 10 + ParamLen;
  SetLength(FullPDU, Len);

  // TPKT
  FullPDU[0] := $03;
  FullPDU[1] := $00;
  FullPDU[2] := (Len shr 8) and $FF;
  FullPDU[3] := Len and $FF;

  // COTP (DT Data)
  FullPDU[4] := $02; // Length 2
  FullPDU[5] := $F0; // PDU Type: DT
  FullPDU[6] := $80; // TPDU Number / EOT

  // S7 Header (Job)
  FullPDU[7] := $32; // Protocol ID
  FullPDU[8] := $01; // ROSCTR: Job
  FullPDU[9] := $00; // Reserved
  FullPDU[10] := $00; // Reserved
  FullPDU[11] := $01; // PDU Ref Hi (Fixed for now)
  FullPDU[12] := $01; // PDU Ref Lo
  FullPDU[13] := (ParamLen shr 8) and $FF; // Param Length
  FullPDU[14] := ParamLen and $FF;
  FullPDU[15] := $00; // Data Length Hi (Read requests have 0 data)
  FullPDU[16] := $00; // Data Length Lo

  LogD('PLC', 'S7 Header Sent: ParamLen=' + IntToStr(ParamLen));

  // Copy Parameter (Var Spec)
  Move(Data[0], FullPDU[17], ParamLen);

  //AppLog('PLC', 'Driver.SendPDU Wrapped ' + IntToStr(Len) + ' bytes');
  FPort.Send(FullPDU);
end;

procedure TISOTCPDriver.PortOnFrame(Sender: TObject; const Frame: TBytes);
var
  FullPDU: TBytes;
  i: Integer;
  Item: THandlerItem;
begin
  if FRequestQueue.Count > 0 then
  begin
    Item := THandlerItem(FRequestQueue[0]);
    if Assigned(Item.Handler) then
      Item.Handler(Self, Frame);
    FRequestQueue.Delete(0);
    Item.Free;
  end
  else
  if Assigned(FOnFrame) then
    FOnFrame(Self, Frame);

  if Length(FFrameListeners) > 0 then
  begin
    for i := 0 to High(FFrameListeners) do
      if Assigned(FFrameListeners[i]) then
        FFrameListeners[i](Self, Frame);
  end;
end;

procedure TISOTCPDriver.SetOnS7Connected(Value: TNotifyEvent);
begin
  FOnS7Connected := Value;
  FPort.OnS7Connected := Value;
end;

function TISOTCPDriver.SameListener(a, b: TS7FrameReceived): Boolean;
var
  ma, mb: TMethod;
begin
  ma := TMethod(a);
  mb := TMethod(b);
  Result := (ma.Code = mb.Code) and (ma.Data = mb.Data);
end;

function TISOTCPDriver.AddFrameListener(L: TS7FrameReceived): TISOTCPDriver;
var
  i: Integer;
begin
  for i := 0 to High(FFrameListeners) do
    if SameListener(FFrameListeners[i], L) then
      Exit(Self);
  i := Length(FFrameListeners);
  SetLength(FFrameListeners, i + 1);
  FFrameListeners[i] := L;
  Result := Self;
end;

function TISOTCPDriver.RemoveFrameListener(L: TS7FrameReceived): TISOTCPDriver;
var
  i, j: Integer;
begin
  for i := 0 to High(FFrameListeners) do
    if SameListener(FFrameListeners[i], L) then
    begin
      for j := i to High(FFrameListeners) - 1 do
        FFrameListeners[j] := FFrameListeners[j + 1];
      SetLength(FFrameListeners, Length(FFrameListeners) - 1);
      Break;
    end;
  Result := Self;
end;

function TISOTCPDriver.SetActiveHandler(L: TS7FrameReceived): TISOTCPDriver;
begin
  FNextHandler := L;
  Result := Self;
end;

end.
