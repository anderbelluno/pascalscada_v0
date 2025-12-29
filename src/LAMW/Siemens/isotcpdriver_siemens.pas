unit ISOTCPDriver_Siemens;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, TCPPort, S7Transport, AndroidLog;

type
  TISOTCPDriver = class(TComponent)
  private
    FPort: TPortTCP;
    FOnFrame: TS7FrameReceived;
    FOnS7Connected: TNotifyEvent;
    procedure PortOnFrame(Sender: TObject; const Frame: TBytes);
    procedure SetOnS7Connected(Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent; APort: TPortTCP); reintroduce;
    function SetHost(const AHost: string): TISOTCPDriver;
    function SetPort(const APortNum: Integer): TISOTCPDriver;
    function SetRack(const ARack: Integer): TISOTCPDriver;
    function SetSlot(const ASlot: Integer): TISOTCPDriver;
    function Connect: TISOTCPDriver;
    function Disconnect: TISOTCPDriver;
    procedure StartHandshake;
    procedure SendPDU(const Data: TBytes);
    property OnFrameReceived: TS7FrameReceived read FOnFrame write FOnFrame;
    property OnS7Connected: TNotifyEvent read FOnS7Connected write SetOnS7Connected;
  end;

implementation

constructor TISOTCPDriver.Create(AOwner: TComponent; APort: TPortTCP);
begin
  inherited Create(AOwner);
  FPort := APort;
  FPort.OnFrameReceived := PortOnFrame;
end;

function TISOTCPDriver.SetHost(const AHost: string): TISOTCPDriver;
begin
  FPort.Host := AHost;
  Result := Self;
end;

function TISOTCPDriver.SetPort(const APortNum: Integer): TISOTCPDriver;
begin
  FPort.Port := APortNum;
  Result := Self;
end;

function TISOTCPDriver.SetRack(const ARack: Integer): TISOTCPDriver;
begin
  FPort.Rack := ARack;
  Result := Self;
end;

function TISOTCPDriver.SetSlot(const ASlot: Integer): TISOTCPDriver;
begin
  FPort.Slot := ASlot;
  Result := Self;
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
var
  FullPDU: TBytes;
  Len, ParamLen: Integer;
begin
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
begin
  //AppLog('PLC', 'Driver.PortOnFrame len=' + IntToStr(Length(Frame)));
  if Assigned(FOnFrame) then FOnFrame(Self, Frame);
end;

procedure TISOTCPDriver.SetOnS7Connected(Value: TNotifyEvent);
begin
  FOnS7Connected := Value;
  FPort.OnS7Connected := Value;
end;

end.
