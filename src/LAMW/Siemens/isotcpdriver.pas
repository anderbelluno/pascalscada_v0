unit ISOTCPDriver;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, TCPPort, S7Transport, AndroidLog;

type
  TISOTCPDriver = class(TComponent)
  private
    FPort: TPortTCP;
    FOnFrame: TS7FrameReceived;
    procedure PortOnFrame(Sender: TObject; const Frame: TBytes);
  public
    constructor Create(AOwner: TComponent; APort: TPortTCP); reintroduce;
    function SetHost(const AHost: string): TISOTCPDriver;
    function SetPort(const APortNum: Integer): TISOTCPDriver;
    function Connect: TISOTCPDriver;
    function Disconnect: TISOTCPDriver;
    procedure SendPDU(const Data: TBytes);
    property OnFrameReceived: TS7FrameReceived read FOnFrame write FOnFrame;
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

function TISOTCPDriver.Connect: TISOTCPDriver;
begin
  //LogD('PLC', 'Driver.Connect');
  FPort.Connect;
  Result := Self;
end;

function TISOTCPDriver.Disconnect: TISOTCPDriver;
begin
  FPort.Disconnect;
  Result := Self;
end;

procedure TISOTCPDriver.SendPDU(const Data: TBytes);
begin
  //LogD('PLC', 'Driver.SendPDU ' + IntToStr(Length(Data)) + ' bytes');
  FPort.Send(Data);
end;

procedure TISOTCPDriver.PortOnFrame(Sender: TObject; const Frame: TBytes);
begin
  //LogD('PLC', 'Driver.PortOnFrame len=' + IntToStr(Length(Frame)));
  if Assigned(FOnFrame) then FOnFrame(Self, Frame);
end;

end.
