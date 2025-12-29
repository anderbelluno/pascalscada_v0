unit PLCString;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls,
  ISOTCPDriver_Siemens, S7Types_Siemens, S7PDU, S7Parser;

type
  TOnValue = procedure(Sender: TObject; const ValueText: string) of object;

  TPLCString = class(TComponent)
  private
    FDriver: TISOTCPDriver;
    FArea: TS7Area;
    FDB: Word;
    FOffset: LongWord;
    FSize: Word;
    FOnValueChange: TOnValue;
    FScanInterval: Integer;
    FTimer: jTimer;
    FMemReadFunction: Integer;
    FAutoRead: Boolean;
    FLastValue: string;
    FValueValid: Boolean;
    procedure InternalTimer(Sender: TObject);
    procedure EnsureTimer;
    procedure KillTimer;
    procedure InvalidateValue;
    function GetScanInterval: Integer;
    function GetDataBytes(const Frame: TBytes): TBytes;
    procedure DriverOnFrame(Sender: TObject; const Frame: TBytes);
  public
    constructor Create(AOwner: TComponent); override;
    function Connection(Drv: TISOTCPDriver): TPLCString;
    function SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCString;
    function SetArea(AArea: TS7Area): TPLCString;
    function SetScanInterval(Value: Integer): TPLCString;
    function SetAutoRead(Value: Boolean): TPLCString;
    function SetMemReadFunction(Func: Integer): TPLCString;
    function SetMemFileDB(DB: Word): TPLCString;
    function SetMemAddress(Offset: LongWord): TPLCString;
    procedure Read;
    property OnValueChange: TOnValue  read FOnValueChange write FOnValueChange;
    property ScanInterval: Integer read GetScanInterval write FScanInterval;
    property AutoRead: Boolean read FAutoRead write FAutoRead;
  end;

implementation

constructor TPLCString.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueValid := False;
  FLastValue := '';
end;

procedure TPLCString.InvalidateValue;
begin
  FValueValid := False;
  FLastValue := '';
end;

function TPLCString.Connection(Drv: TISOTCPDriver): TPLCString;
begin
  FDriver := Drv;
  Result := Self;
end;

function TPLCString.SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCString;
begin
  InvalidateValue;
  FDB := ADB;
  FOffset := AOffset;
  FSize := ASize;
  Result := Self;
end;

function TPLCString.SetArea(AArea: TS7Area): TPLCString;
begin
  InvalidateValue;
  FArea := AArea;
  Result := Self;
end;

function TPLCString.SetScanInterval(Value: Integer): TPLCString;
begin
  FScanInterval := Value;
  if (FScanInterval <= 0) or (not FAutoRead) then
    KillTimer
  else
    EnsureTimer;
  Result := Self;
end;

function TPLCString.SetAutoRead(Value: Boolean): TPLCString;
begin
  if FAutoRead <> Value then
  begin
    FAutoRead := Value;
    if FAutoRead then
      EnsureTimer
    else
      KillTimer;
  end;
  Result := Self;
end;

function TPLCString.SetMemReadFunction(Func: Integer): TPLCString;
begin
  FMemReadFunction := Func;
  case Func of
    1: SetArea(saInputs);
    2: SetArea(saOutputs);
    3: SetArea(saFlags);
    4: SetArea(saDB);
    5: SetArea(saCounters);
    6: SetArea(saTimers);
  else
    SetArea(saDB);
  end;
  Result := Self;
end;

function TPLCString.SetMemFileDB(DB: Word): TPLCString;
begin
  InvalidateValue;
  FDB := DB;
  Result := Self;
end;

function TPLCString.SetMemAddress(Offset: LongWord): TPLCString;
begin
  InvalidateValue;
  FOffset := Offset;
  Result := Self;
end;

procedure TPLCString.InternalTimer(Sender: TObject);
begin
  Read;
end;

procedure TPLCString.EnsureTimer;
begin
  if not FAutoRead then Exit;
  if (FTimer = nil) and (FScanInterval > 0) then
  begin
    if Owner <> nil then
      FTimer := jTimer.Create(Owner)
    else
      FTimer := jTimer.Create(nil);
    FTimer.Init;
    FTimer.Interval := FScanInterval;
    FTimer.OnTimer := InternalTimer;
    FTimer.Enabled := True;
  end
  else if (FTimer <> nil) then
  begin
    FTimer.Interval := FScanInterval;
    FTimer.Enabled := FScanInterval > 0;
  end;
end;

procedure TPLCString.KillTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;
end;

function TPLCString.GetScanInterval: Integer;
begin
  Result := FScanInterval;
end;

function TPLCString.GetDataBytes(const Frame: TBytes): TBytes;
var
  i, Offset: Integer;
begin
  SetLength(Result, FSize);
  Offset := 25;
  for i := 0 to FSize - 1 do
    if (Offset + i) < Length(Frame) then
      Result[i] := Frame[Offset + i]
    else
      Result[i] := 0;
end;

procedure TPLCString.Read;
var
  pdu: TBytes;
begin
  pdu := S7PDU.BuildReadVar(FArea, FDB, FOffset, FSize, tsByte);
  FDriver.SendPDU(pdu, DriverOnFrame);
end;

procedure TPLCString.DriverOnFrame(Sender: TObject; const Frame: TBytes);
var
  data: TBytes;
  st: Integer;
  s: string;
  i: Integer;
begin
  if not S7Parser.IsReadResponseOK(Frame) then Exit;
  data := GetDataBytes(Frame);
  if Length(data) < 2 then Exit;
  st := data[1];
  if st > Length(data) - 2 then st := Length(data) - 2;
  s := '';
  for i := 0 to st - 1 do
    s := s + Chr(data[2 + i]);
  if (not FValueValid) or (s <> FLastValue) then
  begin
    FValueValid := True;
    FLastValue := s;
    if Assigned(FOnValueChange) then FOnValueChange(Self, s);
  end;
end;

end.
