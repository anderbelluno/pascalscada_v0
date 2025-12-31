unit PLCStruct;

{$mode delphi}

interface

uses
  Classes, SysUtils, ISOTCPDriver_Siemens, S7PDU, S7Parser, S7Types_Siemens, AndroidLog, Laz_And_Controls;

type
  TPLCStruct = class(TComponent)
  private
    FDriver: TISOTCPDriver;
    FArea: TS7Area;
    FDB: Word;
    FOffset: LongWord;
    FSize: Word;
    FScanInterval: Integer;
    FAutoRead: Boolean;
    FTimer: jTimer;
    FData: TBytes;
    FListeners: TList;
    procedure InternalTimer(Sender: TObject);
    procedure EnsureTimer;
    procedure KillTimer;
    procedure DriverOnFrame(Sender: TObject; const Frame: TBytes);
    function GetDataBytes(const Frame: TBytes): TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connection(Drv: TISOTCPDriver): TPLCStruct;
    function SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCStruct;
    function SetArea(AArea: TS7Area): TPLCStruct;
    function SetScanInterval(Value: Integer): TPLCStruct;
    function SetAutoRead(Value: Boolean): TPLCStruct;
    procedure Read;
    procedure AddListener(Listener: TNotifyEvent);
    procedure RemoveListener(Listener: TNotifyEvent);
    function GetByte(Offset: Integer): Byte;
    function GetWord(Offset: Integer): Word;
    function GetDWord(Offset: Integer): LongWord;
    property Data: TBytes read FData;
  end;

implementation


constructor TPLCStruct.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
  SetLength(FData, 0);
end;

destructor TPLCStruct.Destroy;
begin
  KillTimer;
  FListeners.Free;
  inherited Destroy;
end;

function TPLCStruct.Connection(Drv: TISOTCPDriver): TPLCStruct;
begin
  FDriver := Drv;
  Result := Self;
end;

function TPLCStruct.SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCStruct;
begin
  FArea := saDB;
  FDB := ADB;
  FOffset := AOffset;
  FSize := ASize;
  Result := Self;
end;

function TPLCStruct.SetArea(AArea: TS7Area): TPLCStruct;
begin
  FArea := AArea;
  Result := Self;
end;

function TPLCStruct.SetScanInterval(Value: Integer): TPLCStruct;
begin
  FScanInterval := Value;
  if (FScanInterval <= 0) or (not FAutoRead) then
    KillTimer
  else
    EnsureTimer;
  Result := Self;
end;

function TPLCStruct.SetAutoRead(Value: Boolean): TPLCStruct;
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

procedure TPLCStruct.EnsureTimer;
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

procedure TPLCStruct.KillTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;
end;

procedure TPLCStruct.InternalTimer(Sender: TObject);
begin
  Read;
end;

procedure TPLCStruct.Read;
var
  pdu: TBytes;
begin
  if Assigned(FDriver) then
  begin
    pdu := S7PDU.BuildReadVar(FArea, FDB, FOffset, FSize, tsByte);
    FDriver.SendPDU(pdu, DriverOnFrame);
  end;
end;

function TPLCStruct.GetDataBytes(const Frame: TBytes): TBytes;
var
  i, Offset: Integer;
begin
  SetLength(Result, FSize);
  Offset := 25; // Standard S7 Read Response Data Offset
  for i := 0 to FSize - 1 do
    if (Offset + i) < Length(Frame) then
      Result[i] := Frame[Offset + i]
    else
      Result[i] := 0;
end;

procedure TPLCStruct.DriverOnFrame(Sender: TObject; const Frame: TBytes);
var
  i: Integer;
  Method: TMethod;
  NotifyEvent: TNotifyEvent;
begin
  if not S7Parser.IsReadResponseOK(Frame) then Exit;
  FData := GetDataBytes(Frame);
  
  // Notify listeners
  for i := 0 to FListeners.Count - 1 do
  begin
    Method := TMethod(FListeners[i]^);
    NotifyEvent := TNotifyEvent(Method);
    if Assigned(NotifyEvent) then
      NotifyEvent(Self);
  end;
end;

procedure TPLCStruct.AddListener(Listener: TNotifyEvent);
var
  P: ^TMethod;
begin
  New(P);
  P^ := TMethod(Listener);
  FListeners.Add(P);
end;

procedure TPLCStruct.RemoveListener(Listener: TNotifyEvent);
var
  i: Integer;
  P: ^TMethod;
  Target: TMethod;
begin
  Target := TMethod(Listener);
  for i := FListeners.Count - 1 downto 0 do
  begin
    P := FListeners[i];
    if (P^.Code = Target.Code) and (P^.Data = Target.Data) then
    begin
      Dispose(P);
      FListeners.Delete(i);
      Break;
    end;
  end;
end;

function TPLCStruct.GetByte(Offset: Integer): Byte;
begin
  if (Offset >= 0) and (Offset < Length(FData)) then
    Result := FData[Offset]
  else
    Result := 0;
end;

function TPLCStruct.GetWord(Offset: Integer): Word;
begin
  if (Offset >= 0) and (Offset + 1 < Length(FData)) then
    Result := (FData[Offset] shl 8) or FData[Offset + 1]
  else
    Result := 0;
end;

function TPLCStruct.GetDWord(Offset: Integer): LongWord;
begin
  if (Offset >= 0) and (Offset + 3 < Length(FData)) then
    Result := (FData[Offset] shl 24) or (FData[Offset + 1] shl 16) or (FData[Offset + 2] shl 8) or FData[Offset + 3]
  else
    Result := 0;
end;

end.
