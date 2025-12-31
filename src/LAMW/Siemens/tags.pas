unit Tags;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls, S7Types_Siemens, S7PDU,
  S7Parser, ISOTCPDriver_Siemens, AndroidLog, PLCStructElement;

type

  TPLCNumberKind = (nkByte, nkWord, nkDWord, nkInt, nkDInt, nkReal);

  TOnValueChange = procedure(Sender: TObject; const ValueText: string) of object;

  TOnValueBool  = procedure(Sender: TObject; Value: Boolean) of object;
  TOnValueByte  = procedure(Sender: TObject; Value: Byte) of object;
  TOnValueWord  = procedure(Sender: TObject; Value: Word) of object;
  TOnValueDWord = procedure(Sender: TObject; Value: LongWord) of object;
  TOnValueInt16 = procedure(Sender: TObject; Value: SmallInt) of object;
  TOnValueDInt  = procedure(Sender: TObject; Value: LongInt) of object;
  TOnValueReal  = procedure(Sender: TObject; Value: Single) of object;

  TPLCTagNumber = class(TComponent)
  private
    FDriver: TISOTCPDriver;
    FArea: TS7Area;
    FDB: Word;
    FOffset: LongWord;
    FSize: Word;
    FTS: TS7TransportSize;
    FTagType: TS7TagType;
    FBitIndex: Integer;
    FOnValueBool:  TOnValueBool;
    FOnValueByte:  TOnValueByte;
    FOnValueWord:  TOnValueWord;
    FOnValueDWord: TOnValueDWord;
    FOnValueInt16: TOnValueInt16;
    FOnValueDInt:  TOnValueDInt;
    FOnValueReal:  TOnValueReal;
    FOnValueChange: TOnValueChange;
    FScanInterval: Integer;
    FTimer: jTimer;
    FSwapBytes: Boolean;
    FSwapWords: Boolean;
    FSwapDWords: Boolean;
    FMemReadFunction: Integer;
    FMemWriteFunction: Integer;
    FAutoRead: Boolean;
    FLastValue: string;
    FValueValid: Boolean;
    procedure InternalTimer(Sender: TObject);
    procedure EnsureTimer;
    procedure KillTimer;
    procedure InvalidateValue;
    function GetScanInterval: Integer;
    function GetDataBytes(const Frame: TBytes): TBytes;
    procedure ApplySwaps(var Data: TBytes);
    procedure DriverOnFrame(Sender: TObject; const Frame: TBytes);
  public
    constructor Create(AOwner: TComponent); override;
    function Connection(Drv: TISOTCPDriver): TPLCTagNumber;
    function SetDB(ADB: Word; AOffset: LongWord): TPLCTagNumber; overload;
    function SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCTagNumber;
    function SetArea(AArea: TS7Area): TPLCTagNumber;
    function SetTransportSize(ATS: TS7TransportSize): TPLCTagNumber;
    function SetTagType(AType: TS7TagType): TPLCTagNumber;
    procedure SetTagTypeProp(Value: TS7TagType);
    procedure SetScanIntervalProp(Value: Integer);
    procedure SetAutoReadProp(Value: Boolean);
    function SetKind(AKind: TPLCNumberKind): TPLCTagNumber; overload;
    function SetKind(AType: TS7TagType): TPLCTagNumber; overload;
    function SetmKind(AType: TS7TagType): TPLCTagNumber; overload;
    function SetScanInterval(Value: Integer): TPLCTagNumber;
    function SetAutoRead(Value: Boolean): TPLCTagNumber;
    function SetSwapBytes(Value: Boolean): TPLCTagNumber;
    function SetSwapWords(Value: Boolean): TPLCTagNumber;
    function SetSwapDWords(Value: Boolean): TPLCTagNumber;
    function SetMemReadFunction(Func: Integer): TPLCTagNumber;
    function SetMemWriteFunction(Func: Integer): TPLCTagNumber;
    function SetMemFileDB(DB: Word): TPLCTagNumber;
    function SetMemAddress(Offset: LongWord): TPLCTagNumber;
    function SetBitIndex(AIndex: Integer): TPLCTagNumber;
    procedure Read;
    property OnValueBool:  TOnValueBool  read FOnValueBool  write FOnValueBool;
    property OnValueByte:  TOnValueByte  read FOnValueByte  write FOnValueByte;
    property OnValueWord:  TOnValueWord  read FOnValueWord  write FOnValueWord;
    property OnValueDWord: TOnValueDWord read FOnValueDWord write FOnValueDWord;
    property OnValueInt16: TOnValueInt16 read FOnValueInt16 write FOnValueInt16;
    property OnValueDInt:  TOnValueDInt  read FOnValueDInt  write FOnValueDInt;
    property OnValueReal:  TOnValueReal  read FOnValueReal  write FOnValueReal;
    property OnValueChange: TOnValueChange read FOnValueChange write FOnValueChange;
    property ScanInterval: Integer read GetScanInterval write SetScanIntervalProp;
    property AutoRead: Boolean read FAutoRead write SetAutoReadProp;
    property SwapBytes: Boolean read FSwapBytes write FSwapBytes;
    property SwapWords: Boolean read FSwapWords write FSwapWords;
    property SwapDWords: Boolean read FSwapDWords write FSwapDWords;
    property MemReadFunction: Integer read FMemReadFunction write FMemReadFunction;
    property MemWriteFunction: Integer read FMemWriteFunction write FMemWriteFunction;
    property MemFile_DB: Word read FDB write FDB;
    property MemAddress: LongWord read FOffset write FOffset;
    property TagType: TS7TagType read FTagType write SetTagTypeProp;
  end;

  TTagBit = class(TPLCTagNumber)
  private
    FStructItem: TPLCStructItem;
    FStartBit: Integer;
    FEndBit: Integer;
    procedure StructItemChanged(Sender: TObject);
    function GetValue: Integer;
    function GetAsBoolean: Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function SetBit(AIndex: Integer): TTagBit;
    function SetStartBit(Value: Integer): TTagBit;
    function SetEndBit(Value: Integer): TTagBit;
    procedure SetStartBitProp(Value: Integer);
    procedure SetEndBitProp(Value: Integer);
    function ConnectToStructItem(AItem: TPLCStructItem): TTagBit;
    property StartBit: Integer read FStartBit write SetStartBitProp;
    property EndBit: Integer read FEndBit write SetEndBitProp;
    property Value: Integer read GetValue;
    property AsBoolean: Boolean read GetAsBoolean;
    destructor Destroy; override;
  end;

implementation

constructor TPLCTagNumber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueValid := False;
  FLastValue  := '';
end;

procedure TPLCTagNumber.InvalidateValue;
begin
  FValueValid := False;
  FLastValue  := '';
end;


function TPLCTagNumber.Connection(Drv: TISOTCPDriver): TPLCTagNumber;
begin
  FDriver := Drv;
  Result  := Self;
end;

function TPLCTagNumber.SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCTagNumber;
begin
  InvalidateValue;
  FDB     := ADB;
  FOffset := AOffset;
  FSize   := ASize;
  Result  := Self;
end;

function TPLCTagNumber.SetDB(ADB: Word; AOffset: LongWord): TPLCTagNumber;
begin
  InvalidateValue;
  FDB := ADB;
  FOffset := AOffset;
  if FSize = 0 then
    case FTS of
      tsBit:   FSize := 1;
      tsByte:  FSize := 1;
      tsWord:  FSize := 2;
      tsDWord: FSize := 4;
      tsReal:  FSize := 4;
    end;
  Result := Self;
end;

function TPLCTagNumber.SetArea(AArea: TS7Area): TPLCTagNumber;
begin
  InvalidateValue;
  FArea  := AArea;
  Result := Self;
end;

function TTagBit.SetBit(AIndex: Integer): TTagBit;
begin
  // SetTagType calls InvalidateValue
  SetTagType(pttBool);
  SetBitIndex(AIndex);
  FStartBit := AIndex;
  FEndBit := AIndex;
  Result := Self;
end;

function TTagBit.SetStartBit(Value: Integer): TTagBit;
begin
  FStartBit := Value;
  InvalidateValue;
  Result := Self;
end;

function TTagBit.SetEndBit(Value: Integer): TTagBit;
begin
  FEndBit := Value;
  InvalidateValue;
  Result := Self;
end;

function TTagBit.GetValue: Integer;
var
  Val, Mask: Integer;
begin
  if Assigned(FStructItem) then
  begin
    Val := FStructItem.GetValueAsInteger;
    
    // Ensure EndBit is valid
    if FEndBit < FStartBit then
      Mask := (1 shl FStartBit)
    else
      Mask := ((1 shl (FEndBit - FStartBit + 1)) - 1) shl FStartBit;
      
    Result := (Val and Mask) shr FStartBit;
  end
  else
    Result := 0;
end;

function TTagBit.GetAsBoolean: Boolean;
begin
  Result := GetValue <> 0;
end;

procedure TTagBit.SetStartBitProp(Value: Integer);
begin
  SetStartBit(Value);
end;

procedure TTagBit.SetEndBitProp(Value: Integer);
begin
  SetEndBit(Value);
end;

function TTagBit.ConnectToStructItem(AItem: TPLCStructItem): TTagBit;
begin
  if FStructItem = AItem then Exit(Self);

  if Assigned(FStructItem) then
  begin
    FStructItem.RemoveFreeNotification(Self);
    FStructItem.RemoveListener(StructItemChanged);
  end;

  FStructItem := AItem;
  
  if Assigned(FStructItem) then
  begin
    FStructItem.FreeNotification(Self);
    FStructItem.AddListener(StructItemChanged);
  end;
  Result := Self;
end;

procedure TTagBit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FStructItem) then
  begin
    FStructItem := nil;
  end;
end;

destructor TTagBit.Destroy;
begin
  if Assigned(FStructItem) then
  begin
    FStructItem.RemoveFreeNotification(Self);
    FStructItem.RemoveListener(StructItemChanged);
    FStructItem := nil;
  end;
  inherited Destroy;
end;

procedure TTagBit.StructItemChanged(Sender: TObject);
var
  Val, Mask, Res: Integer;
  ValStr: string;
begin
  if Assigned(FStructItem) then
  begin
    Val := FStructItem.GetValueAsInteger;
    
    if FEndBit < FStartBit then FEndBit := FStartBit;
    
    Mask := ((1 shl (FEndBit - FStartBit + 1)) - 1) shl FStartBit;
    Res := (Val and Mask) shr FStartBit;
    
    if FStartBit = FEndBit then
    begin
       if Res <> 0 then ValStr := 'True' else ValStr := 'False';
    end
    else
    begin
       ValStr := IntToStr(Res);
    end;
    
    if Assigned(FOnValueReal) then FOnValueReal(Self, Res);

    if (not FValueValid) or (ValStr <> FLastValue) then
    begin
      FValueValid := True;
      FLastValue := ValStr;
      
      if FStartBit = FEndBit then
      begin
         if Assigned(FOnValueBool) then FOnValueBool(Self, Res <> 0);
      end;
      
      if Assigned(FOnValueChange) then FOnValueChange(Self, ValStr);
    end;
  end;
end;

function TPLCTagNumber.SetTransportSize(ATS: TS7TransportSize): TPLCTagNumber;
begin
  InvalidateValue;
  FTS    := ATS;
  Result := Self;
end;

function TPLCTagNumber.SetTagType(AType: TS7TagType): TPLCTagNumber;
begin
  InvalidateValue;
  FTagType := AType;
  case FTagType of
    pttDefault: ;
    pttBool:     begin FTS := tsByte;   FSize := 1; end;
    // Use tsByte (0x02) for all byte-based types to ensure Length is in Bytes
    pttShortInt: begin FTS := tsByte;  FSize := 1; end;
    pttByte:     begin FTS := tsByte;  FSize := 1; end;
    pttSmallInt: begin FTS := tsByte;  FSize := 2; end;
    pttWord:     begin FTS := tsByte;  FSize := 2; end;
    pttLongInt:  begin FTS := tsByte;  FSize := 4; end;
    pttDWord:    begin FTS := tsByte;  FSize := 4; end;
    pttFloat:    begin FTS := tsByte;  FSize := 4; end;
    pttInt:      begin FTS := tsByte;  FSize := 2; end;
    pttDInt:     begin FTS := tsByte;  FSize := 4; end;
    pttInt64:    begin FTS := tsByte;  FSize := 8; end;
    pttQWord:    begin FTS := tsByte;  FSize := 8; end;
    pttDouble:   begin FTS := tsByte;  FSize := 8; end;
  end;
  Result := Self;
end;

procedure TPLCTagNumber.SetTagTypeProp(Value: TS7TagType);
begin
  SetTagType(Value);
end;

procedure TPLCTagNumber.SetScanIntervalProp(Value: Integer);
begin
  SetScanInterval(Value);
end;

procedure TPLCTagNumber.SetAutoReadProp(Value: Boolean);
begin
  SetAutoRead(Value);
end;

function TPLCTagNumber.SetKind(AKind: TPLCNumberKind): TPLCTagNumber;
begin
  case AKind of
    nkByte:  SetTagType(pttByte);
    nkWord:  SetTagType(pttWord);
    nkDWord: SetTagType(pttDWord);
    nkInt:   SetTagType(pttInt);
    nkDInt:  SetTagType(pttDInt);
    nkReal:  SetTagType(pttFloat);
  end;
  Result := Self;
end;

function TPLCTagNumber.SetKind(AType: TS7TagType): TPLCTagNumber;
begin
  SetTagType(AType);
  Result := Self;
end;

function TPLCTagNumber.SetmKind(AType: TS7TagType): TPLCTagNumber;
begin
  Result := SetKind(AType);
end;

// removed obsolete overload using TTagTypePS

function TPLCTagNumber.SetSwapBytes(Value: Boolean): TPLCTagNumber;
begin
  InvalidateValue;
  FSwapBytes := Value;
  Result := Self;
end;

function TPLCTagNumber.SetSwapWords(Value: Boolean): TPLCTagNumber;
begin
  InvalidateValue;
  FSwapWords := Value;
  Result := Self;
end;

function TPLCTagNumber.SetSwapDWords(Value: Boolean): TPLCTagNumber;
begin
  InvalidateValue;
  FSwapDWords := Value;
  Result := Self;
end;

function TPLCTagNumber.SetMemReadFunction(Func: Integer): TPLCTagNumber;
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

function TPLCTagNumber.SetMemWriteFunction(Func: Integer): TPLCTagNumber;
begin
  FMemWriteFunction := Func;
  Result := Self;
end;

function TPLCTagNumber.SetMemFileDB(DB: Word): TPLCTagNumber;
begin
  InvalidateValue;
  FDB := DB;
  Result := Self;
end;

function TPLCTagNumber.SetMemAddress(Offset: LongWord): TPLCTagNumber;
begin
  InvalidateValue;
  FOffset := Offset;
  Result := Self;
end;

function TPLCTagNumber.SetBitIndex(AIndex: Integer): TPLCTagNumber;
begin
  InvalidateValue;
  FBitIndex := AIndex;
  Result := Self;
end;

procedure TPLCTagNumber.Read;
var
  pdu: TBytes;
begin
  LogD('PLC', 'Tag.Read area=' + IntToStr(Ord(FArea)) + ' DB=' + IntToStr(FDB) + ' Off=' + IntToStr(FOffset) + ' Size=' + IntToStr(FSize));
  pdu := S7PDU.BuildReadVar(FArea, FDB, FOffset, FSize, FTS);
  FDriver.SendPDU(pdu, DriverOnFrame);
end;

procedure TPLCTagNumber.DriverOnFrame(Sender: TObject; const Frame: TBytes);
var
  b: Byte;
  w: Word;
  i16: SmallInt;
  dw: LongWord;
  di: LongInt;
  r: Single;
  bo: Boolean;
  data: TBytes;
  u64: QWord;
  i64: Int64;
  d8: Double;
  st: Integer;
  ValStr: string;
begin
  st := 0; if Length(Frame) > 13 then
    st := Frame[13];
  LogD('PLC', 'DriverOnFrame status=' + IntToStr(st) + ' len=' + IntToStr(Length(Frame)));
  
  // Analyze frame for errors. If not a valid Read Response (e.g. Setup Confirm), exit.
  if not S7Parser.IsReadResponseOK(Frame) then
    Exit;
  
  ValStr := '';
  
  case FTagType of
    pttBool:
      begin
        if S7Parser.ParseReadResponseBit(Frame, FBitIndex, bo) then
        begin
          if bo then
            ValStr := 'True'
          else
            ValStr := 'False';
          if (not FValueValid) or (ValStr <> FLastValue) then
          begin
            FValueValid := True;
            FLastValue  := ValStr;
            if Assigned(FOnValueBool) then
              FOnValueBool(Self, bo);
            if Assigned(FOnValueChange) then
              FOnValueChange(Self, ValStr);
          end;
        end;
      end;
    pttByte:
      begin
        if S7Parser.ParseReadResponseByte(Frame, b) then
        begin
          ValStr := IntToStr(b);
          if (not FValueValid) or (ValStr <> FLastValue) then
          begin
            FValueValid := True;
            FLastValue := ValStr;
            if Assigned(FOnValueByte) then
              FOnValueByte(Self, b);
            if Assigned(FOnValueChange) then
              FOnValueChange(Self, ValStr);
          end;
        end;
      end;
    pttShortInt:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        ValStr := IntToStr(ShortInt(data[0]));
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttWord:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        w := (data[0] shl 8) or data[1];
        ValStr := IntToStr(w);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueWord) then
            FOnValueWord(Self, w);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttSmallInt:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        i16 := SmallInt((data[0] shl 8) or data[1]);
        ValStr := IntToStr(i16);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueInt16) then
            FOnValueInt16(Self, i16);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttDWord:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        dw := (data[0] shl 24) or (data[1] shl 16) or (data[2] shl 8) or data[3];
        ValStr := IntToStr(Integer(dw));
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueDWord) then
            FOnValueDWord(Self, dw);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttInt:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        i16 := SmallInt((data[0] shl 8) or data[1]);
        ValStr := IntToStr(i16);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueInt16) then
            FOnValueInt16(Self, i16);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttDInt:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        di := LongInt((data[0] shl 24) or (data[1] shl 16) or (data[2] shl 8) or data[3]);
        ValStr := IntToStr(di);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueDInt) then
            FOnValueDInt(Self, di);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttFloat:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        dw := (data[0] shl 24) or (data[1] shl 16) or (data[2] shl 8) or data[3];
        r := PSingle(@dw)^;
        ValStr := FloatToStr(r);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueReal) then
            FOnValueReal(Self, r);
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttLongInt:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        di := LongInt((data[0] shl 24) or (data[1] shl 16) or (data[2] shl 8) or data[3]);
        ValStr := IntToStr(di);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttInt64:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        u64 := (QWord(data[0]) shl 56) or (QWord(data[1]) shl 48) or (QWord(data[2]) shl 40) or (QWord(data[3]) shl 32) or
               (QWord(data[4]) shl 24) or (QWord(data[5]) shl 16) or (QWord(data[6]) shl 8) or QWord(data[7]);
        i64 := Int64(u64);
        ValStr := IntToStr(i64);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttQWord:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        u64 := (QWord(data[0]) shl 56) or (QWord(data[1]) shl 48) or (QWord(data[2]) shl 40) or (QWord(data[3]) shl 32) or
               (QWord(data[4]) shl 24) or (QWord(data[5]) shl 16) or (QWord(data[6]) shl 8) or QWord(data[7]);
        ValStr := IntToStr(u64);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
    pttDouble:
      begin
        data := GetDataBytes(Frame);
        ApplySwaps(data);
        u64 := (QWord(data[0]) shl 56) or (QWord(data[1]) shl 48) or (QWord(data[2]) shl 40) or (QWord(data[3]) shl 32) or
               (QWord(data[4]) shl 24) or (QWord(data[5]) shl 16) or (QWord(data[6]) shl 8) or QWord(data[7]);
        d8 := PDouble(@u64)^;
        ValStr := FloatToStr(d8);
        if (not FValueValid) or (ValStr <> FLastValue) then
        begin
          FValueValid := True;
          FLastValue := ValStr;
          if Assigned(FOnValueChange) then
            FOnValueChange(Self, ValStr);
        end;
      end;
  end;
end;

procedure TPLCTagNumber.InternalTimer(Sender: TObject);
begin
  Read;
end;

procedure TPLCTagNumber.EnsureTimer;
begin
  if not FAutoRead then Exit; // Check AutoRead
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

procedure TPLCTagNumber.KillTimer;
begin
  if FTimer <> nil then
  begin
    FTimer.Enabled := False;
    FreeAndNil(FTimer);
  end;
end;

function TPLCTagNumber.SetScanInterval(Value: Integer): TPLCTagNumber;
begin
  FScanInterval := Value;
  if (FScanInterval <= 0) or (not FAutoRead) then
    KillTimer
  else
    EnsureTimer;
  Result := Self;
end;

function TPLCTagNumber.SetAutoRead(Value: Boolean): TPLCTagNumber;
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

function TPLCTagNumber.GetScanInterval: Integer;
begin
  Result := FScanInterval;
end;
function TPLCTagNumber.GetDataBytes(const Frame: TBytes): TBytes;
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

procedure TPLCTagNumber.ApplySwaps(var Data: TBytes);
var
  t: Byte;
  function SwapPair(a, b: Integer): Boolean;
  begin
    if (a >= 0) and (b < Length(Data)) then
    begin
      t := Data[a];
      Data[a] := Data[b];
      Data[b] := t;
      Exit(True);
    end;
    Result := False;
  end;
begin
  if Length(Data) = 2 then
  begin
    if FSwapBytes then
      SwapPair(0,1);
    Exit;
  end;
  if Length(Data) = 4 then
  begin
    if FSwapBytes then
    begin
      SwapPair(0,1);
      SwapPair(2,3);
    end;
    if FSwapWords then
    begin
      SwapPair(0,2);
      SwapPair(1,3);
    end;
    Exit;
  end;
  if Length(Data) = 8 then
  begin
    if FSwapBytes then
    begin
      SwapPair(0,1);
      SwapPair(2,3);
      SwapPair(4,5);
      SwapPair(6,7);
    end;
    if FSwapWords then
    begin
      SwapPair(0,2);
      SwapPair(1,3);
      SwapPair(4,6);
      SwapPair(5,7);
    end;
    if FSwapDWords then
    begin
      SwapPair(0,4);
      SwapPair(1,5);
      SwapPair(2,6);
      SwapPair(3,7);
    end;
    Exit;
  end;
end;

end.
