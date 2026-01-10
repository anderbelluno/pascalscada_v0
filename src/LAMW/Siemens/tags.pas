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

  TOnWriteComplete = procedure(Sender: TObject; Status: TProtocolIOResult) of object;

  // PT: Classe base para tags numéricos do PLC
  // EN: Base class for numeric PLC tags
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
    FOnWriteComplete: TOnWriteComplete;
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
    FLastSyncReadStatus: TProtocolIOResult;
    FLastSyncWriteStatus: TProtocolIOResult;
    procedure InternalTimer(Sender: TObject);
    procedure EnsureTimer;
    procedure KillTimer;
    procedure InvalidateValue;
    function GetScanInterval: Integer;
    function GetDataBytes(const Frame: TBytes): TBytes;
    procedure ApplySwaps(var Data: TBytes);
    procedure DriverOnFrame(Sender: TObject; const Frame: TBytes);
    procedure DriverOnWriteFrame(Sender: TObject; const Frame: TBytes);
  protected
    function GetLastSyncReadStatus: TProtocolIOResult; virtual;
    function GetLastSyncWriteStatus: TProtocolIOResult; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    
    // PT: Define o driver de comunicação
    // EN: Sets the communication driver
    function Connection(Drv: TISOTCPDriver): TPLCTagNumber;
    
    // PT: Configura endereço DB (Sobrecarga sem Tamanho)
    // EN: Configures DB address (Overload without Size)
    function SetDB(ADB: Word; AOffset: LongWord): TPLCTagNumber; overload;
    
    // PT: Configura endereço DB completo
    // EN: Configures complete DB address
    function SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCTagNumber;
    
    // PT: Configura área de memória
    // EN: Configures memory area
    function SetArea(AArea: TS7Area): TPLCTagNumber;
    
    // PT: Define tamanho de transporte S7
    // EN: Sets S7 transport size
    function SetTransportSize(ATS: TS7TransportSize): TPLCTagNumber;
    
    // PT: Define tipo do tag (Byte, Word, etc.)
    // EN: Sets tag type (Byte, Word, etc.)
    function SetTagType(AType: TS7TagType): TPLCTagNumber;
    procedure SetTagTypeProp(Value: TS7TagType);
    procedure SetScanIntervalProp(Value: Integer);
    procedure SetAutoReadProp(Value: Boolean);
    
    // PT: Define tipo numérico (Enum PascalSCADA)
    // EN: Sets numeric kind (PascalSCADA Enum)
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
    
    // PT: Executa leitura
    // EN: Performs read
    procedure Read;

    // PT: Escreve um valor inteiro
    // EN: Writes an integer value
    procedure Write(Value: Integer); overload;

    // PT: Escreve um valor Int64
    // EN: Writes an Int64 value
    procedure Write(Value: Int64); overload;

    // PT: Escreve um valor Ponto Flutuante
    // EN: Writes a Floating Point value
    procedure Write(Value: Double); overload;

    // PT: Escreve um valor Booleano
    // EN: Writes a Boolean value
    procedure Write(Value: Boolean); overload;
    
    property OnValueBool:  TOnValueBool  read FOnValueBool  write FOnValueBool;
    property OnValueByte:  TOnValueByte  read FOnValueByte  write FOnValueByte;
    property OnValueWord:  TOnValueWord  read FOnValueWord  write FOnValueWord;
    property OnValueDWord: TOnValueDWord read FOnValueDWord write FOnValueDWord;
    property OnValueInt16: TOnValueInt16 read FOnValueInt16 write FOnValueInt16;
    property OnValueDInt:  TOnValueDInt  read FOnValueDInt  write FOnValueDInt;
    property OnValueReal:  TOnValueReal  read FOnValueReal  write FOnValueReal;
    property OnValueChange: TOnValueChange read FOnValueChange write FOnValueChange;
    property OnWriteComplete: TOnWriteComplete read FOnWriteComplete write FOnWriteComplete;
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
    
    // PT: Status da última leitura síncrona
    // EN: Status of the last synchronous read
    property LastSyncReadStatus: TProtocolIOResult read GetLastSyncReadStatus;
    
    // PT: Status da última escrita síncrona
    // EN: Status of the last synchronous write
    property LastSyncWriteStatus: TProtocolIOResult read GetLastSyncWriteStatus;
  end;

  // PT: Classe especializada para manipulação de bits dentro de palavras (Word/Byte)
  // EN: Specialized class for bit manipulation within words (Word/Byte)
  TTagBit = class(TPLCTagNumber)
  private
    FStructItem: TPLCStructItem;
    FStartBit: Integer;
    FEndBit: Integer;
    procedure StructItemChanged(Sender: TObject);
    function GetValue: Integer;
    procedure SetValue(Value: Integer);
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetLastSyncReadStatus: TProtocolIOResult; override;
    function GetLastSyncWriteStatus: TProtocolIOResult; override;
  public
    // PT: Define um único bit para leitura/escrita
    // EN: Sets a single bit for read/write
    function SetBit(AIndex: Integer): TTagBit;
    
    // PT: Define o bit inicial da faixa
    // EN: Sets the start bit of the range
    function SetStartBit(Value: Integer): TTagBit;
    
    // PT: Define o bit final da faixa
    // EN: Sets the end bit of the range
    function SetEndBit(Value: Integer): TTagBit;
    
    procedure SetStartBitProp(Value: Integer);
    procedure SetEndBitProp(Value: Integer);
    
    // PT: Conecta este TagBit a um item de estrutura (TPLCStructItem)
    // EN: Connects this TagBit to a structure item (TPLCStructItem)
    function ConnectToStructItem(AItem: TPLCStructItem): TTagBit;
    
    // PT: Propriedade para definir o bit inicial (0-15)
    // EN: Property to set the start bit (0-15)
    property StartBit: Integer read FStartBit write SetStartBitProp;
    
    // PT: Propriedade para definir o bit final
    // EN: Property to set the end bit
    property EndBit: Integer read FEndBit write SetEndBitProp;
    
    // PT: Valor inteiro dos bits extraídos. Ao escrever, modifica os bits no PLC.
    // EN: Integer value of extracted bits. Writing modifies bits in the PLC.
    property Value: Integer read GetValue write SetValue;
    
    // PT: Valor booleano (True se Value > 0). Ao escrever, modifica o bit no PLC.
    // EN: Boolean value (True if Value > 0). Writing modifies the bit in the PLC.
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    destructor Destroy; override;
  end;

implementation

constructor TPLCTagNumber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValueValid := False;
  FLastValue  := '';
  FLastSyncReadStatus := ioNone;
  FLastSyncWriteStatus := ioNone;
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

procedure TTagBit.SetValue(Value: Integer);
var
  CurrentVal, Mask, NewVal: Integer;
begin
  if not Assigned(FStructItem) then Exit;

  CurrentVal := FStructItem.GetValueAsInteger;

  // Calculate Mask for the target bits
  if FEndBit < FStartBit then
    Mask := (1 shl FStartBit)
  else
    Mask := ((1 shl (FEndBit - FStartBit + 1)) - 1) shl FStartBit;

  // Clear target bits in current value
  CurrentVal := CurrentVal and (not Mask);

  // Shift new value to correct position and mask it to ensure it fits
  NewVal := (Value shl FStartBit) and Mask;

  // Combine
  FStructItem.SetValueAsInteger(CurrentVal or NewVal);
end;

function TTagBit.GetAsBoolean: Boolean;
begin
  Result := GetValue <> 0;
end;

procedure TTagBit.SetAsBoolean(Value: Boolean);
begin
  if Value then
    SetValue(1)
  else
    SetValue(0);
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

procedure TPLCTagNumber.Write(Value: Integer);
var
  Data: TBytes;
begin
  if not Assigned(FDriver) then
  begin
    FLastSyncWriteStatus := ioNullDriver;
    Exit;
  end;
  
  SetLength(Data, FSize);
  
  // Construct Big Endian Data
  case FTagType of
    pttByte, pttShortInt:
      Data[0] := Byte(Value);
    pttWord, pttInt, pttSmallInt:
      begin
        Data[0] := (Value shr 8) and $FF;
        Data[1] := Value and $FF;
      end;
    pttDWord, pttDInt, pttLongInt:
      begin
        Data[0] := (Value shr 24) and $FF;
        Data[1] := (Value shr 16) and $FF;
        Data[2] := (Value shr 8) and $FF;
        Data[3] := Value and $FF;
      end;
    else
      Exit; 
  end;
  
  ApplySwaps(Data);
  FLastSyncWriteStatus := ioBusy;
  FDriver.SendPDU(S7PDU.BuildWriteVar(FArea, FDB, FOffset, FSize, FTS, Data), DriverOnWriteFrame);
end;

procedure TPLCTagNumber.Write(Value: Int64);
var
  Data: TBytes;
begin
  if not Assigned(FDriver) then 
  begin
    FLastSyncWriteStatus := ioNullDriver;
    Exit;
  end;
  if FSize <> 8 then Exit;
  
  SetLength(Data, 8);
  Data[0] := (Value shr 56) and $FF;
  Data[1] := (Value shr 48) and $FF;
  Data[2] := (Value shr 40) and $FF;
  Data[3] := (Value shr 32) and $FF;
  Data[4] := (Value shr 24) and $FF;
  Data[5] := (Value shr 16) and $FF;
  Data[6] := (Value shr 8) and $FF;
  Data[7] := Value and $FF;
  
  ApplySwaps(Data);
  FLastSyncWriteStatus := ioBusy;
  FDriver.SendPDU(S7PDU.BuildWriteVar(FArea, FDB, FOffset, FSize, FTS, Data), DriverOnWriteFrame);
end;

procedure TPLCTagNumber.Write(Value: Double);
var
  Data: TBytes;
  s: Single;
  d: Double;
  u32: LongWord;
  u64: QWord;
begin
  if not Assigned(FDriver) then 
  begin
    FLastSyncWriteStatus := ioNullDriver;
    Exit;
  end;
  SetLength(Data, FSize);
  
  if FTagType = pttFloat then
  begin
    s := Value;
    u32 := PLongWord(@s)^;
    // Swap Little Endian (Native) to Big Endian
    Data[0] := (u32 shr 24) and $FF;
    Data[1] := (u32 shr 16) and $FF;
    Data[2] := (u32 shr 8) and $FF;
    Data[3] := u32 and $FF;
  end
  else if (FTagType = pttDouble) then
  begin
    d := Value;
    u64 := PQWord(@d)^;
    // Swap Little Endian (Native) to Big Endian
    Data[0] := (u64 shr 56) and $FF;
    Data[1] := (u64 shr 48) and $FF;
    Data[2] := (u64 shr 40) and $FF;
    Data[3] := (u64 shr 32) and $FF;
    Data[4] := (u64 shr 24) and $FF;
    Data[5] := (u64 shr 16) and $FF;
    Data[6] := (u64 shr 8) and $FF;
    Data[7] := u64 and $FF;
  end
  else Exit;
  
  ApplySwaps(Data);
  FLastSyncWriteStatus := ioBusy;
  FDriver.SendPDU(S7PDU.BuildWriteVar(FArea, FDB, FOffset, FSize, FTS, Data), DriverOnWriteFrame);
end;

procedure TPLCTagNumber.Write(Value: Boolean);
var
  Data: TBytes;
begin
  if not Assigned(FDriver) then 
  begin
    FLastSyncWriteStatus := ioNullDriver;
    Exit;
  end;
  if FTagType <> pttBool then Exit;
  
  SetLength(Data, 1);
  if Value then Data[0] := 1 else Data[0] := 0;
  
  FLastSyncWriteStatus := ioBusy;
  FDriver.SendPDU(S7PDU.BuildWriteVar(FArea, FDB, FOffset, FSize, FTS, Data), DriverOnWriteFrame);
end;

procedure TPLCTagNumber.DriverOnWriteFrame(Sender: TObject; const Frame: TBytes);
begin
  if S7Parser.IsWriteResponseOK(Frame) then
    FLastSyncWriteStatus := ioOk
  else
    FLastSyncWriteStatus := ioCommError;

  if Assigned(FOnWriteComplete) then
    FOnWriteComplete(Self, FLastSyncWriteStatus);
end;

procedure TPLCTagNumber.Read;
var
  pdu: TBytes;
begin
  if Assigned(FDriver) then
  begin
    FLastSyncReadStatus := ioBusy;
    //LogD('PLC', 'Tag.Read area=' + IntToStr(Ord(FArea)) + ' DB=' + IntToStr(FDB) + ' Off=' + IntToStr(FOffset) + ' Size=' + IntToStr(FSize));
    pdu := S7PDU.BuildReadVar(FArea, FDB, FOffset, FSize, FTS);
    FDriver.SendPDU(pdu, DriverOnFrame);
  end else
    FLastSyncReadStatus := ioNullDriver;
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
  //LogD('PLC', 'DriverOnFrame status=' + IntToStr(st) + ' len=' + IntToStr(Length(Frame)));
  
  // Analyze frame for errors. If not a valid Read Response (e.g. Setup Confirm), exit.
  if not S7Parser.IsReadResponseOK(Frame) then
  begin
    FLastSyncReadStatus := ioCommError;
    Exit;
  end;

  FLastSyncReadStatus := ioOk;
  
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

function TPLCTagNumber.GetLastSyncReadStatus: TProtocolIOResult;
begin
  Result := FLastSyncReadStatus;
end;

function TPLCTagNumber.GetLastSyncWriteStatus: TProtocolIOResult;
begin
  Result := FLastSyncWriteStatus;
end;

function TTagBit.GetLastSyncReadStatus: TProtocolIOResult;
begin
  if Assigned(FStructItem) then
    Result := FStructItem.LastSyncReadStatus
  else
    Result := inherited GetLastSyncReadStatus;
end;

function TTagBit.GetLastSyncWriteStatus: TProtocolIOResult;
begin
  if Assigned(FStructItem) then
    Result := FStructItem.LastSyncWriteStatus
  else
    Result := inherited GetLastSyncWriteStatus;
end;

end.
