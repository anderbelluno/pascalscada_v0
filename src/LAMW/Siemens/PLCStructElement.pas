unit PLCStructElement;

{$mode delphi}

interface

uses
  Classes, SysUtils, PLCStruct, S7Types_Siemens, AndroidLog;

type
  // PT: Representa um item (variável) dentro de uma estrutura (TPLCStruct)
  // EN: Represents an item (variable) within a structure (TPLCStruct)
  TPLCStructItem = class(TComponent)
  private
    FStruct: TPLCStruct;
    FOffset: Integer;
    FTagType: TS7TagType;
    FBitIndex: Integer;
    FLastValue: Integer;
    FValueValid: Boolean;
    FListeners: TList;
    procedure StructChanged(Sender: TObject);
  protected
    FOnValueChange: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    // PT: Associa este item a uma estrutura TPLCStruct pai
    // EN: Associates this item with a parent TPLCStruct structure
    function SetStruct(AStruct: TPLCStruct): TPLCStructItem;
    
    // PT: Define o offset (deslocamento) em bytes dentro da estrutura
    // EN: Sets the offset (displacement) in bytes within the structure
    function SetOffset(AOffset: Integer): TPLCStructItem;
    
    // PT: Define o tipo de dado (Byte, Word, DWord)
    // EN: Sets the data type (Byte, Word, DWord)
    function SetTagType(AType: TS7TagType): TPLCStructItem;
    
    // PT: Define o índice do bit (0-7) para variáveis booleanas
    // EN: Sets the bit index (0-7) for boolean variables
    function SetBitIndex(Index: Integer): TPLCStructItem;
    
    // PT: Retorna o valor interpretado como Booleano
    // EN: Returns the value interpreted as Boolean
    function GetValueAsBoolean: Boolean;
    
    // PT: Retorna o valor interpretado como Inteiro
    // EN: Returns the value interpreted as Integer
    function GetValueAsInteger: Integer;
    
    // PT: Define um valor booleano, atualiza o buffer da estrutura e escreve no PLC
    // EN: Sets a boolean value, updates the structure buffer, and writes to the PLC
    procedure SetValueAsBoolean(Value: Boolean);
    
    // PT: Define um valor inteiro, atualiza o buffer da estrutura e escreve no PLC
    // EN: Sets an integer value, updates the structure buffer, and writes to the PLC
    procedure SetValueAsInteger(Value: Integer);
    
    // PT: Define o offset (alias para SetOffset)
    // EN: Sets the offset (alias for SetOffset)
    function SetIndex(Value: Integer): TPLCStructItem;
    
    // PT: Define a estrutura pai (alias para SetStruct)
    // EN: Sets the parent structure (alias for SetStruct)
    function SetPLCBlock(Value: TPLCStruct): TPLCStructItem;
    
    // PT: Adiciona ouvinte para mudanças de valor
    // EN: Adds a listener for value changes
    procedure AddListener(Listener: TNotifyEvent);
    
    // PT: Remove ouvinte
    // EN: Removes a listener
    procedure RemoveListener(Listener: TNotifyEvent);
    
    // PT: Retorna o status da última leitura síncrona da estrutura
    // EN: Returns the status of the last synchronous read of the structure
    function GetLastSyncReadStatus: TProtocolIOResult;

    // PT: Retorna o status da última escrita síncrona da estrutura
    // EN: Returns the status of the last synchronous write of the structure
    function GetLastSyncWriteStatus: TProtocolIOResult;

    property Index: Integer read FOffset write FOffset;
    property PLCBlock: TPLCStruct read FStruct write FStruct;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
    property TagType: TS7TagType read FTagType;
    
    // PT: Status da última leitura síncrona
    // EN: Status of the last synchronous read
    property LastSyncReadStatus: TProtocolIOResult read GetLastSyncReadStatus;
    
    // PT: Status da última escrita síncrona
    // EN: Status of the last synchronous write
    property LastSyncWriteStatus: TProtocolIOResult read GetLastSyncWriteStatus;
  end;

implementation

constructor TPLCStructItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
  FBitIndex := 0;
  FValueValid := False;
end;

destructor TPLCStructItem.Destroy;
var
  i: Integer;
  P: ^TMethod;
begin
  if Assigned(FStruct) then
    FStruct.RemoveListener(StructChanged);
  if Assigned(FListeners) then
  begin
    for i := 0 to FListeners.Count - 1 do
    begin
      P := FListeners[i];
      Dispose(P);
    end;
    FListeners.Free;
  end;
  inherited Destroy;
end;

procedure TPLCStructItem.AddListener(Listener: TNotifyEvent);
var
  P: ^TMethod;
begin
  if not Assigned(FListeners) then Exit;
  New(P);
  P^ := TMethod(Listener);
  FListeners.Add(P);
end;

procedure TPLCStructItem.RemoveListener(Listener: TNotifyEvent);
var
  i: Integer;
  P: ^TMethod;
  Target: TMethod;
begin
  if not Assigned(FListeners) then Exit;
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

function TPLCStructItem.SetIndex(Value: Integer): TPLCStructItem;
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    FValueValid := False;
  end;
  Result := Self;
end;

function TPLCStructItem.SetPLCBlock(Value: TPLCStruct): TPLCStructItem;
begin
  SetStruct(Value);
  Result := Self;
end;

function TPLCStructItem.SetStruct(AStruct: TPLCStruct): TPLCStructItem;
begin
  if Assigned(FStruct) then
    FStruct.RemoveListener(StructChanged);
  FStruct := AStruct;
  FValueValid := False;
  if Assigned(FStruct) then
    FStruct.AddListener(StructChanged);
  Result := Self;
end;

function TPLCStructItem.SetOffset(AOffset: Integer): TPLCStructItem;
begin
  FOffset := AOffset;
  Result := Self;
end;

function TPLCStructItem.SetTagType(AType: TS7TagType): TPLCStructItem;
begin
  FTagType := AType;
  Result := Self;
end;

function TPLCStructItem.SetBitIndex(Index: Integer): TPLCStructItem;
begin
  FBitIndex := Index;
  Result := Self;
end;

procedure TPLCStructItem.StructChanged(Sender: TObject);
var
  CurrentValue: Integer;
  i: Integer;
  P: ^TMethod;
  NotifyEvent: TNotifyEvent;
begin
  CurrentValue := GetValueAsInteger;
  
  //LogD('PLCStructItem', 'StructChanged: Offset=' + IntToStr(FOffset) + ' Val=' + IntToStr(CurrentValue) + ' Last=' + IntToStr(FLastValue) + ' Valid=' + BoolToStr(FValueValid, True));

  if (not FValueValid) or (CurrentValue <> FLastValue) then
  begin
    FValueValid := True;
    FLastValue := CurrentValue;
    if Assigned(FOnValueChange) then
    begin
      //LogD('PLCStructItem', 'Triggering OnValueChange');
      FOnValueChange(Self);
    end;
    
    if Assigned(FListeners) then
    begin
      for i := 0 to FListeners.Count - 1 do
      begin
        P := FListeners[i];
        NotifyEvent := TNotifyEvent(P^);
        if Assigned(NotifyEvent) then
          NotifyEvent(Self);
      end;
    end;
  end;
end;

function TPLCStructItem.GetValueAsBoolean: Boolean;
var
  b: Byte;
begin
  if not Assigned(FStruct) then Exit(False);
  b := FStruct.GetByte(FOffset);
  Result := (b and (1 shl FBitIndex)) <> 0;
end;

function TPLCStructItem.GetValueAsInteger: Integer;
begin
  if not Assigned(FStruct) then Exit(0);
  case FTagType of
    pttShortInt: Result := ShortInt(FStruct.GetByte(FOffset));
    pttByte:     Result := FStruct.GetByte(FOffset);
    pttSmallInt, pttInt: Result := SmallInt(FStruct.GetWord(FOffset));
    pttWord:     Result := FStruct.GetWord(FOffset);
    pttLongInt, pttDInt: Result := LongInt(FStruct.GetDWord(FOffset));
    pttDWord:    Result := LongInt(FStruct.GetDWord(FOffset)); // Cast to Integer (signed 32-bit in Pascal usually)
    else Result := 0;
  end;
end;

procedure TPLCStructItem.SetValueAsBoolean(Value: Boolean);
var
  b, Mask: Byte;
begin
  if not Assigned(FStruct) then Exit;
  // Ensure buffer exists
  if (FStruct.Data = nil) or (Length(FStruct.Data) <= FOffset) then Exit;

  b := FStruct.GetByte(FOffset);
  Mask := 1 shl FBitIndex;
  
  if Value then
    b := b or Mask
  else
    b := b and (not Mask);
    
  FStruct.SetByte(FOffset, b);
  FStruct.Write; 
end;

procedure TPLCStructItem.SetValueAsInteger(Value: Integer);
begin
  if not Assigned(FStruct) then Exit;
  
  case FTagType of
    pttShortInt, pttByte: FStruct.SetByte(FOffset, Byte(Value));
    pttSmallInt, pttInt, pttWord: FStruct.SetWord(FOffset, Word(Value));
    pttLongInt, pttDInt, pttDWord: FStruct.SetDWord(FOffset, LongWord(Value));
  end;
  
  FStruct.Write;
end;

function TPLCStructItem.GetLastSyncReadStatus: TProtocolIOResult;
begin
  if Assigned(FStruct) then
    Result := FStruct.LastSyncReadStatus
  else
    Result := ioNullTagBlock;
end;

function TPLCStructItem.GetLastSyncWriteStatus: TProtocolIOResult;
begin
  if Assigned(FStruct) then
    Result := FStruct.LastSyncWriteStatus
  else
    Result := ioNullTagBlock;
end;

end.
