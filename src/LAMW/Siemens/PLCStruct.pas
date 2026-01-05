unit PLCStruct;

{$mode delphi}

interface

uses
  Classes, SysUtils, ISOTCPDriver_Siemens, S7PDU, S7Parser, S7Types_Siemens, AndroidLog, Laz_And_Controls;

type
  // PT: Classe responsável por ler/escrever um bloco contíguo de memória do PLC
  // EN: Class responsible for reading/writing a contiguous memory block from the PLC
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
    FLastSyncReadStatus: TProtocolIOResult;
    FLastSyncWriteStatus: TProtocolIOResult;
    procedure InternalTimer(Sender: TObject);
    procedure EnsureTimer;
    procedure KillTimer;
    procedure DriverOnReadFrame(Sender: TObject; const Frame: TBytes);
    procedure DriverOnWriteFrame(Sender: TObject; const Frame: TBytes);
    function GetDataBytes(const Frame: TBytes): TBytes;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    // PT: Define o driver de comunicação a ser usado
    // EN: Sets the communication driver to be used
    function Connection(Drv: TISOTCPDriver): TPLCStruct;
    
    // PT: Configura o endereço do DB (Número, Offset e Tamanho)
    // EN: Configures the DB address (Number, Offset, and Size)
    function SetDB(ADB: Word; AOffset: LongWord; ASize: Word): TPLCStruct;
    
    // PT: Configura a área de memória (ex: DB, Marker, Input, Output)
    // EN: Sets the memory area (e.g., DB, Marker, Input, Output)
    function SetArea(AArea: TS7Area): TPLCStruct;
    
    // PT: Define o intervalo de varredura (leitura automática) em milissegundos
    // EN: Sets the scan interval (auto-read) in milliseconds
    function SetScanInterval(Value: Integer): TPLCStruct;
    
    // PT: Habilita ou desabilita a leitura automática
    // EN: Enables or disables automatic reading
    function SetAutoRead(Value: Boolean): TPLCStruct;
    
    // PT: Executa uma leitura imediata do bloco
    // EN: Performs an immediate read of the block
    procedure Read;
    
    // PT: Escreve o conteúdo atual do buffer (FData) no PLC
    // EN: Writes the current buffer content (FData) to the PLC
    procedure Write;
    
    // PT: Adiciona um ouvinte para notificação de atualização de dados
    // EN: Adds a listener for data update notifications
    procedure AddListener(Listener: TNotifyEvent);
    
    // PT: Remove um ouvinte
    // EN: Removes a listener
    procedure RemoveListener(Listener: TNotifyEvent);
    
    // PT: Lê um byte do buffer local
    // EN: Reads a byte from the local buffer
    function GetByte(Offset: Integer): Byte;
    
    // PT: Escreve um byte no buffer local (não envia ao PLC automaticamente)
    // EN: Writes a byte to the local buffer (does not send to PLC automatically)
    procedure SetByte(Offset: Integer; Value: Byte);
    
    // PT: Lê uma palavra (Word - 2 bytes) do buffer local (Big Endian)
    // EN: Reads a word (2 bytes) from the local buffer (Big Endian)
    function GetWord(Offset: Integer): Word;
    
    // PT: Escreve uma palavra no buffer local
    // EN: Writes a word to the local buffer
    procedure SetWord(Offset: Integer; Value: Word);
    
    // PT: Lê uma palavra dupla (DWord - 4 bytes) do buffer local
    // EN: Reads a double word (4 bytes) from the local buffer
    function GetDWord(Offset: Integer): LongWord;
    
    // PT: Escreve uma palavra dupla no buffer local
    // EN: Writes a double word to the local buffer
    procedure SetDWord(Offset: Integer; Value: LongWord);
    
    // PT: Acesso direto aos dados brutos
    // EN: Direct access to raw data
    property Data: TBytes read FData;

    // PT: Status da última leitura síncrona
    // EN: Status of the last synchronous read
    property LastSyncReadStatus: TProtocolIOResult read FLastSyncReadStatus;

    // PT: Status da última escrita síncrona
    // EN: Status of the last synchronous write
    property LastSyncWriteStatus: TProtocolIOResult read FLastSyncWriteStatus;
  end;

implementation


constructor TPLCStruct.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
  SetLength(FData, 0);
  FLastSyncReadStatus := ioNone;
  FLastSyncWriteStatus := ioNone;
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
    FLastSyncReadStatus := ioBusy;
    pdu := S7PDU.BuildReadVar(FArea, FDB, FOffset, FSize, tsByte);
    FDriver.SendPDU(pdu, DriverOnReadFrame);
  end else
    FLastSyncReadStatus := ioNullDriver;
end;

procedure TPLCStruct.Write;
var
  pdu: TBytes;
begin
  if Assigned(FDriver) and (Length(FData) = FSize) then
  begin
    FLastSyncWriteStatus := ioBusy;
    pdu := S7PDU.BuildWriteVar(FArea, FDB, FOffset, FSize, tsByte, FData);
    FDriver.SendPDU(pdu, DriverOnWriteFrame);
  end else begin
    if not Assigned(FDriver) then FLastSyncWriteStatus := ioNullDriver
    else FLastSyncWriteStatus := ioDriverError;
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

procedure TPLCStruct.DriverOnReadFrame(Sender: TObject; const Frame: TBytes);
var
  i: Integer;
  Method: TMethod;
  NotifyEvent: TNotifyEvent;
begin
  if S7Parser.IsReadResponseOK(Frame) then
  begin
    FData := GetDataBytes(Frame);
    FLastSyncReadStatus := ioOk;
    
    // Notify listeners
    for i := 0 to FListeners.Count - 1 do
    begin
      Method := TMethod(FListeners[i]^);
      NotifyEvent := TNotifyEvent(Method);
      if Assigned(NotifyEvent) then
        NotifyEvent(Self);
    end;
  end
  else
    FLastSyncReadStatus := ioCommError;
end;

procedure TPLCStruct.DriverOnWriteFrame(Sender: TObject; const Frame: TBytes);
begin
  if S7Parser.IsWriteResponseOK(Frame) then
    FLastSyncWriteStatus := ioOk
  else
    FLastSyncWriteStatus := ioCommError;
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

procedure TPLCStruct.SetByte(Offset: Integer; Value: Byte);
begin
  if (Offset >= 0) and (Offset < Length(FData)) then
    FData[Offset] := Value;
end;

procedure TPLCStruct.SetWord(Offset: Integer; Value: Word);
begin
  if (Offset >= 0) and (Offset + 1 < Length(FData)) then
  begin
    FData[Offset] := (Value shr 8) and $FF;
    FData[Offset + 1] := Value and $FF;
  end;
end;

procedure TPLCStruct.SetDWord(Offset: Integer; Value: LongWord);
begin
  if (Offset >= 0) and (Offset + 3 < Length(FData)) then
  begin
    FData[Offset] := (Value shr 24) and $FF;
    FData[Offset + 1] := (Value shr 16) and $FF;
    FData[Offset + 2] := (Value shr 8) and $FF;
    FData[Offset + 3] := Value and $FF;
  end;
end;

end.
