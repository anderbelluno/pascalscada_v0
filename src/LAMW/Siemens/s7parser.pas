unit S7Parser;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function ParseReadResponseInt16(const Resp: TBytes; out Value: SmallInt): Boolean;
function ParseReadResponseByte(const Resp: TBytes; out Value: Byte): Boolean;
function ParseReadResponseWord(const Resp: TBytes; out Value: Word): Boolean;
function ParseReadResponseDWord(const Resp: TBytes; out Value: LongWord): Boolean;
function ParseReadResponseDInt(const Resp: TBytes; out Value: LongInt): Boolean;
function ParseReadResponseReal(const Resp: TBytes; out Value: Single): Boolean;
function ParseReadResponseBit(const Resp: TBytes; BitIndex: Integer; out Value: Boolean): Boolean;
function IsReadResponseOK(const Resp: TBytes): Boolean;
function IsWriteResponseOK(const Resp: TBytes): Boolean;

implementation

uses AndroidLog;

function HexStr(const Data: TBytes; MaxBytes: Integer = 64): string;
var
  i, n: Integer;
begin
  Result := '';
  n := Length(Data);
  if n > MaxBytes then
    n := MaxBytes;
  for i := 0 to n - 1 do
    Result := Result + IntToHex(Data[i], 2) + ' ';
end;

function IsReadResponseOK(const Resp: TBytes): Boolean;
var
  ErrClass, ErrCode: Byte;
  ParamLen: Word;
  ReturnCodeOffset: Integer;
begin
  Result := False;
  if Length(Resp) < 12 then
  begin
    //AndroidLog.LogD('PLC', 'Resposta muito curta (Header incompleto): ' + IntToStr(Length(Resp)) + ' bytes.');
    Exit;
  end;

  // Log Header for debugging
  //AndroidLog.LogD('PLC', 'S7 Header: ' + HexStr(Resp, 12));

  if Resp[8] <> $03 then
  begin
    //AndroidLog.LogD('PLC', 'ROSCTR inválido: ' + IntToHex(Resp[8], 2) + ' (esperado 03 Ack_Data)');
    Exit;
  end;

  // Check for S7 Error (Bytes 17 and 18 for Ack_Data)
  if Length(Resp) >= 19 then
  begin
    if (Resp[17] <> 0) or (Resp[18] <> 0) then
    begin
      //AndroidLog.LogD('PLC', 'Erro retornado pelo PLC. Error Class: ' + IntToHex(Resp[17], 2) + ' Error Code: ' + IntToHex(Resp[18], 2));
      if Resp[18] = $85 then
        //AndroidLog.LogD('PLC', 'Erro 0x85: PDU Inválido ou não suportado (Verifique tamanho do PDU).');
      if Resp[18] = $81 then
        //AndroidLog.LogD('PLC', 'Erro 0x81: Contexto/Access Error.');
      Exit;
    end;
  end;

  // Check Parameter Length
  if Length(Resp) >= 15 then
     ParamLen := (Resp[13] shl 8) or Resp[14]
  else
     ParamLen := 0;

  // Filter out Setup Communication Confirm (Function Code F0)
  // Parameter starts at 19.
  if (Length(Resp) > 19) then
  begin
     if Resp[19] = $F0 then
     begin
        //AndroidLog.LogD('PLC', 'Ignorando Setup Communication Confirm (Function F0).');
        Result := False;
        Exit;
     end;

     // Strict check: Only accept Read Var (04) for Read Tag
     if Resp[19] <> $04 then
     begin
        //AndroidLog.LogD('PLC', 'Pacote ignorado: Função ' + IntToHex(Resp[19], 2) + ' (esperado 04 Read Var)');
        Result := False;
        Exit;
     end;
  end;

  if Length(Resp) < 25 then
  begin
    // Could be OK if just header, but usually we expect data.
    if Length(Resp) >= 17 then
    begin
       if ((Resp[15] = 0) and (Resp[16] = 0)) then
       begin
          //AndroidLog.LogD('PLC', 'Resposta OK mas sem dados (Data Length = 0).');
       end;
    end;
    AndroidLog.LogD('PLC', 'Resposta curta: ' + IntToStr(Length(Resp)) + ' bytes.');
  end;

  // Log Param/Data Length
  if Length(Resp) >= 15 then
  begin
    // AndroidLog.LogD('PLC', 'Param Length: ' + IntToStr((Resp[13] shl 8) or Resp[14]));
    // AndroidLog.LogD('PLC', 'Data Length: ' + IntToStr((Resp[15] shl 8) or Resp[16]));
  end;

  // Check Return Code (First byte of Data)
  // Data starts at 19 + ParamLen.
  ReturnCodeOffset := 19 + ParamLen;
  if Length(Resp) > ReturnCodeOffset then
  begin
     if Resp[ReturnCodeOffset] <> $FF then
     begin
       //AndroidLog.LogD('PLC', 'Byte ' + IntToStr(ReturnCodeOffset) + ' (Return Code) = ' + IntToHex(Resp[ReturnCodeOffset], 2) + ' (esperado FF para sucesso)');
       {case Resp[ReturnCodeOffset] of
         $03: //AndroidLog.LogD('PLC', 'Return Code 03: Access Denied');
         $05: //AndroidLog.LogD('PLC', 'Return Code 05: Address Out of Range');
         $0A: //AndroidLog.LogD('PLC', 'Return Code 0A: Item not available');
       end; }
       // Do not exit, allow partial parsing if needed (sometimes multiple items)
     end;
  end;

  Result := True;
  //AndroidLog.LogD('PLC', 'Resposta de leitura analisada.');
end;

function GetDataOffset(const Resp: TBytes): Integer;
begin
  Result := -1;
  if not IsReadResponseOK(Resp) then
    Exit;
  // TPKT(4) + COTP(3) + S7Header(12) + Param(2) + ItemHeader(4) = 25
  // ItemHeader = ReturnCode(1) + TranspSize(1) + Length(2)
  Result := 25;
  //AndroidLog.LogD('PLC', 'Data offset calculado: ' + IntToStr(Result));
end;

function ParseReadResponseInt16(const Resp: TBytes; out Value: SmallInt): Boolean;
var
  Offset: Integer;
  w: Word;
begin
  Result := False;
  //AndroidLog.LogD('PLC', 'Tentando parse Int16 da resposta: ' + HexStr(Resp));
  Offset := GetDataOffset(Resp);
  if Offset < 0 then
    Exit;
  if Offset + 1 >= Length(Resp) then
  begin
    //AndroidLog.LogD('PLC', 'Dados insuficientes para Int16');
    Exit;
  end;
  w := (Resp[Offset] shl 8) or Resp[Offset + 1];
  Value := SmallInt(w);
  //AndroidLog.LogD('PLC', 'Valor Int16 lido com sucesso: ' + IntToStr(Value));
  Result := True;
end;

function ParseReadResponseByte(const Resp: TBytes; out Value: Byte): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  Offset := GetDataOffset(Resp);
  if Offset < 0 then
    Exit;
  Value := Resp[Offset];
  Result := True;
end;

function ParseReadResponseWord(const Resp: TBytes; out Value: Word): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  Offset := GetDataOffset(Resp);
  if (Offset < 0) or (Offset + 1 >= Length(Resp)) then
    Exit;
  Value := (Resp[Offset] shl 8) or Resp[Offset + 1];
  Result := True;
end;

function ParseReadResponseDWord(const Resp: TBytes; out Value: LongWord): Boolean;
var
  Offset: Integer;
begin
  Result := False;
  Offset := GetDataOffset(Resp);
  if (Offset < 0) or (Offset + 3 >= Length(Resp)) then
    Exit;
  Value := (Resp[Offset] shl 24) or (Resp[Offset + 1] shl 16) or
           (Resp[Offset + 2] shl 8) or Resp[Offset + 3];
  Result := True;
end;

function ParseReadResponseDInt(const Resp: TBytes; out Value: LongInt): Boolean;
var
  u: LongWord;
begin
  Result := ParseReadResponseDWord(Resp, u);
  if Result then
    Value := LongInt(u);
end;

function ParseReadResponseReal(const Resp: TBytes; out Value: Single): Boolean;
var
  Offset: Integer;
  u: LongWord;
  p: ^Single;
begin
  Result := False;
  Offset := GetDataOffset(Resp);
  if (Offset < 0) or (Offset + 3 >= Length(Resp)) then
    Exit;
  u := (Resp[Offset] shl 24) or (Resp[Offset + 1] shl 16) or
       (Resp[Offset + 2] shl 8) or Resp[Offset + 3];
  p := @u;
  Value := p^;
  Result := True;
end;

function ParseReadResponseBit(const Resp: TBytes; BitIndex: Integer; out Value: Boolean): Boolean;
var
  b: Byte;
begin
  Result := False;
  if not ParseReadResponseByte(Resp, b) then
    Exit;
  if (BitIndex < 0) or (BitIndex > 7) then
    Exit;
  Value := (b and (1 shl BitIndex)) <> 0;
  Result := True;
end;

function IsWriteResponseOK(const Resp: TBytes): Boolean;
var
  ParamLen, DataLen: Word;
  ReturnCodeOffset: Integer;
begin
  Result := False;
  if Length(Resp) < 12 then Exit;

  // Header checks (same as Read)
  if Resp[8] <> $03 then Exit; // ROSCTR must be Ack_Data

  // Error Class/Code
  if Length(Resp) >= 19 then
  begin
    if (Resp[17] <> 0) or (Resp[18] <> 0) then Exit;
  end;

  // Get Lengths
  if Length(Resp) >= 17 then
  begin
     ParamLen := (Resp[13] shl 8) or Resp[14];
     DataLen := (Resp[15] shl 8) or Resp[16];
  end
  else
     Exit;

  // Check Function Code (0x05 for Write Var)
  if (Length(Resp) > 19) then
  begin
     if Resp[19] <> $05 then Exit;
  end;

  // Check Return Code (Data Part)
  // Data starts at 19 + ParamLen
  ReturnCodeOffset := 19 + ParamLen;
  
  if Length(Resp) > ReturnCodeOffset then
  begin
     // For a single item write, we expect one return code byte.
     // 0xFF = Success. Other values indicate error.
     if Resp[ReturnCodeOffset] = $FF then
       Result := True
     else
     begin
       //AndroidLog.LogD('PLC', 'Write Failed. Return Code: ' + IntToHex(Resp[ReturnCodeOffset], 2));
       Result := False;
     end;
  end;
end;

end.
