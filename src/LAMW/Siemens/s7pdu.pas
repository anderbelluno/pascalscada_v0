unit S7PDU;

{$mode delphi}

interface

uses
  Classes, SysUtils, S7Types_Siemens, AndroidLog;

function BuildReadVar(Area: TS7Area; DBNum: Word; ByteOffset: LongWord; SizeBytes: Word; TSize: TS7TransportSize): TBytes;
function BuildWriteVar(Area: TS7Area; DBNum: Word; ByteOffset: LongWord; SizeBytes: Word; TSize: TS7TransportSize; const Data: TBytes): TBytes;

implementation

function BuildWriteVar(Area: TS7Area; DBNum: Word; ByteOffset: LongWord; SizeBytes: Word; TSize: TS7TransportSize; const Data: TBytes): TBytes;
var
  AreaCode: Byte;
  TS: Byte;
  Addr: LongWord;
  DataLen: Integer;
  TotalLen: Integer;
  BitLen: Word;
begin
  AreaCode := S7AreaCode(Area);
  TS := S7TransportSizeCode(TSize);
  Addr := ByteOffset * 8;
  
  if TSize = tsBit then
  begin
    BitLen := SizeBytes; // For bits, SizeBytes is number of bits
    DataLen := (BitLen + 7) div 8;
  end
  else
  begin
    BitLen := SizeBytes * 8;
    DataLen := SizeBytes;
  end;

  // Header (12) + Param (14) + Data Header (4) + Data
  TotalLen := 12 + 14 + 4 + DataLen;
  SetLength(Result, 35 + DataLen); // Pre-allocate with margin, resize later

  // 1. Header (TPKT + ISO + PDU Header) is handled by S7Transport, 
  // but here we build the S7 PDU part starting from Function Code
  
  // Actually, BuildReadVar returns the S7 PDU payload. S7Transport wraps it.
  // PDU Structure for Write Var (0x05):
  // [0] Function Code (0x05)
  // [1] Item Count (0x01)
  // [2..13] Item Specification (12 bytes)
  // [14..17] Data Header (4 bytes)
  // [18..N] Data
  
  SetLength(Result, 2 + 12 + 4 + DataLen);
  
  Result[0] := $05; // Write Var
  Result[1] := $01; // Item Count

  // Item Specification
  Result[2] := $12; // Var Spec
  Result[3] := $0A; // Length
  Result[4] := $10; // Syntax ID
  Result[5] := TS;  // Transport Size
  Result[6] := (SizeBytes shr 8) and $FF; // Length (items/bytes)
  Result[7] := SizeBytes and $FF;
  Result[8] := (DBNum shr 8) and $FF;
  Result[9] := DBNum and $FF;
  Result[10] := AreaCode;
  Result[11] := (Addr shr 16) and $FF;
  Result[12] := (Addr shr 8) and $FF;
  Result[13] := Addr and $FF;

  // Data Header
  Result[14] := $00; // Return Code
  Result[15] := $04; // Transport Size (Byte/Word/DWord) - 0x03 for Bit, 0x04 for Byte/Word
  if TSize = tsBit then Result[15] := $03;
  
  Result[16] := (BitLen shr 8) and $FF; // Length in bits
  Result[17] := BitLen and $FF;

  // Copy Data
  Move(Data[0], Result[18], DataLen);
  
  LogD('PLC', 'DEBUG_WRITE_PDU: Area=' + IntToHex(AreaCode,2) + ' DB=' + IntToHex(DBNum,4) + 
              ' Addr=' + IntToHex(Addr,8) + ' Len=' + IntToStr(DataLen));
end;

function BuildReadVar(Area: TS7Area; DBNum: Word; ByteOffset: LongWord; SizeBytes: Word; TSize: TS7TransportSize): TBytes;
var
  AreaCode: Byte;
  TS: Byte;
  Addr: LongWord;
begin
  AreaCode := S7AreaCode(Area);
  TS := S7TransportSizeCode(TSize);

  // Para todas as áreas, incluindo DB, o endereço é em bits (ByteOffset * 8)
  Addr := ByteOffset * 8;


  // O PDU de leitura deve conter:
  // 1. Function Code (0x04 = Read Var)
  // 2. Item Count (1)
  // 3. Item Specification (12 bytes)
  
  SetLength(Result, 14);

  Result[0] := $04;  // Function: Read Var
  Result[1] := $01;  // Item Count: 1
  
  // Item Specification (S7ANY) starts at offset 2
  Result[2] := $12;  // Syntax ID = S7ANY
  Result[3] := $0A;  // Length = 10 bytes seguintes
  Result[4] := $10;  // Transport specifier
  Result[5] := TS;   // Transport size (ex: $02 para Byte)
  Result[6] := (SizeBytes shr 8) and $FF;  // Length hi
  Result[7] := SizeBytes and $FF;          // Length lo
  Result[8] := (DBNum shr 8) and $FF;      // DB hi
  Result[9] := DBNum and $FF;              // DB lo
  Result[10] := AreaCode;                   // $84 para DB
  Result[11] := (Addr shr 16) and $FF;      // Addr byte 2
  Result[12] := (Addr shr 8) and $FF;      // Addr byte 1
  Result[13] := Addr and $FF;              // Addr byte 0
  
  LogD('PLC', 'DEBUG_PDU: Area=' + IntToHex(AreaCode,2) + ' DB=' + IntToHex(DBNum,4) +
              ' Addr=' + IntToHex(Addr,8) + ' Size=' + IntToStr(SizeBytes) + ' TS=' + IntToHex(TS,2));
              
  LogD('PLC', 'DEBUG_PDU_BYTES: ' + HexStr(Result)); // Using HexStr from S7Parser/AndroidLog if available or just raw hex loop
end;

end.
