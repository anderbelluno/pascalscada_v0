unit S7PDU;

{$mode delphi}

interface

uses
  Classes, SysUtils, S7Types_Siemens, AndroidLog;

function BuildReadVar(Area: TS7Area; DBNum: Word; ByteOffset: LongWord; SizeBytes: Word; TSize: TS7TransportSize): TBytes;

implementation

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
