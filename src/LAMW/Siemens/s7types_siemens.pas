unit S7Types_Siemens;

{$mode delphi}

interface

type
  TS7Area = (saMarker, saDB, saInputs, saOutputs, saFlags, saTimers, saCounters);
  TS7TransportSize = (tsBit, tsByte, tsWord, tsDWord, tsReal);

function S7AreaCode(Area: TS7Area): Byte;
function S7TransportSizeCode(Size: TS7TransportSize): Byte;

implementation

function S7AreaCode(Area: TS7Area): Byte;
begin
  case Area of
    saMarker : Result := $83;
    saDB : Result := $84;
    saInputs : Result := $81;
    saOutputs : Result := $82;
    saFlags : Result := $83;
    saTimers : Result := $1D;
    saCounters : Result := $1C;
  else
    Result := $84;
  end;
end;

function S7TransportSizeCode(Size: TS7TransportSize): Byte;
begin
  case Size of
    tsBit:   Result := $01;
    tsByte:  Result := $02;
    tsWord:  Result := $04;  // Equivalente ao PascalSCADA para pttSmallInt/pttWord
    tsDWord: Result := $06;
    tsReal:  Result := $07;
  else
    Result := $04;
  end;
end;

end.
