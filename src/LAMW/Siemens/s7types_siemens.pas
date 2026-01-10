unit S7Types_Siemens;

{$mode delphi}

interface

type
  TS7Area = (saMarker, saDB, saInputs, saOutputs, saFlags, saTimers, saCounters);
  TS7TransportSize = (tsBit, tsByte, tsWord, tsDWord, tsReal);

  TS7TagType = (
    pttDefault,
    pttBool,
    pttShortInt, pttByte,
    pttSmallInt, pttWord,
    pttLongInt,  pttDWord, pttFloat,
    pttInt,      pttDInt,
    pttInt64,    pttQWord, pttDouble
  );

  TPLCNumberKind = (nkByte, nkWord, nkDWord, nkInt, nkDInt, nkReal);

  {:
  @name é o conjunto de resultados possíveis das comunicações entre driver e equipamento.
  @member ioNone Nenhuma ação foi executada.
  @member ioOk O comando executado foi bem sucedido.
  @member ioDriverError Ocorreu um erro interno no driver de protocolo.
  @member ioCommError Ocorreu um erro de comunicação entre o driver e a porta de comunicação.
  @member ioTimeOut O equipamento endereçado não respondeu.
  @member ioIllegalFunction A função solicitada não é suportada pelo equipamento.
  @member ioIllegalRegAddress O endereço de memória solicitado não existe no equipamento.
  @member ioIllegalValue O valor passado na escrita está fora dos limites aceitos pelo equipamento.
  @member ioPLCError O equipamento reportou um erro na execução do comando.
  @member ioTagError O tag está configurado incorretamente.
  @member ioNullDriver O tag não tem driver associado.
  @member ioIllegalRequest A requisição é inválida.
  @member ioIllegalStationAddress O endereço da estação é inválido.
  @member ioObjectNotExists O objeto solicitado não existe.
  @member ioObjectAccessNotAllowed Acesso negado ao objeto.
  @member ioIllegalMemoryAddress Endereço de memória inválido.
  @member ioNACK O equipamento respondeu com NACK (Negative Acknowledge).
  @member ioUnknownError Erro desconhecido.
  @member ioEmptyPacket Pacote vazio recebido.
  @member ioPartialOk Sucesso parcial (alguns itens falharam).
  @member ioBusy O driver está ocupado processando outra requisição.
  @member ioGatewayUnavailable Gateway indisponível.
  @member ioDeviceGatewayFailedToRespond O dispositivo alvo do gateway não respondeu.
  @member ioReadOnlyProtocol O protocolo é somente leitura.
  @member ioCommPortClosed A porta de comunicação está fechada.
  @member ioNullCommPort A porta de comunicação é nula.
  @member ioConnectPLCFailed Falha ao conectar ao PLC.
  @member ioAdapterInitFail Falha ao inicializar o adaptador.
  @member ioNullTagBlock O bloco de tags é nulo.
  }
  TProtocolIOResult = (
    ioNone,
    ioOk,
    ioDriverError,
    ioCommError,
    ioTimeOut,
    ioIllegalFunction,
    ioIllegalRegAddress,
    ioIllegalValue,
    ioPLCError,
    ioTagError,
    ioNullDriver,
    ioIllegalRequest,
    ioIllegalStationAddress,
    ioObjectNotExists,
    ioObjectAccessNotAllowed,
    ioIllegalMemoryAddress,
    ioNACK,
    ioUnknownError,
    ioEmptyPacket,
    ioPartialOk,
    ioBusy,
    ioGatewayUnavailable,
    ioDeviceGatewayFailedToRespond,
    ioReadOnlyProtocol,
    ioCommPortClosed,
    ioNullCommPort,
    ioConnectPLCFailed,
    ioAdapterInitFail,
    ioNullTagBlock
  );

  TProtocolIOResultHelper = record helper for TProtocolIOResult
    function ToString: string;
  end;

function S7AreaCode(Area: TS7Area): Byte;
function S7TransportSizeCode(Size: TS7TransportSize): Byte;

implementation

uses TypInfo;

function TProtocolIOResultHelper.ToString: string;
begin
  Result := GetEnumName(TypeInfo(TProtocolIOResult), Ord(Self));
end;

function S7AreaCode(Area: TS7Area): Byte;
begin
  case Area of
    saMarker   : Result := $83;
    saDB       : Result := $84;
    saInputs   : Result := $81;
    saOutputs  : Result := $82;
    saFlags    : Result := $83;
    saTimers   : Result := $1D;
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
