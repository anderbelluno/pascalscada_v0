unit S7Transport;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, tcpsocketclient, AndroidLog;

type
  TS7FrameReceived = procedure(Sender: TObject; const Frame: TBytes) of object;
  TS7RawBytes = procedure(Sender: TObject; const Data: TBytes) of object;

  TS7Transport = class(TComponent)
  private
    FSocket: jTCPSocketClient;
    FBuffer: TBytes;
    FOnFrameReceived: TS7FrameReceived;
    FOnRawBytes: TS7RawBytes;
    FOnS7Connected: TNotifyEvent;
    FSrcTSAP: Word;
    FDstTSAP: Word;
    FPrevOnConnected: TNotifyEvent;
    procedure OnBytesReceived(Sender: TObject; var jbytesReceived: TDynArrayOfJByte);
    procedure OnConnected(Sender: TObject);
    procedure SendSetupCommunication;
    procedure Push(const Data: TBytes);
    procedure RemoveFront(var A: TBytes; Count: Integer);
    procedure Drain;
  public
    constructor Create(AOwner: TComponent; ASocket: jTCPSocketClient); reintroduce;
    procedure StartHandshake;
    procedure Send(const Data: TBytes);
    procedure SetTSAP(Src, Dst: Word);
    property OnFrameReceived: TS7FrameReceived read FOnFrameReceived write FOnFrameReceived;
    property OnRawBytes: TS7RawBytes read FOnRawBytes write FOnRawBytes;
    property OnS7Connected: TNotifyEvent read FOnS7Connected write FOnS7Connected;
  end;

implementation

procedure AppLog(const tag: string; const msg: string);
begin
  AndroidLog.LogD(tag, msg);
end;

function HexStr(const Data: TBytes; MaxBytes: Integer): string;
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

constructor TS7Transport.Create(AOwner: TComponent; ASocket: jTCPSocketClient);
begin
  inherited Create(AOwner);
  FSocket := ASocket;
  // IMPORTANT: Set DataTransferMode to dtmByte (1) to receive raw bytes
  // instead of text messages. Default is dtmText (0).
  FSocket.SetDataTransferMode(dtmByte);
  
  SetLength(FBuffer, 0);
  FSrcTSAP := $0100;
  FDstTSAP := $0202;
  FSocket.OnBytesReceived := OnBytesReceived;
  FPrevOnConnected := FSocket.OnConnected;
  FSocket.OnConnected := OnConnected;
end;

procedure TS7Transport.OnBytesReceived(Sender: TObject; var jbytesReceived: TDynArrayOfJByte);
var
  Resp: TBytes;
  i, Count: Integer;
begin
  Count := Length(jbytesReceived);
  SetLength(Resp, Count);
  for i := 0 to Count - 1 do
    Resp[i] := jbytesReceived[i];

  AppLog('PLC', 'Socket RX ' + IntToStr(Count) + ' bytes ' + HexStr(Resp, 32));

  if Assigned(FOnRawBytes) then
    FOnRawBytes(Self, Resp);
  Push(Resp);
  Drain;
end;

procedure TS7Transport.OnConnected(Sender: TObject);
begin
  if Assigned(FPrevOnConnected) then
    FPrevOnConnected(Sender);
end;

procedure TS7Transport.StartHandshake;
var
  cr: array[0..21] of Byte;
  dyn: TDynArrayOfJByte;
  i: Integer;
begin
  // TPKT + COTP Connection Request (with TSAPs)
  cr[0] := $03;
  cr[1] := $00;
  cr[2] := $00;
  cr[3] := $16; // TPKT len=22
  cr[4] := $11; // LI (Length of COTP header)
  cr[5] := $E0; // COTP: CR (Connection Request)
  cr[6] := $00;
  cr[7] := $00; // DST-REF (0)
  cr[8] := $00;
  cr[9] := $00; // SRC-REF (0)
  cr[10] := $00; // Class 0
  
  // Parameters
  // TPDU Size (C0)
  cr[11] := $C0;
  cr[12] := $01;
  cr[13] := $0A; // Size 1024 (0x0A)
  
  // Src TSAP (C1)
  cr[14] := $C1;
  cr[15] := $02;
  cr[16] := (FSrcTSAP shr 8) and $FF; 
  cr[17] := FSrcTSAP and $FF;
  
  // Dst TSAP (C2)
  cr[18] := $C2;
  cr[19] := $02;
  cr[20] := (FDstTSAP shr 8) and $FF; 
  cr[21] := FDstTSAP and $FF;

  SetLength(dyn, Length(cr));

  for i := 0 to High(cr) do
    dyn[i] := cr[i];

  AppLog('PLC', 'StartHandshake: Send CR SrcTSAP=' + IntToHex(FSrcTSAP, 4) + ' DstTSAP=' + IntToHex(FDstTSAP, 4));
  FSocket.SendBytes(dyn, False);
end;

procedure TS7Transport.Push(const Data: TBytes);
var
  L, Old: Integer;
begin
  Old := Length(FBuffer);
  L := Length(Data);
  SetLength(FBuffer, Old + L);
  Move(Data[0], FBuffer[Old], L);
end;

procedure TS7Transport.RemoveFront(var A: TBytes; Count: Integer);
var
  L: Integer;
begin
  L := Length(A);
  if Count >= L then
    SetLength(A, 0)
  else
  begin
    Move(A[Count], A[0], L - Count);
    SetLength(A, L - Count);
  end;
end;

procedure TS7Transport.Drain;
var
  L, PktLen: Integer;
  Frame: TBytes;
begin
  while True do
  begin
    L := Length(FBuffer);
    if L < 4 then
      Exit;
    
    // Check TPKT Version (must be 0x03)
    if FBuffer[0] <> $03 then
    begin
       AppLog('PLC', 'Error: TPKT Version not 0x03. Sync lost. Dropping byte: ' + IntToHex(FBuffer[0], 2));
       RemoveFront(FBuffer, 1);
       Continue;
    end;
    
    PktLen := (FBuffer[2] shl 8) or FBuffer[3];
    if (PktLen <= 0) then
    begin
       AppLog('PLC', 'Error: Invalid PDU Len ' + IntToStr(PktLen));
       RemoveFront(FBuffer, 1);
       Continue;
    end;
    
    if (PktLen > L) then 
    begin
       // Wait for more data
       AppLog('PLC', 'Drain: Waiting for more data. PktLen=' + IntToStr(PktLen) + ' BufferLen=' + IntToStr(L));
       Exit; 
    end;
    
    SetLength(Frame, PktLen);
    Move(FBuffer[0], Frame[0], PktLen);
    RemoveFront(FBuffer, PktLen);
    AppLog('PLC', 'Drain: Extracted Frame len=' + IntToStr(PktLen) + ' Content: ' + HexStr(Frame, 32));

    if Length(Frame) > 5 then
       AppLog('PLC', 'PDU Type=' + IntToHex(Frame[5], 2));

    // Check for COTP Connection Confirm (CC) - PDU Type 0xD0
    // CC packet is usually 7 bytes: 03 00 00 07 11 D0 ...
    if (Length(Frame) >= 5) and (Frame[5] = $D0) then
    begin
      AppLog('PLC', 'Received CC (Connection Confirm)');
      SendSetupCommunication;
    end
    else
    if (Length(Frame) >= 5) and (Frame[5] = $70) then
    begin
       AppLog('PLC', 'Received DR (Disconnect Request) - Handshake Refused');
    end
    else
    // Check for S7 Setup Communication Ack (Function 0xF0)
    // TPKT(4) + COTP DT(3) + S7 Header(10/12) + Param(8)
    // We look for Protocol ID 0x32 at offset 7
    if (Length(Frame) > 7) then
    begin
       if Frame[7] = $32 then
       begin
          AppLog('PLC', 'Received S7 PDU (Protocol ID 0x32)');
          if Length(Frame) > 8 then
             AppLog('PLC', 'ROSCTR=' + IntToHex(Frame[8], 2));
          
          if (Length(Frame) > 19) and (Frame[19] = $F0) then
          begin
             AppLog('PLC', 'Setup Communication Confirm.');
             if Length(Frame) >= 27 then
                AppLog('PLC', 'Negotiated PDU Size=' + IntToStr((Frame[25] shl 8) or Frame[26]));
          end;

          // Assume connection established if we get a valid S7 PDU after handshake
          if Assigned(FOnS7Connected) then 
          begin
             AppLog('PLC', 'Firing OnS7Connected event');
             FOnS7Connected(Self);
             // Don't fire it again
             FOnS7Connected := nil;
          end
          else
             AppLog('PLC', 'OnS7Connected is nil or already fired');
             
          if Assigned(FOnFrameReceived) then
            FOnFrameReceived(Self, Frame);
       end
       else
       begin
          AppLog('PLC', 'Frame[7] is not 0x32 (S7 Protocol ID). Value=' + IntToHex(Frame[7], 2));
          if Assigned(FOnFrameReceived) then
            FOnFrameReceived(Self, Frame);
       end;
    end
    else
      if Assigned(FOnFrameReceived) then
        FOnFrameReceived(Self, Frame);
  end;
end;

procedure TS7Transport.Send(const Data: TBytes);
var
  dyn: TDynArrayOfJByte;
  i: Integer;
begin
  AppLog('PLC', 'TX ' + IntToStr(Length(Data)) + ' bytes ' + HexStr(Data, 32));
  SetLength(dyn, Length(Data));
  for i := 0 to High(Data) do
      dyn[i] := Data[i];
  FSocket.SendBytes(dyn, False);
end;

procedure TS7Transport.SetTSAP(Src, Dst: Word);
begin
  FSrcTSAP := Src;
  FDstTSAP := Dst;
end;

procedure TS7Transport.SendSetupCommunication;
var
  s7: array[0..17] of Byte; // S7 Header (10) + Params (8)
  pkt: array of Byte;
  totalLen: Word;
  i: Integer;
  dyn: TDynArrayOfJByte;
begin
  // Build S7 Job: Setup Communication
  s7[0] := $32; // Protocol ID
  s7[1] := $01; // ROSCTR: Job
  s7[2] := $00; // Reserved
  s7[3] := $00; // Reserved
  s7[4] := $00; // PDU Ref (Hi)
  s7[5] := $01; // PDU Ref (Lo)
  s7[6] := $00;
  s7[7] := $08; // Parameter length = 8
  s7[8] := $00;
  s7[9] := $00; // Data length = 0
  // Parameters
  s7[10] := $F0; // Function: Setup Communication
  s7[11] := $00; // Reserved
  s7[12] := $00; // Max AmQ caller Hi
  s7[13] := $01; // Max AmQ caller Lo
  s7[14] := $00; // Max AmQ callee Hi
  s7[15] := $01; // Max AmQ callee Lo
  s7[16] := $01; // PDU length Hi
  s7[17] := $E0; // PDU length Lo (480)

  // TPKT (4) + COTP DT (3) + S7 (18)
  totalLen := 4 + 3 + Length(s7);
  SetLength(pkt, totalLen);
  // TPKT
  pkt[0] := $03;
  pkt[1] := $00;
  pkt[2] := (totalLen shr 8) and $FF;
  pkt[3] := totalLen and $FF;
  // COTP DT
  // LI = 2 (byte after LI + byte after that)
  // PDU Type = F0 (DT Data)
  // TPDU number + EOT (80 = EOT)
  pkt[4] := $02;
  pkt[5] := $F0;
  pkt[6] := $80;
  // S7
  for i := 0 to High(s7) do
    pkt[7+i] := s7[i];

  AppLog('PLC', 'Send Setup Communication');
  SetLength(dyn, Length(pkt));
  for i := 0 to High(pkt) do
    dyn[i] := pkt[i];
  FSocket.SendBytes(dyn, False);
end;

end.

