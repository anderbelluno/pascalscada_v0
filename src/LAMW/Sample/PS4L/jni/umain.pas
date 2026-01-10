unit uMain;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls,
  tcpsocketclient, And_jni, switchbutton, AndroidLog,
  TCPPort, ISOTCPDriver_Siemens, Tags, S7Types_Siemens, PLCString, PLCStruct, PLCStructElement;

type

  { TAndroidModule1 }

  TPendingWriteOp = (pwoNone, pwoWriteText, pwoWriteNumber);

  TAndroidModule1 = class(jForm)
    btnConnect: jButton;
    Button1: jButton;
    Button2: jButton;
    DialogYN: jDialogYN;
    EditText1: jEditText;
    EditText2: jEditText;
    EditText3: jEditText;
    EditText4: jEditText;
    Panel1: jPanel;
    Panel2: jPanel;
    Panel3: jPanel;
    ScrollView1: jScrollView;
    SwitchButton1: jSwitchButton;
    SwitchButton2: jSwitchButton;
    TCPSocketClient1: jTCPSocketClient;
    TextView1: jTextView;
    TextView2: jTextView;
    TextView3: jTextView;
    Timer1: jTimer;
    procedure AndroidModule1Destroy(Sender: TObject);
    procedure AndroidModule1JNIPrompt(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure OnS7Connected(Sender: TObject);
    procedure onS7Disconneted(Sender: TObject);
    procedure OnStringUpdated(Sender: TObject; const ValueText: string);
    procedure OnNumberUpdated(Sender: TObject; const ValueText: string);
    procedure OnTagBitUpdated(Sender: TObject; Value: Boolean);
    procedure OnTagBitUpdated2(Sender: TObject; Value: Boolean);
    procedure OnStructItemUpdated(Sender: TObject);
    procedure SwitchButton1Toggle(Sender: TObject; isStateOn: boolean);
    procedure SwitchButton2Toggle(Sender: TObject; isStateOn: boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure OnWriteNumberComplete(Sender: TObject; Status: TProtocolIOResult);
    procedure OnWriteTextComplete(Sender: TObject; Status: TProtocolIOResult);
  private
    // PT: Gerenciador de conexão TCP de baixo nível
    // EN: Low-level TCP connection manager
    PortTCP: TPortTCP;
    
    // PT: Driver S7 ISO-TCP (Protocolo Siemens)
    // EN: S7 ISO-TCP Driver (Siemens Protocol)
    Driver: TISOTCPDriver;
    
    // PT: Tag para leitura de números (Inteiros)
    // EN: Tag for reading numbers (Integers)
    ReadNumber: TPLCTagNumber;
    
    // PT: Tag para leitura de Strings
    // EN: Tag for reading Strings
    ReadText: TPLCString;
    
    // PT: Tags para leitura/escrita de bits individuais
    // EN: Tags for reading/writing individual bits
    ReadBit: TTagBit;
    ReadBit1 : TTagBit;
    
    // PT: Estrutura para leitura em bloco (melhor performance)
    // EN: Structure for block reading (better performance)
    MyStruct: TPLCStruct;
    
    // PT: Item mapeado dentro da estrutura
    // EN: Item mapped within the structure
    MyStructItem: TPLCStructItem;
    
    FS7Ready: Boolean;
    PiscaPisca: Integer;

    FPendingWriteOp: TPendingWriteOp;
    FPendingText: string;
    FPendingNumberText: string;

    procedure OnConfirmDialogClick(Sender: TObject; YN: TClickYN);
  end;

var
  AndroidModule1: TAndroidModule1;

implementation

{$R *.lfm}

{ TAndroidModule1 }

procedure TAndroidModule1.OnConfirmDialogClick(Sender: TObject; YN: TClickYN);
var
  ValueInt: Integer;
begin
  if YN <> ClickYes then
  begin
    FPendingWriteOp := pwoNone;
    Exit;
  end;

  case FPendingWriteOp of
    pwoWriteText:
      begin
        if Assigned(ReadText) then
        begin
          ShowMessage('Escrevendo... aguardando ioOk', TGravity.gvCenter, TShowLength.slShort);
          ReadText.Write(FPendingText);
        end;
      end;
    pwoWriteNumber:
      begin
        if not Assigned(ReadNumber) then Exit;
        if not TryStrToInt(FPendingNumberText, ValueInt) then
        begin
          ShowMessage('Valor inválido: ' + FPendingNumberText, TGravity.gvCenter, TShowLength.slShort);
          Exit;
        end;
        ShowMessage('Escrevendo... aguardando ioOk', TGravity.gvCenter, TShowLength.slShort);
        ReadNumber.Write(ValueInt);
      end;
  end;

  FPendingWriteOp := pwoNone;
end;

procedure TAndroidModule1.OnS7Connected(Sender: TObject);
begin
  LogD('ANDERSON','S7 Connected');
  TextView1.AppendLn('S7 Connected');
  FS7Ready := True;
  Panel1.BackgroundColor := colbrGreen;
  btnConnect.Tag:= 1;
  btnConnect.Text:= 'Desconectar';
  Timer1.Enabled := True;
end;

procedure TAndroidModule1.onS7Disconneted(Sender: TObject);
begin
  LogD('ANDERSON','S7 Disconnected');
  TextView1.AppendLn('S7 Disconnected');
  FS7Ready := False;
  btnConnect.Tag:= 0;
  btnConnect.Text:= 'Conectar';
  Timer1.Enabled:= True;
end;

procedure TAndroidModule1.OnStringUpdated(Sender: TObject;
  const ValueText: string);
begin
   TextView1.AppendLn('Valor lido: ' + ValueText);
   EditText3.Text := ValueText;
end;

procedure TAndroidModule1.OnNumberUpdated(Sender: TObject;
  const ValueText: string);
begin
  TextView1.AppendLn('Valor lido: ' + ValueText);
  EditText4.Text:= ValueText;
end;

procedure TAndroidModule1.OnTagBitUpdated(Sender: TObject; Value: Boolean);
begin
  if Value then
  begin
    TextView1.AppendLn('Bit 0 (Struct): LIGADO');
    SwitchButton1.State := tsOn;
  end
  else
  begin
    TextView1.AppendLn('Bit 0 (Struct): DESLIGADO');
    SwitchButton1.State := tsOff;
  end;
end;

procedure TAndroidModule1.OnTagBitUpdated2(Sender: TObject; Value: Boolean);
begin
  if Value then
  begin
    TextView1.AppendLn('Bit 1 (Struct): LIGADO');
    SwitchButton2.State := tsOn;
  end
  else
  begin
    TextView1.AppendLn('Bit 1 (Struct): DESLIGADO');
    SwitchButton2.State := tsOff;
  end;
end;

procedure TAndroidModule1.OnStructItemUpdated(Sender: TObject);
var
  Item: TPLCStructItem;
begin
  Item := TPLCStructItem(Sender);
  TextView1.AppendLn('Struct Item (Byte @60): ' + IntToStr(Item.GetValueAsInteger));
end;

procedure TAndroidModule1.SwitchButton1Toggle(Sender: TObject;
  isStateOn: boolean);
begin
  // Example 1: Write to Bit (Existing)
  if Assigned(ReadBit) then
    ReadBit.AsBoolean := isStateOn;
    
  // Example 2: Write to Number (ReadNumber)
  // Escreve 1234 quando LIGADO, 0 quando DESLIGADO
{  if Assigned(ReadNumber) then
  begin
    if isStateOn then
      ReadNumber.Write(1234)
    else
      ReadNumber.Write(0);
  end; }
end;

procedure TAndroidModule1.SwitchButton2Toggle(Sender: TObject;
  isStateOn: boolean);
begin
  // Example 1: Write to Bit (Existing)
  if Assigned(ReadBit1) then
    ReadBit1.AsBoolean := isStateOn;

  // Example 2: Write to String (ReadText)
  // Escreve texto diferente dependendo do estado
 { if Assigned(ReadText) then
  begin
    if isStateOn then
      ReadText.Write('LAMW Rocks!')
    else
      ReadText.Write('PascalSCADA');
  end;  }
end;

procedure TAndroidModule1.AndroidModule1Destroy(Sender: TObject);
begin
  if Assigned(MyStructItem) then
    MyStructItem.Free;
  if Assigned(MyStruct) then
    MyStruct.Free;
  if Assigned(ReadBit) then
    ReadBit.Free;
  if Assigned(ReadBit1) then
    ReadBit1.Free;
  if Assigned(ReadNumber) then
    ReadNumber.Free;
  if Assigned(ReadText) then
    ReadText.Free;
  if Assigned(Driver) then
    Driver.Free;
  if Assigned(PortTCP) then
    PortTCP.Free;
end;

procedure TAndroidModule1.AndroidModule1JNIPrompt(Sender: TObject);
begin
  // Garante que o evento esteja conectado, caso não esteja no LFM
  if Assigned(DialogYN) then
    DialogYN.OnClickYN := OnConfirmDialogClick;

  FPendingWriteOp := pwoNone;
  FS7Ready := False;
 // TextView1.AppendLn('OnJNIPrompt');
  TextView1.SetVerticalScrollBarEnabled(True);
  TextView1.SetScrollingMovementMethod();
  SetDebugLogging(True);
  Timer1.Enabled := True;
end;

procedure TAndroidModule1.btnConnectClick(Sender: TObject);
begin
  // Se o botão estiver em estado de "Conectar" (Tag = 0)
  LogD('ANDERSON','Tag ' + IntToStr(btnConnect.Tag));
  LogD('ANDERSON','FS7Ready: ' + BoolToStr(FS7Ready, True));

  TextView1.Text:= '';

  if btnConnect.Tag = 0 then
  begin
      // Recriar Driver e PortTCP para garantir Socket novo
      // O componente TCPSocketClient pode não suportar reconexão após close
      if Assigned(Driver) then
      begin
        Driver.Free;
        Driver := nil;
      end;

      if Assigned(PortTCP) then
      begin
        PortTCP.Free;
        PortTCP := nil;
      end;

      // Cria Socket Interno (passando nil)
      PortTCP := TPortTCP.Create(Self, nil);
      PortTCP.Rack := 0;
      PortTCP.Slot := 0;
      PortTCP.ConnType := 2; // OP

      Driver := TISOTCPDriver.Create(Self, PortTCP);
      Driver.SetHost('192.168.1.70');
      Driver.SetPort(102);
      Driver.OnS7Connected := OnS7Connected;
      Driver.OnS7Disconnected:= onS7Disconneted;

      // Tags: Cria se não existir, e atualiza conexão
      if not Assigned(ReadNumber) then
      begin
        ReadNumber := TPLCTagNumber.Create(Self);
        ReadNumber.SetScanInterval(1100)
                  .SetAutoRead(True)
                  .SetMemReadFunction(4)  // DB
                  .SetMemFileDB(1)        // DB1
                  .SetMemAddress(2)       // Offset 2
                  .SetKind(pttSmallInt);  // Equivalente ao PascalSCADA para Int
        ReadNumber.OnValueChange := OnNumberUpdated;
        ReadNumber.OnWriteComplete := OnWriteNumberComplete;
      end;
      ReadNumber.Connection(Driver);

      if not Assigned(ReadText) then
      begin
        ReadText := TPLCString.Create(Self);
        ReadText.SetScanInterval(900)
                 .SetAutoRead(True)
                 .SetMemReadFunction(4)
                 .SetDB(1, 4, 52);
        ReadText.OnValueChange := OnStringUpdated;
        ReadText.OnWriteComplete := OnWriteTextComplete;
      end;
      ReadText.Connection(Driver);

      if not Assigned(MyStruct) then
      begin
        MyStruct := TPLCStruct.Create(Self);
        MyStruct.SetScanInterval(1500)
                .SetAutoRead(True)
                .SetDB(1, 58, 10);
      end;
      MyStruct.Connection(Driver);

      if not Assigned(MyStructItem) then
      begin
        MyStructItem := TPLCStructItem.Create(Self);
        MyStructItem.SetStruct(MyStruct)
                    .SetIndex(0)
                    .SetTagType(pttByte);
       // MyStructItem.OnValueChange := OnStructItemUpdated;
      end;

      if not Assigned(ReadBit) then
      begin
        ReadBit := TTagBit.Create(Self);
        ReadBit.ConnectToStructItem(MyStructItem)
               .SetStartBit(0)
               .SetEndBit(0);
        ReadBit.OnValueBool := OnTagBitUpdated;
      end;

      if not Assigned(ReadBit1) then
      begin
        ReadBit1 := TTagBit.Create(Self);
        ReadBit1.ConnectToStructItem(MyStructItem)
               .SetStartBit(1)
               .SetEndBit(1);
        ReadBit1.OnValueBool := OnTagBitUpdated2;
      end;

      TextView1.AppendLn('Conectando...');
      Driver.Connect;
  end
  else
  begin
    // Estado "Desconectar"
    TextView1.AppendLn('Solicitando desconexão...');
    if Assigned(Driver) then
      Driver.Disconnect;
  end;
end;

procedure TAndroidModule1.Button1Click(Sender: TObject);
begin

  LogD('Buttom1','Click Buttom1');
  if not Assigned(ReadText) then
  begin
     LogD('Buttom1','ReadText not assigned');
    ShowMessage('Tag ReadText não inicializada.', TGravity.gvCenter, TShowLength.slShort);
    Exit;
  end;

  if ReadText.LastSyncReadStatus = ioOk then
  begin
    LogD('Buttom1',ReadText.LastSyncReadStatus.ToString);
    FPendingWriteOp := pwoWriteText;
    FPendingText := EditText3.Text;
    DialogYN.Show('Confirmação', 'Deseja escrever o texto: "' + FPendingText + '"?', 'Sim', 'Não');
  end
  else
    ShowMessage('Tag não está pronta para escrita. Status: ' + ReadText.LastSyncReadStatus.ToString, TGravity.gvCenter, TShowLength.slShort);
end;

procedure TAndroidModule1.Button2Click(Sender: TObject);
begin
  if not Assigned(ReadNumber) then
  begin
    ShowMessage('Tag ReadNumber não inicializada.', TGravity.gvCenter, TShowLength.slShort);
    Exit;
  end;

  if ReadNumber.LastSyncReadStatus = ioOk then
  begin
    FPendingWriteOp := pwoWriteNumber;
    FPendingNumberText := EditText4.Text;
    DialogYN.Show('Confirmação', 'Deseja escrever o valor: ' + FPendingNumberText + ' ?', 'Sim', 'Não');
  end
  else
    ShowMessage('Tag não está pronta para escrita. Status: ' + ReadNumber.LastSyncReadStatus.ToString, TGravity.gvCenter, TShowLength.slShort);
end;

procedure TAndroidModule1.OnWriteNumberComplete(Sender: TObject; Status: TProtocolIOResult);
begin
  LogD('ReadNumber - OnWriteComplete', Status.ToString);
  if Status = ioOk then
    ShowMessage('Valor salvo com sucesso (Async)!', TGravity.gvCenter, TShowLength.slLong)
  else
    ShowMessage('Erro ao salvar valor: ' + Status.ToString, TGravity.gvCenter, TShowLength.slLong);
end;

procedure TAndroidModule1.OnWriteTextComplete(Sender: TObject; Status: TProtocolIOResult);
begin
  LogD('ReadText - OnWriteComplete', Status.ToString);
  if Status = ioOk then
    ShowMessage('Texto salvo com sucesso (Async)!', TGravity.gvCenter, TShowLength.slLong)
  else
    ShowMessage('Erro ao salvar texto: ' + Status.ToString, TGravity.gvCenter, TShowLength.slLong);
end;

procedure TAndroidModule1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  try
    if FS7Ready then
    begin
      Panel1.BackgroundColor := colbrGreen;
    end
    else
    begin
      if PiscaPisca = 0 then
      begin
        Panel1.BackgroundColor := colbrBlack;
        PiscaPisca := 1;
      end
      else
      begin
        Panel1.BackgroundColor := colbrYellow;
        PiscaPisca := 0;
      end;
    end;
  finally
    Timer1.Enabled := True;
  end;
end;

end.
