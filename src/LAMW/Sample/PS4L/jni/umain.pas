unit uMain;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls,
  tcpsocketclient, And_jni, switchbutton, AndroidLog,
  TCPPort, ISOTCPDriver_Siemens, Tags, S7Types_Siemens, PLCString, PLCStruct, PLCStructElement;

type

  { TAndroidModule1 }

  TAndroidModule1 = class(jForm)
    btnConnect: jButton;
    EditText1: jEditText;
    EditText2: jEditText;
    Panel1: jPanel;
    Panel2: jPanel;
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
    procedure OnS7Connected(Sender: TObject);
    procedure OnTagUpdated(Sender: TObject; const ValueText: string);
    procedure OnTagBitUpdated(Sender: TObject; Value: Boolean);
    procedure OnTagBitUpdated2(Sender: TObject; Value: Boolean);
    procedure OnStructItemUpdated(Sender: TObject);
    procedure SwitchButton1Toggle(Sender: TObject; isStateOn: boolean);
    procedure SwitchButton2Toggle(Sender: TObject; isStateOn: boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    PortTCP: TPortTCP;
    Driver: TISOTCPDriver;
    ReadNumber: TPLCTagNumber;
    ReadText: TPLCString;
    ReadBit: TTagBit;
    ReadBit1 : TTagBit;
    MyStruct: TPLCStruct;
    MyStructItem: TPLCStructItem;
    FS7Ready: Boolean;
    PiscaPisca: Integer;
  end;

var
  AndroidModule1: TAndroidModule1;

implementation

{$R *.lfm}

{ TAndroidModule1 }

procedure TAndroidModule1.OnS7Connected(Sender: TObject);
begin
  TextView1.AppendLn('S7 Connected');
  FS7Ready := True;
  Panel1.BackgroundColor := colbrGreen;
  Timer1.Enabled := True;
end;

procedure TAndroidModule1.OnTagUpdated(Sender: TObject; const ValueText: string);
begin
  TextView1.AppendLn('Valor lido: ' + ValueText);
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
  ReadBit.AsBoolean := isStateOn;
end;

procedure TAndroidModule1.SwitchButton2Toggle(Sender: TObject;
  isStateOn: boolean);
begin
  ReadBit1.AsBoolean := isStateOn;
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
  FS7Ready := False;
  TextView1.AppendLn('OnJNIPrompt');
  TextView1.SetVerticalScrollBarEnabled(True);
  SetDebugLogging(True);
  Timer1.Enabled := True;
end;

procedure TAndroidModule1.btnConnectClick(Sender: TObject);
begin
  if not FS7Ready then
  begin
    TextView1.AppendLn('Conectando...');
    PortTCP := TPortTCP.Create(Self, TCPSocketClient1);
    PortTCP.Rack := 0;
    PortTCP.Slot := 0;
    PortTCP.ConnType := 2;

    Driver := TISOTCPDriver.Create(Self, PortTCP);
    Driver.SetHost('192.168.1.70');
    Driver.SetPort(102);
    Driver.OnS7Connected := OnS7Connected;

    ReadNumber := TPLCTagNumber.Create(Self);
    ReadNumber.Connection(Driver)
              .SetScanInterval(1100)
              .SetAutoRead(True)
              .SetMemReadFunction(4)  // DB
              .SetMemFileDB(1)        // DB1
              .SetMemAddress(2)       // Offset 2
              .SetKind(pttSmallInt);  // Equivalente ao PascalSCADA para Int
    ReadNumber.OnValueChange := OnTagUpdated;

    ReadText := TPLCString.Create(Self);
    ReadText.Connection(Driver)
             .SetScanInterval(900)
             .SetAutoRead(True)
             .SetMemReadFunction(4)
             .SetDB(1, 4, 52);
    ReadText.OnValueChange := OnTagUpdated;

    MyStruct := TPLCStruct.Create(Self);
    MyStruct.Connection(Driver)
            .SetScanInterval(1500)
            .SetAutoRead(True)
            .SetDB(1, 58, 10);

    MyStructItem := TPLCStructItem.Create(Self);
    MyStructItem.SetStruct(MyStruct)
                .SetIndex(0)
                .SetTagType(pttByte);
   // MyStructItem.OnValueChange := OnStructItemUpdated;

    ReadBit := TTagBit.Create(Self);
    ReadBit.ConnectToStructItem(MyStructItem)
           .SetStartBit(0)
           .SetEndBit(0);
    ReadBit.OnValueBool := OnTagBitUpdated;

    ReadBit1 := TTagBit.Create(Self);
    ReadBit1.ConnectToStructItem(MyStructItem)
           .SetStartBit(1)
           .SetEndBit(1);
    ReadBit1.OnValueBool := OnTagBitUpdated2;

    Driver.Connect;
  end
  else
  begin
    TextView1.AppendLn('Desconectando...');
    Driver.Disconnect;
  end;
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
