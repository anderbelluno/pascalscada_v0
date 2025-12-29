unit uMain;

{$mode delphi}

interface

uses
  Classes, SysUtils, AndroidWidget, Laz_And_Controls,
  tcpsocketclient, And_jni, AndroidLog,
  TCPPort, ISOTCPDriver_Siemens, Tags, S7Types_Siemens;

type

  { TAndroidModule1 }

  TAndroidModule1 = class(jForm)
    btnConnect: jButton;
    EditText1: jEditText;
    EditText2: jEditText;
    Panel1: jPanel;
    TCPSocketClient1: jTCPSocketClient;
    TextView1: jTextView;
    Timer1: jTimer;
    procedure AndroidModule1Destroy(Sender: TObject);
    procedure AndroidModule1JNIPrompt(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure OnS7Connected(Sender: TObject);
    procedure OnTagUpdated(Sender: TObject; const ValueText: string);
    procedure Timer1Timer(Sender: TObject);
  private
    PortTCP: TPortTCP;
    Driver: TISOTCPDriver;
    ReadNumber: TPLCTagNumber;
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

procedure TAndroidModule1.AndroidModule1Destroy(Sender: TObject);
begin
    if Assigned(ReadNumber) then ReadNumber.Free;
  if Assigned(Driver) then Driver.Free;
  if Assigned(PortTCP) then PortTCP.Free;
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
              .SetScanInterval(1000)
              .SetAutoRead(True)
              .SetMemReadFunction(4)  // DB
              .SetMemFileDB(1)        // DB1
              .SetMemAddress(2)       // Offset 2
              .SetKind(pttSmallInt);  // Equivalente ao PascalSCADA para Int
    ReadNumber.OnValueChange := OnTagUpdated;

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
     // ReadNumber.Read;
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
