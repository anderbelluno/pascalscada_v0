unit PLCStructElement;

{$mode delphi}

interface

uses
  Classes, SysUtils, PLCStruct, S7Types_Siemens, AndroidLog;

type
  TPLCStructItem = class(TComponent)
  private
    FStruct: TPLCStruct;
    FOffset: Integer;
    FTagType: TS7TagType;
    FBitIndex: Integer;
    FLastValue: Integer;
    FValueValid: Boolean;
    FListeners: TList;
    procedure StructChanged(Sender: TObject);
  protected
    FOnValueChange: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SetStruct(AStruct: TPLCStruct): TPLCStructItem;
    function SetOffset(AOffset: Integer): TPLCStructItem;
    function SetTagType(AType: TS7TagType): TPLCStructItem;
    function SetBitIndex(Index: Integer): TPLCStructItem;
    function GetValueAsBoolean: Boolean;
    function GetValueAsInteger: Integer;
    function SetIndex(Value: Integer): TPLCStructItem;
    function SetPLCBlock(Value: TPLCStruct): TPLCStructItem;
    procedure AddListener(Listener: TNotifyEvent);
    procedure RemoveListener(Listener: TNotifyEvent);
    property Index: Integer read FOffset write FOffset;
    property PLCBlock: TPLCStruct read FStruct write FStruct;
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
    property TagType: TS7TagType read FTagType;
  end;

implementation

constructor TPLCStructItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList.Create;
  FBitIndex := 0;
  FValueValid := False;
end;

destructor TPLCStructItem.Destroy;
var
  i: Integer;
  P: ^TMethod;
begin
  if Assigned(FStruct) then
    FStruct.RemoveListener(StructChanged);
  if Assigned(FListeners) then
  begin
    for i := 0 to FListeners.Count - 1 do
    begin
      P := FListeners[i];
      Dispose(P);
    end;
    FListeners.Free;
  end;
  inherited Destroy;
end;

procedure TPLCStructItem.AddListener(Listener: TNotifyEvent);
var
  P: ^TMethod;
begin
  if not Assigned(FListeners) then Exit;
  New(P);
  P^ := TMethod(Listener);
  FListeners.Add(P);
end;

procedure TPLCStructItem.RemoveListener(Listener: TNotifyEvent);
var
  i: Integer;
  P: ^TMethod;
  Target: TMethod;
begin
  if not Assigned(FListeners) then Exit;
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

function TPLCStructItem.SetIndex(Value: Integer): TPLCStructItem;
begin
  if FOffset <> Value then
  begin
    FOffset := Value;
    FValueValid := False;
  end;
  Result := Self;
end;

function TPLCStructItem.SetPLCBlock(Value: TPLCStruct): TPLCStructItem;
begin
  SetStruct(Value);
  Result := Self;
end;

function TPLCStructItem.SetStruct(AStruct: TPLCStruct): TPLCStructItem;
begin
  if Assigned(FStruct) then
    FStruct.RemoveListener(StructChanged);
  FStruct := AStruct;
  FValueValid := False;
  if Assigned(FStruct) then
    FStruct.AddListener(StructChanged);
  Result := Self;
end;

function TPLCStructItem.SetOffset(AOffset: Integer): TPLCStructItem;
begin
  FOffset := AOffset;
  Result := Self;
end;

function TPLCStructItem.SetTagType(AType: TS7TagType): TPLCStructItem;
begin
  FTagType := AType;
  Result := Self;
end;

function TPLCStructItem.SetBitIndex(Index: Integer): TPLCStructItem;
begin
  FBitIndex := Index;
  Result := Self;
end;

procedure TPLCStructItem.StructChanged(Sender: TObject);
var
  CurrentValue: Integer;
  i: Integer;
  P: ^TMethod;
  NotifyEvent: TNotifyEvent;
begin
  CurrentValue := GetValueAsInteger;
  
  LogD('PLCStructItem', 'StructChanged: Offset=' + IntToStr(FOffset) + ' Val=' + IntToStr(CurrentValue) + ' Last=' + IntToStr(FLastValue) + ' Valid=' + BoolToStr(FValueValid, True));

  if (not FValueValid) or (CurrentValue <> FLastValue) then
  begin
    FValueValid := True;
    FLastValue := CurrentValue;
    if Assigned(FOnValueChange) then
    begin
      LogD('PLCStructItem', 'Triggering OnValueChange');
      FOnValueChange(Self);
    end;
    
    if Assigned(FListeners) then
    begin
      for i := 0 to FListeners.Count - 1 do
      begin
        P := FListeners[i];
        NotifyEvent := TNotifyEvent(P^);
        if Assigned(NotifyEvent) then
          NotifyEvent(Self);
      end;
    end;
  end;
end;

function TPLCStructItem.GetValueAsBoolean: Boolean;
var
  b: Byte;
begin
  if not Assigned(FStruct) then Exit(False);
  b := FStruct.GetByte(FOffset);
  Result := (b and (1 shl FBitIndex)) <> 0;
end;

function TPLCStructItem.GetValueAsInteger: Integer;
begin
  if not Assigned(FStruct) then Exit(0);
  case FTagType of
    pttByte: Result := FStruct.GetByte(FOffset);
    pttWord: Result := FStruct.GetWord(FOffset);
    pttDWord: Result := FStruct.GetDWord(FOffset);
    // Add other types as needed
    else Result := 0;
  end;
end;

end.
