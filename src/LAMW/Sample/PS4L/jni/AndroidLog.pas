unit AndroidLog;

{$mode delphi}

interface

procedure LogD(const tag, msg: string);
procedure LogI(const tag, msg: string);
procedure LogW(const tag, msg: string);
procedure LogE(const tag, msg: string);
procedure SetDebugLogging(v: Boolean);

implementation

uses SysUtils;

const
  ANDROID_LOG_DEBUG = 3;
  ANDROID_LOG_INFO  = 4;
  ANDROID_LOG_WARN  = 5;
  ANDROID_LOG_ERROR = 6;

function __android_log_write(prio: longint; tag: PChar; text: PChar): longint; cdecl; external 'log';

var
  LogDebugEnabled: Boolean = True;
  LastDbgTag, LastDbgMsg: string;
  LastDbgTick: QWord;

procedure SetDebugLogging(v: Boolean);
begin
  LogDebugEnabled := v;
end;

procedure LogD(const tag, msg: string);
begin
  if not LogDebugEnabled then exit;
  if (tag=LastDbgTag) and (msg=LastDbgMsg) then
  begin
    if (GetTickCount64-LastDbgTick)<800 then exit;
  end;
  LastDbgTag := tag;
  LastDbgMsg := msg;
  LastDbgTick := GetTickCount64;
  __android_log_write(ANDROID_LOG_DEBUG, PChar(tag), PChar(msg));
end;

procedure LogI(const tag, msg: string);
begin
  __android_log_write(ANDROID_LOG_INFO, PChar(tag), PChar(msg));
end;

procedure LogW(const tag, msg: string);
begin
  __android_log_write(ANDROID_LOG_WARN, PChar(tag), PChar(msg));
end;

procedure LogE(const tag, msg: string);
begin
  __android_log_write(ANDROID_LOG_ERROR, PChar(tag), PChar(msg));
end;

end.
