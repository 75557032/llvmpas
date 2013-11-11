unit fileutils;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses SysUtils, ast;

procedure GetFileTimeStamp(const S: string; out TimeStamp: TFileTimeStamp); overload;
procedure GetFileTimeStamp(Handle: THandle; out TimeStamp: TFileTimeStamp); overload;

implementation
uses Windows;

procedure ToTimeStamp(const T: TSystemTime; var TimeStamp: TFileTimeStamp);
begin
  TimeStamp.Date := T.wYear * 10000 + T.wMonth * 100 + T.wDay;
  TimeStamp.Time := T.wHour * 10000000 + T.wMinute * 100000 + T.wSecond * 1000 + T.wMilliseconds;
end;

procedure GetFileTimeStamp(const S: string; out TimeStamp: TFileTimeStamp);
var
  FindData: TWin32FindData;
  hFind: THandle;
  localT: TFileTime;
  sysT: TSystemTime;
begin
  TimeStamp.Date := 0;
  TimeStamp.Time := 0;
  hFind := FindFirstFile(PChar(S), FindData);
  if hFind <> INVALID_HANDLE_VALUE then
  begin
    if FileTimeToLocalFileTime(FindData.ftLastWriteTime, localT)
      and FileTimeToSystemTime(localT, sysT) then
    begin
      ToTimeStamp(sysT, TimeStamp);
    end;
    FindClose(hFind);
  end;
end;

procedure GetFileTimeStamp(Handle: THandle; out TimeStamp: TFileTimeStamp);
var
  lastWrite, localT: TFileTime;
  sysT: TSystemTime;
begin
  TimeStamp.Date := 0;
  TimeStamp.Time := 0;
  if GetFileTime(Handle, nil, nil, @lastWrite)
    and FileTimeToLocalFileTime(lastWrite, localT)
    and FileTimeToSystemTime(localT, sysT) then
  begin
    ToTimeStamp(sysT, TimeStamp);
  end
end;

end.
