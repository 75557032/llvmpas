unit run;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;
type

  ETestItemError = class(Exception);

  { TTestItem }

  TCheckAction = (caContains, caRegex);
  TTestItem = class
  public
    Command: string;
    ExitCode: string;
    Output: string;
    CheckAction: TCheckAction;
  end;

function RunDOS(const CommandLine, Dir: String; out ExitCode: DWORD): String;
function Parse(const FileName: string): TTestItem;

implementation
uses DOM, XMLRead;

(*
格式：
<test>
  <command>%%pc %%source </command>
  <expect>
    <exitcode>0</exitcode>
    <output action="contains">not found</output>
  </expect>
</test>
*)

function Parse(const FileName: string): TTestItem;

  function ExtractComment: string;
  var
    F: TextFile;
    s, comment: string;
    i, savedFileMode: Integer;
    flag: Boolean;
  begin
    AssignFile(F, FileName);

    try
      savedFileMode := FileMode;
      FileMode := 0;
      Reset(F);
      FileMode := savedFileMode;

      Result := '';
      comment := '';
      flag := False;
      // get first comment
      while not Eof(F) do
      begin
        Readln(F, s);
        s := Trim(s);
        if (Copy(s, 1, 2) = '(*') then
        begin
          comment := Copy(s, 3, MaxInt);
          flag := True;
          Break;
        end;
      end;

      if not flag then Exit;

      flag := False;
      while not Eof(F) do
      begin
        Readln(F, s);
        i := Pos('*)', s);
        if (i > 0) then
        begin
          comment := comment + Copy(s, 1, i - 1);
          flag := True;
          Break;
        end
        else
          comment := comment + s;
      end;

      if not flag then Exit;
    finally
      CloseFile(F);
    end;

    Result := Trim(comment);
  end;

  function LoadTestItem(const S: string): TTestItem;
  var
    xml: TXMLDocument;
    ss: TStringStream;
    root, node, n: TDOMNode;
    cmd, exitcode, output, action: string;
  begin
    ss := TStringStream.Create(S);
    xml := nil;
    try
      ReadXMLFile(xml, ss);
      root := xml.DocumentElement;
      node := root.FindNode('command');
      if node <> nil then cmd := node.TextContent;

      root := root.FindNode('expect');
      node := root.FindNode('exitcode');
      if node <> nil then exitcode := node.TextContent;

      node := root.FindNode('output');
      if node <> nil then
      begin
        output := node.TextContent;
        n := node.Attributes.GetNamedItem('action');
        if n <> nil then action := n.NodeValue;
      end;

      Result := TTestItem.Create;
      Result.Command := cmd;
      Result.ExitCode := exitcode;
      Result.Output := output;
      if action = 'regex' then
        Result.CheckAction := caRegex;

    finally
      xml.Free;
      ss.Free;
    end;
  end;

var
  S: string;
begin
  try
    S := ExtractComment;
  except
    raise ETestItemError.Create('读取测试信息失败。');
  end;

  if Length(S) > 0 then
  begin
    try
      Result := LoadTestItem(S);
    except
      raise ETestItemError.Create('分析测试信息失败。');
    end;
  end
  else
    raise ETestItemError.Create('无测试信息。');
end;

procedure CheckResult(b: Boolean);
begin
  if not b then
     Raise EOSError.Create(SysErrorMessage(GetLastError));
end;

{$warn 5057 off}
function RunDOS(const CommandLine, Dir: String; out ExitCode: DWORD): String;

  function loadText(hFile: THandle): string;
  const
    bufSize = 1024;
  var
    buf: array of Byte;
    len: Integer;
    ss: TStringStream;
  begin
    ss := TStringStream.Create('');
    try
      SetLength(buf, bufSize);
      repeat
        len := FileRead(hFile, buf[0], bufSize);
        if (len = -1) or (len = 0) then Break;

        ss.WriteBuffer(buf[0], len);
      until False;
      Result := ss.DataString;
    finally
      ss.Free;
    end;
  end;
var
  HRead,HWrite: THandle;
  StartInfo: TStartupInfo;
  ProceInfo: TProcessInformation;
  b: Boolean;
  sa: TSecurityAttributes;
begin
  Result := '';
  FillChar(sa, SizeOf(sa), 0);
  // 设置允许继承，否则在NT和2000下无法取得输出结果
  sa.nLength := SizeOf(sa);
  sa.bInheritHandle := True;
  sa.lpSecurityDescriptor := nil;
  b := CreatePipe(HRead, HWrite, @sa, 0);
  CheckResult(b);

  SetHandleInformation( HRead, HANDLE_FLAG_INHERIT, 0);

  FillChar(StartInfo, SizeOf(StartInfo), 0);
  StartInfo.cb := SizeOf(StartInfo);
  StartInfo.wShowWindow := SW_HIDE;
  //使用指定的句柄作为标准输入输出的文件句柄,使用指定的显示方式
  StartInfo.dwFlags     := STARTF_USESTDHANDLES+STARTF_USESHOWWINDOW;
  StartInfo.hStdError   := HWrite;
  StartInfo.hStdInput   := GetStdHandle(STD_INPUT_HANDLE);//HRead;
  StartInfo.hStdOutput  := HWrite;

  try
    b := CreateProcess(nil, // lpApplicationName: PChar
           PChar(CommandLine),    //lpCommandLine: PChar
           nil,    //lpProcessAttributes: PSecurityAttributes
           nil,    //lpThreadAttributes: PSecurityAttributes
           True,    //bInheritHandles: BOOL
           CREATE_NEW_CONSOLE,
           nil,
           PChar(Dir),
           StartInfo,
           ProceInfo
        );

    CheckResult(b);

    // Close the write end of the pipe before reading from the
    // read end of the pipe.
    CloseHandle(hWrite);
    Result := loadText(HRead);

    WaitForSingleObject(ProceInfo.hProcess, INFINITE);

    ExitCode := 0;
    GetExitCodeProcess(ProceInfo.hProcess, ExitCode);

    CloseHandle(ProceInfo.hProcess);
    CloseHandle(ProceInfo.hThread);
  finally
    CloseHandle(HRead);
  //  CloseHandle(HWrite);
  end;
end;
{$warn 5057 on}

end.

