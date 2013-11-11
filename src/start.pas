unit start;
{$ifdef FPC}
  {$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast, cntx, parser;

procedure StartCompile;

implementation

procedure test;
var
  i: Integer;
begin
  writeln('param count:', ParamCount);
  for i := 1 to ParamCount do
    writeln(ParamStr(i));
  writeln(SysUtils.GetCurrentDir);
end;

type
  TCompiler = class
  private
    procedure OnError(ErrInfo: TParserErrorInfo);
  public
    procedure Run;
  end;

procedure StartCompile;
var
  Compiler: TCompiler;
begin
  Compiler := TCompiler.Create;
  Compiler.Run;
  Compiler.Free;
end;

{ TCompiler }

procedure TCompiler.OnError(ErrInfo: TParserErrorInfo);
const
  Levels: array[TErrorLevel] of string = (
    'Error', 'Warning', 'Hint'
  );
var
  Msg: string;
begin
  Msg := Format('%s %s(%d,%d): %s', [Levels[ErrInfo.ErrorLevel],
            ExtractFileName(ErrInfo.FileName),
            ErrInfo.Row, ErrInfo.Column, ErrInfo.ErrorMessage]);
  WriteLn(Msg);
end;

procedure TCompiler.Run;
var
  inf, outf: string;
  CurDir, LibDir: string;
  Context: TCompileContext;
begin
//  test;
  if ParamCount <> 1 then
  begin
    WriteLn('Usage: lpc filename');
    Exit;
  end;

  inf := ParamStr(1);
  inf := SysUtils.ExpandFileName(inf);
  WriteLn(inf);
  outf := ChangeFileExt(inf, '.cu');

  try
    Context := TCompileContext.Create;
    try
      CurDir := ExtractFilePath(ParamStr(0));
      LibDir := ExtractFilePath(ExcludeTrailingPathDelimiter(CurDir));
      LibDir := LibDir + 'lib' + PathDelim;
      Context.UnitDirs.Add(LibDir);

      Context.OnError := OnError;
      Context.Compile(inf, outf, False, True);
    finally
      Context.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.Message);
  end;
  ReadLn;
end;

end.
