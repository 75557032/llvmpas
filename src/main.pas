unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    btnCompile: TButton;
    btnCopy: TButton;
    btnClear: TButton;
    btnGenSys: TButton;
    chkDumpAst: TCheckBox;
    chkGenCode: TCheckBox;
    edt_IncludeDir: TEdit;
    edt_SrcFile: TEdit;
    edt_UnitOutputDir: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mmo_LibDirs: TMemo;
    mmo_Output: TMemo;
    Panel1: TPanel;
    pgc1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnClearClick(Sender: TObject);
    procedure btnCompileClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnGenSysClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation
uses ast, cntx, dump, llvm_codegen;

{$R *.lfm}

type
  TCompiler = class
  private
    FMemo: TMemo;
    procedure OnError(ErrInfo: TParserErrorInfo);
    procedure WriteLn(const s: string);
  public
    SrcFile, IncludeDir, UnitOutputDir, UnitDirs: string;
    GenCode, IsSys, AstDump: Boolean;

    procedure Run;
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
  Msg := Format('[%s]%s(%d,%d): %s', [Levels[ErrInfo.ErrorLevel],
            ExtractFileName(ErrInfo.FileName),
            ErrInfo.Row, ErrInfo.Column, ErrInfo.ErrorMessage]);
  WriteLn(Msg);
end;

procedure TCompiler.Run;
var
  inf: string;
 // CurDir, LibDir: string;
  Context: TCompileContext;
  i: Integer;
  M: TModule;

  procedure DoGenLLCode(M: TModule);
  var
    cg: TCodeGen;
  begin
    cg := TCodeGen.Create(Context);
    try
      cg.EmitModuleDecl(M);
      Self.FMemo.Text := cg.GetIR
    finally
      cg.Free;
    end;
  end;

  procedure DoDump(M: TModule);
  var
    dump: TDump;
    s: string;
  begin
    dump := TJsonDump.Create;
    try
      s := dump.Dump(M, DumpAllOptions);
      FMemo.Lines.Add(s);
    finally
      dump.Free;
    end;
  end;
begin
  inf := SrcFile;
  inf := SysUtils.ExpandFileName(inf);
  WriteLn(inf);
  //outf := ChangeFileExt(inf, '.cu');

  try
    Context := TCompileContext.Create;
    try
      Context.UnitOutputDir := UnitOutputDir;
      Context.LibDirs.Text := UnitDirs;
      for i := 0 to Context.LibDirs.Count - 1 do
      begin
        Context.LibDirs[i] := IncludeTrailingPathDelimiter(ExpandFileName(Context.LibDirs[i]));
      end;
{      CurDir := ExtractFilePath(ParamStr(0));
      LibDir := ExtractFilePath(ExcludeTrailingPathDelimiter(CurDir));
      LibDir := LibDir + 'lib' + PathDelim;
      Context.LibDirs.Add(LibDir);}

      Context.OnError := @OnError;
      Context.IsSystemUnit := IsSys;
      M := Context.Compile(inf);
      if not Context.HasError then
      begin
        if GenCode then DoGenLLCode(M);
        if AstDump then DoDump(M);
//        FMemo.Lines.Add(M.Codes);
//        if AstDump then FMemo.Lines.Add(M.Dump);
      end;
    finally
      Context.Free;
    end;
  except
    on E: EParseStop do ;
    on E: EParserError do
      WriteLn(Format('%s(%d,%d): %s', [ExtractFileName(E.FileName), E.Row, E.Column, E.Message]));
    on E: Exception do begin
      WriteLn(E.Message);
    end;
  end;
end;

procedure TCompiler.WriteLn(const s: string);
begin
  FMemo.Lines.Add(s)
end;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  pgc1.ActivePageIndex := 0;
end;

procedure TMainFrm.btnCompileClick(Sender: TObject);
var
  Compiler: TCompiler;
  s: string;
begin
  s := Trim(edt_SrcFile.Text);
  if s = '' then Exit;

  Compiler := TCompiler.Create;
  try
    Compiler.FMemo := mmo_Output;
    Compiler.SrcFile := s;
    Compiler.UnitOutputDir := edt_UnitOutputDir.Text;
    Compiler.UnitDirs := mmo_LibDirs.Text;
    Compiler.IncludeDir := edt_IncludeDir.Text;
    Compiler.GenCode := chkGenCode.Checked;
    Compiler.AstDump := chkDumpAst.Checked;
    Compiler.IsSys := False;
    Compiler.Run;
  finally
    Compiler.Free;
  end;
end;

procedure TMainFrm.btnClearClick(Sender: TObject);
begin
  mmo_Output.Clear;
end;

procedure TMainFrm.btnCopyClick(Sender: TObject);
begin
  mmo_Output.SelectAll;
  mmo_Output.CopyToClipboard;

end;

procedure TMainFrm.btnGenSysClick(Sender: TObject);
var
  Compiler: TCompiler;
  s: string;
begin
  s := Trim(edt_SrcFile.Text);
  if s = '' then Exit;

  Compiler := TCompiler.Create;
  try
    Compiler.FMemo := mmo_Output;
    Compiler.SrcFile := 'sources\rtl\system.pas';
    Compiler.UnitOutputDir := 'lib\i386-win32\rtl\';
    Compiler.IsSys := True;
    Compiler.GenCode := True;
    Compiler.Run;
  finally
    Compiler.Free;
  end;
end;

end.

