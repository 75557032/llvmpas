unit main;

{$mode objfpc}{$H+}
{$warn 5024 off To disable parameter 'xxx' not used}
interface

uses
  Classes, SysUtils, FileUtil, RTTICtrls, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Grids, Buttons;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    btnRun: TButton;
    btnClose: TButton;
    btnStop: TButton;
    cbItems: TComboBox;
    edtDir: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    grd1: TStringGrid;
    btnOpenDir: TSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnOpenDirClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure grd1DblClick(Sender: TObject);
  private
    FSavedName, FSavedMsg, FSavedOutput: string;
    FThread: TThread;
    FRunning: Boolean;
    procedure ClearItems;
    procedure AddResultSync;
    procedure FinishRunSync;
    procedure LoadDir(const Path: string);

  public
    { public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

uses run, memodlg, SyncObjs;

{$R *.lfm}

type
  TItemInfo = class
  public
    FileName: string;
  end;

  { TRunThread }

  TRunThread = class(TThread)
  private
    FFiles: array of string;
    FForm: TMainFrm;
    FEvent: TEvent;
    FPaused: Boolean;
    procedure AddResult(const AName, AMsg: string; const aOutput: string = '');
  protected
    procedure Execute; override;
  end;

{ TRunThread }

procedure TRunThread.AddResult(const AName, AMsg, AOutput: string);
begin
  FForm.FSavedName := AName;
  FForm.FSavedMsg := AMsg;
  FForm.FSavedOutput := AOutput;
  Synchronize(@FForm.AddResultSync);
end;

procedure TRunThread.Execute;

  procedure RunItem(const sourceFile: string);
  var
    Item: TTestItem;
    cmd, workDir, output, fn, lpcfn: string;
    code: Integer;
  begin
    lpcfn := ExpandFileName('bin\lpc.exe');
    fn := extractFileName(sourceFile);
    try
      Item := run.Parse(sourceFile);
      try
        workDir := ExtractFilePath(lpcfn);
        cmd := Item.Command;
        cmd := StringReplace(cmd, '%%pc', lpcfn, [rfReplaceAll]);
        cmd := StringReplace(cmd, '%%source', '"' + sourceFile + '"', [rfReplaceAll]);
        output := run.RunDOS(cmd, workDir, DWord(code));

        if Item.ExitCode <> '' then
        begin
          if Item.ExitCode = IntToStr(code) then
            AddResult(fn, 'OK', output)
          else
            AddResult(fn,
              Format('Expect exit code %s, but %d', [Item.ExitCode, Integer(code)]),
              output
            );
        end;

        if Item.Output <> '' then
        begin
          if AnsiPos(Item.Output, output) > 0 then
            AddResult(fn, 'OK')
          else
            AddResult(fn, 'Output mismatch', output);
        end;
      finally
        Item.Free;
      end;
    except
      on E: Exception do
        AddResult(fn, E.Message);
    end;
  end;
var
  i: Integer;
begin
  FEvent := TEvent.Create(nil, False, False, '');
  try
    for i := 0 to Length(FFiles) - 1 do
    begin
      while FPaused do
      begin
        if Self.Terminated then Break;
        FEvent.ResetEvent;
        FEvent.WaitFor(INFINITE);
      end;

      if Self.Terminated then Break;

      if FFiles[i] <> '' then RunItem(FFiles[i]);
    end;

  finally
    Synchronize(@FForm.FinishRunSync);
    FEvent.Free;
    FEvent := nil;
  end;
end;

{ TMainFrm }

procedure TMainFrm.btnRunClick(Sender: TObject);
var
  item: TItemInfo;
  files: array of string;
  i: Integer;
begin
  if cbItems.ItemIndex < 0 then Exit;

  item := cbItems.Items.Objects[cbItems.ItemIndex] as TItemInfo;
  if item = nil then
  begin
    SetLength(files, cbItems.Items.Count);
    for i := 0 to Length(files) - 1 do
    begin
      item := cbItems.Items.Objects[i] as TItemInfo;
      if Assigned(item) then files[i] := item.FileName;
    end;
  end
  else
  begin
    SetLength(files, 1);
    files[0] := item.FileName;
  end;

  FThread := TRunThread.Create(True);
  FThread.FreeOnTerminate := False;
  TRunThread(FThread).FForm := Self;
  TRunThread(FThread).FFiles := files;
  FThread.Start;
  btnRun.Enabled := False;
  btnStop.Enabled := True;
  FRunning := True;
end;

procedure TMainFrm.btnStopClick(Sender: TObject);
var
  LStop: Boolean;
begin
  if FThread = nil then Exit;
  TRunThread(FThread).FPaused := True;

  LStop := MessageDlg('Sure to stop?', mtConfirmation,
                    [mbYes, mbNo], 0) = mrYes;
  if FThread = nil then Exit;

  TRunThread(FThread).FPaused := False;
  if LStop then FThread.Terminate;
  TRunThread(FThread).FEvent.SetEvent;
end;

procedure TMainFrm.ClearItems;
var
  i: Integer;
  item: TObject;
begin
  for i := 0 to cbItems.Items.Count - 1 do
  begin
    item := cbItems.Items.Objects[i];
    item.Free;
  end;
  cbItems.Clear;
end;

procedure TMainFrm.FinishRunSync;
begin
  btnRun.Enabled := True;
  btnStop.Enabled := False;
  FRunning := False;
  FThread.FreeOnTerminate := True;
  FThread := nil;
end;

procedure TMainFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FRunning then CloseAction := caNone;

  if CloseAction = caFree then
    Self.ClearItems;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
var
  path: string;
begin
  path := ExtractFilePath(ParamStr(0));
  edtDir.Text := SysUtils.IncludeTrailingPathDelimiter(path) + 'test';
  LoadDir(edtDir.Text);
end;

procedure TMainFrm.grd1DblClick(Sender: TObject);
var
  s: string;
begin
  if grd1.Col <> 2 then Exit;
  s := grd1.Cells[grd1.Col, grd1.Row];
  if s <> '' then
    ShowMemo(s);
end;

procedure TMainFrm.LoadDir(const Path: string);

  procedure ProcessDir(const ADir: string);
  var
    sr: TSearchRec;
    item: TItemInfo;
    s: string;
  begin
    if FindFirst(ADir + '*', faAnyFile, sr) = 0 then
    begin
      ClearItems;
      cbItems.AddItem('All', nil);
      repeat
        if sr.Attr and faDirectory <> 0 then
        begin
          Continue;
        end
        else if sr.Attr and (faSymLink or faVolumeId) <> 0 then
        begin
          Continue;
        end;

        s := ExtractFileExt(sr.Name);
        if not (SameText(s, '.pas') or SameText(s, '.pp')) then Continue;

        item := TItemInfo.Create;
        item.FileName := ADir + sr.Name;
        cbItems.AddItem(sr.Name, item);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  end;

var
  Dir: string;
begin
  Dir := SysUtils.IncludeTrailingPathDelimiter(Path);
  ProcessDir(Dir);
  if cbItems.Items.Count = 1 then
    ClearItems
  else
    cbItems.ItemIndex := 0;
end;

procedure TMainFrm.btnOpenDirClick(Sender: TObject);
var
  Dir: string;
begin
  if Dialogs.SelectDirectory('Select Folder', edtDir.Text, Dir, True) then
  begin
    edtDir.Text := Dir;
    LoadDir(Dir);
  end;
end;

procedure TMainFrm.AddResultSync;
var
  row: Integer;
begin
  row := grd1.RowCount;
  grd1.RowCount := row + 1;
  grd1.Cells[0, row] := FSavedName;
  grd1.Cells[1, row] := FSavedMsg;
  grd1.Cells[2, row] := FSavedOutput;
end;

procedure TMainFrm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

