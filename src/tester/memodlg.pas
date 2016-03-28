unit memodlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TMemoDlgFrm }

  TMemoDlgFrm = class(TForm)
    btnOK: TButton;
    Memos: TMemo;
    Panel1: TPanel;
  private
    function GetText: string;
    procedure SetText(const AValue: string);
    { private declarations }
  public
    property Text: string read GetText write SetText;
  end;

procedure ShowMemo(const s: string);

implementation

{$R *.lfm}

procedure ShowMemo(const s: string);
var
  frm: TMemoDlgFrm;
begin
  frm := TMemoDlgFrm.Create(nil);
  try
    frm.Text := s;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{ TMemoDlgFrm }

function TMemoDlgFrm.GetText: string;
begin
  Result := Memos.Text;
end;

procedure TMemoDlgFrm.SetText(const AValue: string);
begin
  Memos.Text := AValue;
end;

end.

