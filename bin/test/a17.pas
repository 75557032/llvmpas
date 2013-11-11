unit a17;

interface
{ 检查函数声明重载、重复声明与定义 }

implementation

type
  tmyobj = class
  procedure show;
  end;

  tmyobj2 = class(tmyobj)
    procedure show(i: byte); overload;
    procedure show(a: string); overload;
	
  end;
  
{ tmyobj }

procedure tmyobj.show;
begin

end;

{ tmyobj2 }

procedure tmyobj2.show(i: byte);
begin

end;

procedure tmyobj2.show(a: string);
begin

end;
{
procedure tmyobj2.show(b:string);
begin
end;
}
{
procedure Button1Click(Sender: TObject);
var
  obj: tmyobj2;
begin
//
  obj := tmyobj2.Create;
  obj.show();
end;}

end.