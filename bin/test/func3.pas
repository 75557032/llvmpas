unit fun3;

interface
{ 检查函数重复声明与定义 错误 }

procedure test(a: byte); overload;

implementation

type
	tmyobj = class
	public
		procedure show; 
	end;
	
	tmyobj2 = class(tmyobj)
	public
		procedure show(a:byte); overload;
		
	end;

{ tmyobj }
	
procedure tmyobj.show;
begin
end;

{ tmyobj2 }

procedure tmyobj2.show(a:byte);
begin
end;

procedure test(a: byte);
var
	obj: tmyobj2;
begin
	obj.show(a);
	obj.show();
end;

procedure show(a: byte);
	procedure test(a:boolean); overload;
	begin
		
	end;
begin
	test(1);
end;

{
procedure test(const A: AnsiString); overload;
procedure test(const W: WideString); overload;
procedure test(const U: UnicodeString); overload;

var
	pw: pwidechar;
begin
	pw := nil;
	test(pw);
end;
}

{

procedure SomeValue(a: Integer); overload;
begin
end;

function SomeValue: Integer; overload;
begin
end;

procedure mytest;
	procedure my1(i: Integer);
	begin
	end;
var
	ptr: function: Integer;
begin
	ptr := SomeValue;
	showmessagefmt('%x', [ptr]);
	my1(SomeValue);
end;
}
end.