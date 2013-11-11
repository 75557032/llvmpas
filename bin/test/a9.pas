unit a9;

interface
{
测试开放数组参数和集合构造器

[1,2,3] - [s]
}
implementation

procedure show(args: array of const);
begin
end;

procedure show2(var args: array of const);
begin
end;

procedure show3(const args: array of const);
begin
end;

procedure show4(out args: array of const);
begin
end;

procedure show5(const args: array of TVarRec);
begin
end;

procedure sss(args: array of Integer);
begin
end;

procedure put(var buf);
begin
end;

procedure call;
var
	a1: array of LongWord;
	a2: array of TVarRec;
	b: byte;
begin
	show(['bb',2]);
	show2(a2);
	show5(a2);
	put(b);
//	sss(a1);
//	sss(['d', 2, b]);
end;

end.