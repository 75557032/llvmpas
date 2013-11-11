unit a16;

interface
{ 测试返回函数指针的表达式能不能正确地转成调用或取指针
}
implementation

procedure test;
type
	TMyFunc = function : Integer;
var
	a: function: Integer;
	i: Integer;
	p: pointer;
	
	procedure aaa(a: integer);
	begin
	end;
begin
	p := nil;
	i := TMyFunc(p) + 2;
	aaa(TMyFunc(p));
end;

end.