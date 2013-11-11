unit fun2;

interface
{ 检查函数重复声明与定义 错误 }

procedure test(a: byte);
procedure test(a: word);
procedure test(a: word);

implementation

procedure test(a: byte);
begin
end;

procedure test(a: word);
begin
end;

procedure test(b: string);
begin
end;

end.