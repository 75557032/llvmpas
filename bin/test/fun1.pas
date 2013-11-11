unit fun1;

interface
{ 检查函数声明重载、重复声明与定义 }

procedure test(a: byte);
procedure test(a: word);

implementation

procedure test(a: byte);
begin
end;

procedure test(a: word);
begin
end;

end.