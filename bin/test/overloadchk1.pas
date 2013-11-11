unit overloadchk1;

interface
{ 检查overload}
implementation

function aaa: Integer;
begin
	result := 0;
end;

procedure test;
	procedure bbb(a:Integer); overload;
	begin
	end;
	
	procedure bbb(a:pointer); overload;
	begin
	end;
	
	procedure ccc(a:pointer);
	begin
	end;
var
	qqq: function: Integer;
begin
	bbb(qqq);
	bbb(aaa);
	
	//ccc(aaa);
end;

end.