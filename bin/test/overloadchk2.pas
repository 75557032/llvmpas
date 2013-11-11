unit overloadchk1;

interface
{ 检查overload}
implementation

function aaa: Integer; overload;
begin
	result := 0;
end;

function aaa(a:integer):Integer; overload;
begin
end;

procedure test;
	procedure bbb(a:Integer); overload;
	begin
	end;
	
	procedure bbb(a:pointer); overload;
	begin
	end;

type
	TMyFunc = function : Integer;
	
	procedure bbb(a:TMyFunc); overload;
	begin
	end;
	
	procedure ccc(a:pointer);
	begin
	end;
begin
	bbb(aaa);

	//ccc(aaa);
end;

end.