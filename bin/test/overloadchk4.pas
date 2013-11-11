unit overloadchk4;

interface

implementation

function aaa: Integer; overload;
begin
	result := 0;
end;

function aaa(a:integer):Integer; overload;
begin
end;

procedure test;
type
	TMyFunc = function: Integer;
	TMyFunc2 = function (a:Integer): Integer;
	
	procedure test1(a:TMyFunc);
	begin
	end;
	procedure test2(a:TMyFunc2);
	begin
	end;
	procedure test3(a:pointer);
	begin
	end;
begin
	//test2(@test);
	test3(@aaa);
	test1(@aaa);
	test2(@aaa);
end;

end.