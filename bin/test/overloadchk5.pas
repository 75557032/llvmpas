unit overloadchk5;

interface

implementation

procedure test;
type
	TMyCharSet = set of char;
	procedure aaa(a: TMyCharSet);
	begin
	end;
	
	procedure bbb(a: array of const);
	begin
	end;
begin
	aaa(['a', 'b']);
//	bbb(['a', 'b']);
	bbb(['a', 1]);
end;

end.