unit b7;

// check cast

interface

type
	pbyte = ^byte;
	pword = ^word;
	pInteger = ^Integer;
	
procedure test_cast(var a: byte; var b,c: word; p: pointer);

implementation

procedure test_cast(var a: byte; var b,c: word; p: pointer);
begin
	a := byte(b);
	b := word(p^);
	c := pword(p)^;
end;

end.