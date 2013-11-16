unit b7;

// check cast

interface

type
	pbyte = ^byte;
	pword = ^word;
	pInteger = ^Integer;

	TMyEvent = procedure (Sender: TObject);
	
//	mysub = 1..10;
//	myset = set of 'a'..'c';

	tmyrec = record
		a, b: integer;
	end;

	tmyobj = class
	public
		constructor Create;
	end;

function test_rec(rec: tmyrec): tmyrec;

procedure test_cast(var a: byte; var b,c: word; p: pointer);

implementation

constructor tmyobj.Create;
begin
end;

function test_rec(rec: tmyrec): tmyrec;
begin
	result.a := rec.b;
	result.b := rec.a;
end;

procedure test_cast(var a: byte; var b,c: word; p: pointer);
begin
	a := byte(b);
	b := word(p^);
	c := pword(p)^;
end;

var
	i: Integer;
initialization
	inc(i);
finalization
	dec(i);
end.