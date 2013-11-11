unit a4;

interface

implementation

// test cast
procedure show;
var
	i: byte;
	p: procedure;
	ptr: Pointer;
begin
	smallint(i) := 25;
	byte(i) := 25;
	ptr := nil;
	byte(ptr^) := 25;
	word(ptr^) := 25;
//	variant(i) := 25;
//	integer(variant(i)) := 25;
	p := show;
	p := @show;
end;

{// test set constructor
procedure sss;
var
  s: set of 'a'..'z';
  w: WideChar;
begin
  s := [w, 'a'];
end;
}
end.
