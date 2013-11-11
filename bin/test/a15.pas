unit a15;

{ test check function }

interface

implementation

procedure test;
var
	i: Integer;
label L1, L2;
begin
	goto L1;
	try
L1:
	finally
	goto L2;
	end;
end;

procedure aa(i: Integer);
begin
end;

procedure aa(s: string);
begin
end;

procedure bb(c: char; s: string='');
begin
end;

procedure test2;
begin
	aa(2.5);
	bb(4);
end;

end.