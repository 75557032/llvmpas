unit b8;

// check record field

interface

type
	TMyRec = record
		aa: Integer;
		cc: Byte;
		ss: Word;
		ff: Double;
	end;

procedure test_rec3(var rec: TMyRec);
procedure test_rec(var rec: TMyRec; a: Byte; c: Integer);
procedure test_rec2(var rec: TMyRec);

implementation

procedure test_rec3(var rec: TMyRec);
begin
	test_rec2(rec);
	test_rec(rec, 22, 33);
end;

procedure test_rec(var rec: TMyRec; a: Byte; c: Integer);
begin
	rec.aa := c;
	rec.cc := a;
	rec.ff := 0;
	rec.ss := 1;
end;

procedure test_rec2(var rec: TMyRec);
begin
	rec.aa := rec.cc + rec.ss;
end;

end.