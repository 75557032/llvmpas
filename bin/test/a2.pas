unit a2;

interface
{ with statement }
implementation

type
	MyData = record
		dd: Integer;
	end;
	PMyData = ^MyData;
	MyRec = record
		a: Integer;
		b: smallint;
		c: char;
		d: MyData;
		p: PMyData;
	end;
	
	{IMyIntf = interface
		procedure show;
		procedure hide;
	end;
	
	MyObj = class(TObject, IMyIntf)
		procedure show;
		procedure hide;
	end;}
	
function test: MyRec;
begin
	result.a := 1;
	result.b := 2;
	result.c := 'a';
end;

procedure show;
var
	rec: MyRec;
	pr: ^MyRec;
begin
	rec.p := @rec.d;
	with rec,p^ do
	begin
		c := 'c';
		d.dd := 2;
		dd := 1;
	end;

	with rec do
	begin
		c := 'c';
		d.dd := 2;
		p^.dd := 1;
	end;

	pr := @rec;
	with pr^ do
	begin
		b := 2;
	end;
	with test do
	begin
		a := 1;
		b := 2;
	end;
end;

end.