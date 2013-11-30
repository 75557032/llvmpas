unit b7;

// check cast

interface
(*
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
type
	TMyEvent2 = procedure of object;
function test_event(const e: TMyEvent2): Boolean;

function test_rec(rec: tmyrec): tmyrec;

procedure test_cast(var a: byte; var b,c: word; p: pointer);
*)
procedure safecall_proc; safecall;
procedure call_safecall;

implementation

function test_str(s: string): string;
begin
	result := s;
end;

procedure test_str2(var s: string; const s2: string);
begin
	s := test_str(s2);
end;

procedure safecall_proc; safecall;
begin
end;

procedure call_safecall;
begin
	safecall_proc;
end;
(*
function test_event(const e: TMyEvent2): Boolean;
begin
	Result := Assigned(e);
end;

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
*)
var
	i: Integer;
initialization
	inc(i);
finalization
	dec(i);
end.