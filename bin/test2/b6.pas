unit b6;

interface
(*
procedure test_assigned(var a: pointer);
function test_assigned2(a: pointer): boolean;
function test_exit(a: boolean; var x,y: Integer): Integer;
function test_succ(a: boolean): boolean;
function test_dec(a: boolean): boolean;
function test_dec2(a: byte): byte;
function test_inc(a: byte): byte;
function test_inc2(a: byte; b: shortint; c: Integer; d: cardinal; e: Int64; f: UInt64; i: Integer): byte;
*)
type
	pInt = ^Integer;
	
procedure test_ptr(var p: pwidechar; var p2: pansichar; var p3: pInt; i: word);

function MakeKey(c1, c2: WideChar): Integer; 

function test_bit(w1: word; s1: smallint; i1: Integer): Integer;

implementation

function test_bit(w1: word; s1: smallint; i1: Integer): Integer;
begin
	Result := i1 + (w1 and s1);
end;

function MakeKey(c1, c2: WideChar): Integer; 
begin
  Result := (Ord(c1) and $ff00) or (Ord(c2) and $00ff);
  Result := Result shl 16;
  Result := Result or ((Ord(c1) and $00ff) or (Ord(c2) and $ff00));
end;

procedure test_ptr(var p: pwidechar; var p2: pansichar; var p3: pInt; i: word);
begin
	inc(p, i);
	inc(p2, i);
	inc(p3, i);
	dec(p, 1);
	dec(p2, 1);
	dec(p3, 1);
end;
(*
procedure test_assigned(var a: pointer);
begin
	if assigned(a) then
		a := nil;
end;

function test_assigned2(a: pointer): boolean;
begin
	result := assigned(a);
end;

function test_exit(a: boolean; var x,y: Integer): Integer;
begin
	if a then
		exit(x + y)
	else
		exit(x - y);
end;
{$r+}
function test_succ(a: boolean): boolean;
begin
	result := succ(a);
end;

function test_dec(a: boolean): boolean;
begin
	dec(a);
	result := a;
end;

function test_dec2(a: byte): byte;
begin
	dec(a);
	result := a;
end;

function test_inc(a: byte): byte;
begin
	inc(a, a);
	result := a;
end;

function test_inc2(a: byte; b: shortint; c: Integer; d: cardinal; e: Int64; f: UInt64; i: Integer): byte;
begin
	inc(a, i);
	inc(b, i);
	inc(c, i);
	inc(d, i);
	inc(e, i);
	inc(f, i);
{$q+}
	inc(a, i);
	inc(b, i);
	inc(c, i);
	inc(d, i);
	inc(e, i);
	inc(f, i);
	result := a;
end;
*)
end.