unit b2;

interface

(*type
	TBuffer = array[1..2] of Byte;
	MyEnum = (aa,bb,cc);
	TSub = aa..cc;
	TMyBuf = array[aa..cc] of Byte;
	TMyBuf2 = array[MyEnum] of Byte;
	myproc3 = procedure (a:string; b: array of string);
	mystr  = string[20];
	myproc4 = function : mystr;
	myarr1 = array of string;
	myarr2 = array of byte;
	myproc5 = function : myarr1;
	myproc6 = function : myarr2;
	*)
type
	tmydata = record
		a: smallint;
		c,d,e: double;
		s: string;
	end;
	
	tmyobj = class;
	
	tmyobj = class
	public
		ccc: char;
	end;
	
function test2(var a:byte): tmydata;

//function test_var(a: string; d: tmydata): tmydata;

implementation
(*
function test_var(a: string; d:tmydata): tmydata;
var
	v1: Variant;
	s, s2: string;
	intf1: IUnknown;
	ss: String[20];
	arr: array of string;
begin
	s := s2 + 'a' + a;
	arr := nil;
	s := ss;
	v1 := s;
end;*)

function test2(var a:byte): tmydata;
begin
	result.a := a;
end;

function test3( s: string; var b): string;
begin
	s := '';
	byte(b) := 0;
	result := s;
end;

function test4( obj: tmydata ): boolean;
begin
	obj.s := '';
end;

end.