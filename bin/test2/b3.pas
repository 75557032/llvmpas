unit b3;

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
	end;
	
	tmyobj = class;
	
	tmyobj = class
	public
		ccc: char;
	end;
	
procedure test1(a:byte; m: tmydata);
function test2(a:byte): tmydata;

implementation

procedure testa;
label
  L1;
begin
	try
	goto L1;
L1:
	finally
	end;
end;

procedure test1(a:byte; m: tmydata);
const
	flag: boolean = True;
var
	i: Integer;
	procedure nest1;
	begin
		inc(i, a);
	end;
begin
end;

function test2(a:byte): tmydata;
begin
	result.a := a;
end;

end.