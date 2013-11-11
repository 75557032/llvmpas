unit b5;

interface

type
	TMyArr = array[0..2, 1..3] of array of smallint;
	TMyArr2 = array[0..2] of string;
	PByte = ^Byte;
	TMyArr3 = array[0..3] of PByte;
	PPByte = ^PByte;
	PSmallint = ^Smallint;
	PPSmallint = ^PSmallint;
	TMyArr4 = array of string;
	
	TMySubRng = 1..25;
	
//function arr_test2(p: pbyte): Integer;
//function arr_test(const a: TMyArr): Integer;

function rngchk_test1(a: Integer): TMySubRng;

function bol_test(b: longbool): boolean;
function bol_test2(b: boolean): boolean;
function bol_test3(b: longbool): wordbool;
function neg_test(var a: integer; var b: int64; var c: currency; var d: double): boolean;

procedure if_test(b: longbool; var c: integer);

//function for_test1(value: Integer; key: Byte): Integer;
procedure for_test2(a,b: Integer; var c: Integer);

{
function arr_test4(const a: TMyArr3; x1,y1,x2,y2: Integer): Integer;
function arr_test2(const a: tmyarr; x,y,z: Integer): Integer;
function arr_test3(const a: tmyarr2; x,y: Integer): Char;

procedure while_test(a,b: Integer; var c: Integer);
}
function arr_test5(const a: tmyarr4; x,y: Integer): char;
function arr_test6(p: ppbyte; x1,y1,x2,y2: Integer): Integer;
function arr_test7(p: ppsmallint; x1,y1,x2,y2: Integer): Word;

implementation

const bbb = 22;

{$define ccc}
{$ifndef ccc}
{$ifndef a}
ssfd '
'
{$else ss}
	aa
{$endif}
bb
{$endif}

{$if 1+bbb=23}
(*$q+*)
	{$ifndef ccc}
	aa
	{$else}
	
	{$endif}
{$endif}

{$ifopt q+}
{$r+}
{$endif}

function rngchk_test1(a: Integer): TMySubRng;
begin
	Result := a;
end;

function arr_test5(const a: tmyarr4; x,y: Integer): char;
begin
	result := a[x,y];
end;

function arr_test6(p: ppbyte; x1,y1,x2,y2: Integer): Integer;
begin
	result := p[x1,y1] * p[x2,y2];
end;

function arr_test7(p: ppsmallint; x1,y1,x2,y2: Integer): Word;
begin
	result := p[x1,y1] * p[x2,y2];
end;

{
function arr_test4(const a: TMyArr3; x1,y1,x2,y2: Integer): Integer;
begin
	Result := a[x1][y1] * a[x2,y2];
end;

function arr_test2(const a: tmyarr; x,y,z: Integer): Integer;
begin
	result := a[x,y,z];
end;

function arr_test3(const a: tmyarr2; x,y: Integer): Char;
begin
	result := a[x,y];
end;

procedure while_test(a,b: Integer; var c: Integer);
begin
	while a <= b do
		a := a + 1;
end;
}
procedure if_test(b: longbool; var c: integer);
begin
	if b and (c > 0) then
		c := c + 2
	else
		c := c - 2;
end;
{
function for_test1(value: Integer; key: Byte): Integer;
var
	i: Integer;
begin
	result := 0;
	for i := 0 to 3 do
	begin
	//result += ((value >> (i * 8)) ^ key) << (i * 8);
		result := result + ((value shr (i*8)) xor key) shr (i * 8);
	end;
end;
}
procedure for_test2(a,b: Integer; var c: Integer);
var
	i: Integer;
begin
	for i := a to b do
		c := c xor i;
end;


function bol_test(b: longbool): boolean;
begin
	b := not b;
	result := b;
end;

function bol_test2(b: boolean): boolean;
begin
	result := not b;
end;

function bol_test3(b: longbool): wordbool;
begin
	b := not b;
	result := b;
end;

function neg_test(var a: integer; var b: int64; var c: currency; var d: double): boolean;
begin
	a := -a;
	b := -b;
	c := -c;
	d := a * -d;
	result := true;
end;

{function arr_test(const a: TMyArr): Integer;
begin
	result := a[0] + a[1] + a[2];
end;}

{
function arr_test2(p: pbyte): Integer;
begin
	result := p[0] + p[1];
end;}

end.