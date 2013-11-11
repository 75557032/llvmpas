unit a11;

interface
{ 测试预定义函数 }
implementation

type
	TMyInfo = class
	public
		const X = 1;
	end;

procedure test_typeinfo;
var
	p: Pointer;
begin
	p := typeinfo(TMyInfo);
	p := typeinfo(string);
	//p := typeinfo('a'); //!ERR
end;

procedure test_addr;
var
	p: Pointer;
begin
	//p := Addr('test');//!ERR
	p := Addr(p);
	p := Addr(test_addr);
end;

procedure test_assigned;
  function ff: Pointer;
  begin
  end;
var
  b: Boolean;
begin
  b := Assigned(nil);
  //b := Assigned(test_assigned);//!ERR
  //b := assigned(ff);//!ERR
  b := Assigned(@test_assigned);
  b := Assigned(@b);
end;

procedure test_copy;
var
	s: string;
	s1,s2: array of string;
	s3: array of string;
begin
	s := copy('aa', 1, 2);
	s1 := copy(s2, 1, 2);
end;

procedure test_exclude;
  function aa: string;
  begin
  end;
type
  myset = set of 'a'..'z';
var
  p: Pointer;
  s: myset;
begin
  p := @s;
  Exclude(s, 'a');
  Exclude(myset(p^), 'a');
  //exclude(myset(aa), 'a');//!ERR
  Exclude(myset(pointer(pansichar(p) + 1)^), 'a');
end;

procedure test_var;
	procedure dotest(var p);
	begin
	end;
var
	p: Pointer;
begin
	dotest(p);
end;

procedure test_inc;
type
	PPByte = ^PByte;
var
  p: pbyte;
  p2: ppbyte;
  a2: array of pbyte;
  
begin
  inc(p);
  inc(p, 2);
  inc(a2[1], 1);
  inc(p2^);
  inc(p^);
end;

procedure test_finalize;
var
  i: Integer;
  s: string;
  p: pointer;
begin
  finalize(i, 1);
  finalize(s, 1);
  finalize(p^, 1);
end;

procedure test;
const
	con1 = 245;
	scon1 = 'a';
var
	i: Integer;
begin
	i := sizeof('a');
	i := sizeof('bbb');
	i := sizeof(i);
	i := sizeof(i * 2 + 5);
	i := sizeof(TMyInfo.X);
	i := sizeof(TMyInfo.X * i);
	
	i := abs(i);
	//i := abs(shortint); // !ERR
	//i := abs(scon1); // !ERR
end;

end.