(*
<test>
  <description>检查在构造函数调用另一个构造函数</description>
  <command>%%pc %%source </command>
  <expect>
    <output action="contains">IsCtorInner=true</output>
  </expect>
</test>
*)
program ctor002;

type
  tmyobj = class
  constructor create2; virtual;
  function clstype: TClass;
	class function clsNameIs(const Name: string): Boolean;
	procedure test_as(const s: shortstring);
	procedure test_as2;
  end;

  tmyobj2 = class(tmyobj)
  constructor create(a:byte);
  end;

function testClassType(Self: TObject): TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

{ tmyobj }

function tmyobj.clstype: TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

class function tmyobj.clsNameIs(const Name: string): Boolean;
var
  Temp: ShortString;
  I: Byte;
begin
  Result := False;
  Temp := ClassName;
  for I := 0 to Byte(Temp[0]) do
    if Temp[I] <> Name[I] then Exit;
  Result := True;
end;

procedure tmyobj.test_as(const s: shortstring);
begin
end;

procedure tmyobj.test_as2;
	procedure test_i(var i: Integer);
	begin
	end;
var
	i: Integer;
begin
	test_as('abc');
	test_i(i);
end;

constructor tmyobj.create2;
begin

end;

{ tmyobj2 }

constructor tmyobj2.create(a: byte);
var
	o: tmyobj;
begin
	create2;
	o := tmyobj.create2;
	o.free;
end;

procedure test;
var
	obj: tmyobj2;
	p: pointer;
begin
	obj := tmyobj2(tmyobj2.NewInstance);
	obj := obj.Create(1);
	obj.free;
	getmem(p, 25);
	freemem(p);
end;

begin
	test;
end.