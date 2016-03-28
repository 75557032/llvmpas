(*
<test>
  <description>检查构造函数。以实例调用Create，是否正确地传递标志 -1</description>
  <command>%%pc %%source </command>
  <expect>
    <exitcode>0</exitcode>
    <output action="contains"></output>
  </expect>
</test>
*)
program ctor1;

type
	tmyobj = class
	end;

procedure test;
var
	obj: tmyobj;
begin
	obj := tmyobj(tmyobj.NewInstance);
	obj := tmyobj(obj.Create);
	obj.free;
end;

begin
	test;
end.