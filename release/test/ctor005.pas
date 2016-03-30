(*
<test>
  <description>无构造函数的类生成并传给同一类型的参数</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
program ctor005;
{$mode delphi}

type
	tmyobj = class
	end;

procedure test(obj: tmyobj);
begin
	obj.free;
end;

begin
	test( tmyobj.create );
end.