(*
<test>
  <description>无构造函数的类生成并赋值给同一类型的变量</description>
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

var obj: tmyobj;
begin
	obj := tmyobj.create;
	obj.free;
end.