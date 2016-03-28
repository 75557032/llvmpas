(*
<test>
  <description>method pointer. (obj.getobj.docheck tranlate obj.getobj to call)
    obj.getobj.docheck这个表达式中的obj.getobj需要转成函数调用
  </description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit methptr006;

{$mode delphi}
interface

type
	tmyobj = class
	public
		procedure docheck;
		function getobj(): tmyobj;
	end;
	
procedure test(obj: tmyobj);

implementation

procedure test(obj: tmyobj);
begin
	obj.getobj.docheck;
end;

procedure tmyobj.docheck;
begin
end;

function tmyobj.getobj(): tmyobj;
begin
	result := Self;
end;

end.