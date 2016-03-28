(*
<test>
  <description>
    getobj.docheck这个表达式中的getobj需要转成函数调用。
	(under mode delphi)
  </description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit methptr004;

{$mode delphi}
interface

type
	tmyobj = class
	public
		procedure docheck;
	end;
	
function getobj: tmyobj;
procedure test;

implementation

function getobj: tmyobj;
begin
	result := nil;
end;

procedure test;
begin
	getobj.docheck;
end;

procedure tmyobj.docheck;
begin
end;

end.