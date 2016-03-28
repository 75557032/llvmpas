(*
<test>
  <description>function pointer in objfpc mode.</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit funcptr001;
{$mode objfpc}
interface

type
	TMyProc = procedure;

procedure test(var p: TMyProc);

implementation

procedure show;
begin
end;

procedure test(var p: TMyProc);
begin
	p := @show;
end;

end.