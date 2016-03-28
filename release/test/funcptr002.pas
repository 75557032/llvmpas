(*
<test>
  <description>
    function pointer in delphi mode. Valid that call test2 with `show`, extend `show` to @show.
	验证test2(show)表达式中的show扩展为@show
  </description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit funcptr002;
{$mode delphi}
interface

type
	TMyProc = procedure;

procedure test(var p: TMyProc);
procedure test2(p: TMyProc);

implementation
{
	callfunc(show
}
procedure show;
begin
end;

procedure test(var p: TMyProc);
begin
	p := show;
	test2(show);
end;

procedure test2(p: TMyProc);
begin
end;

end.