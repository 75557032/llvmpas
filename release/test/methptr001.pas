(*
<test>
  <description>method pointer</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit methptr001;

interface

type
	TMyEvent = procedure (s: tobject) of object;

procedure test(E: TMyEvent; var res: integer);
function test2(E1, E2: TMyEvent): Boolean;
procedure test3(var E: TMyEvent; const E2: TMyEvent);

implementation
{
opt testpas5.ll -O2 -S -o testpas5.o2.ll
llc testpas5.ll -filetype=asm -o testpas5.o0.s
llc testpas5.o2.ll -filetype=asm -o testpas5.o2.s
}
procedure test(E: TMyEvent; var res: integer);
begin
	if E = nil then
		res := 1;
end;

function test2(E1, E2: TMyEvent): Boolean;
begin
	result := @E1 = @E2;
end;

procedure test3(var E: TMyEvent; const E2: TMyEvent);
begin
	E := nil;
	E := E2;
end;

end.