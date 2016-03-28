(*
<test>
  <description>method pointer assignment</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit methptr002;
{$mode delphi}
interface

type
	TMyEvent = procedure (s: tobject) of object;
	tmyobj = class
	public
		procedure doclick(s: tobject);
	end;
	
procedure test(obj: tmyobj; var E: TMyEvent; var res: Boolean);

implementation
{
opt testpas5.ll -O2 -S -o testpas5.o2.ll
llc testpas5.ll -filetype=asm -o testpas5.o0.s
llc testpas5.o2.ll -filetype=asm -o testpas5.o2.s
}
procedure test(obj: tmyobj; var E: TMyEvent; var res: Boolean);
begin
	E := obj.doclick;
	res := E <> nil;
end;

procedure tmyobj.doclick(s: tobject);
begin
end;

end.