(*
<test>
  <description>field基本检查</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit class001;
(*
*)
interface

type
	tmydata = record
		a: array[1..5] of byte;
		b: boolean;
		c: char;
	end;
	
	tmyrec = record
		kind: byte;
		data: ^tmydata;
	end;
	
	pmyrec = ^tmyrec;
	
procedure test1(p: pmyrec; var b: boolean);
	
implementation

procedure test1(p: pmyrec; var b: Boolean);
begin
	b := p^.data^.b;
end;

end.