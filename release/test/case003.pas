(*
<test>
  <description>case语句,多标签</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit case003;
interface

procedure test1(var d: Integer; var res: Integer);

implementation

procedure test1(var d: Integer; var res: Integer);
begin
	case d of
		1, 3, 10: res := 25;
		4, 9, 11..22: res := 33;
	else
		res := 1;
	end;
end;

end.