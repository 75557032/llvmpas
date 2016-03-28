(*
<test>
  <description>case语句</description>
  <command>%%pc -E %%source </command>
  <expect>
    <output action="contains">(1501)</output>
  </expect>
</test>
*)
unit case002;
interface

procedure test1(var d: Integer; var res: Integer);

implementation

procedure test1(var d: Integer; var res: Integer);
begin
	case d of
		1, 3, 10, 13: res := 25;
		4, 9, 11..22: res := 33;
	else
		res := 1;
	end;
end;

end.