(*
<test>
  <description>case语句</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit case001;
interface

procedure test1(var d: Integer; var res: Integer);

implementation

procedure test1(var d: Integer; var res: Integer);
begin
	case d of
		1, 2, 10: res := 25;
		4, 6, 7..9: res := 33;
	else
		res := 1;
	end;

	res := res * res;
end;

end.