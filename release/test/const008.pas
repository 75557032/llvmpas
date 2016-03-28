(*
<test>
  <description>Error in set op</description>
  <command>%%pc %%source -E </command>
  <expect>
    <output action="contains">(1301)</output>
  </expect>
</test>
*)
unit const008;

interface

var
	i: Integer = 2;
const
	c1 = 5;
	s1 = [1,2,3,i];
	
	b1 = 35 in s1;
	
implementation

end.