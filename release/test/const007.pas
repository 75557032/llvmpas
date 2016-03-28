(*
<test>
  <description>集合常量表达式</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit const007;

interface

var
	i: Integer = 2;
const
	c1 = 5;
	s1 = [1,2,3,4];
	s2 = [2,3,4,c1];
	s3 = s1 + s2;
	s4 = s1 * s2;
	s5 = s1 - s2;
	
	b1 = 35 in s1;
	b2 = c1 in (s1 + s2);
	b3 = s1 >= s2;
	b4 = s1 <= s2;
	b5 = s1 <> s2;
	b6 = s1 = s2;
	b7 = s1 <= s3;
	
implementation

end.