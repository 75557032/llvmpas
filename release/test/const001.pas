(*
<test>
  <description>常量检查</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit const001;

interface

const
	c1 = 25.5;
	c2 = 'd';
	c3 = ord(c2) * 25.5;
	c4 = round(25.5);
	c5 = trunc(c1) * ord(c2) + low(word);
	c6 = c2 + 'e';

implementation

end.