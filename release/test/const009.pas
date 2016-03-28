(*
<test>
  <description>enum and set</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit const009;

interface

type
	myenum = (aa,bb,cc);
const
	s1 = [aa..cc];
	b1 = cc in s1;
	b2 = cc in [];
implementation

end.