(*
<test>
  <description>常量检查(real to/from int转型错误)</description>
  <command>%%pc %%source -E </command>
  <expect>
    <output action="contains">(1218)</output>
  </expect>
</test>
*)
unit const002;

interface

const
    c1 = 25;
    c2 = Double(c1);

implementation

end.