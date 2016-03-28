(*
<test>
  <description>真常量不允许指针</description>
  <command>%%pc %%source -E </command>
  <expect>
    <output action="contains">(1301)</output>
  </expect>
</test>
*)
unit const005;

interface

var
  arr: array[0..5] of byte;
const
  p1 = @arr;
  p2: Pointer = @arr[2];

implementation

end.