(*
<test>
  <description>地址常量</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit const004;

interface

var {$t+}
  arr: array[0..5] of byte;
  p1: Pointer = @arr;
  p2: Pointer = @arr[2];

implementation

end.