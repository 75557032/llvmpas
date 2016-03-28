(*
<test>
  <description>builtin function in constant expression</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit const006;

interface

var
	arr: array[0..5] of smallint;
const
	a1 = 'a';
	a2 = $2125;
	a3 = $10002000;
	a4 = $1000000020000000;
	c1 = -25;
	c2 = -45.6345;
	c3 = Abs(c1);
	c4 = Abs(c2);
	c5 = Succ(c3) + Pred(c3);
	c6 = Ord(succ(a1)) * c1;
	c7 = Odd(c1);
	c8 = Swap($1020);
	c9 = Trunc(c2);
	c10 = Round(c2);
	c11 = Length(arr);
	c12 = sizeof(arr);
	c13 = Ord(a1);
	c14 = Chr(c12);
	c15 = high(word);
	c16 = high(int64);
	c17 = low(smallint);
	c18 = low(int64);
	c19 = Lo(ord(a1));
	c20 = Hi(ord(a1));
	c21 = lo(a2);
	c22 = hi(a2);
	c23 = lo(a3);
	c24 = hi(a3);
	c25 = lo(a4);
	c26 = hi(a4);
	c27 = swap(ord(a1));
	c28 = swap(a2);
	c29 = swap(a3);
	c30 = swap(a4);

{
const w = 10;

writeln(a1:w, a2:w);
writeln(c1:w, c2:w:5, c3:w, c4:w:5);
writeln(c5:w, c6:w, c7:w, c8:w);
writeln(c9:w, c10:w, c11:w, c12:w);
writeln(c13:w, c14:w, c15:w, c16:w*2+3);
writeln(c17:w, c18:w*2+3, c19:w, c20:w);
writeln(c21:w, c22:w, c23:w, c24:w);
writeln(c25:w, c26:w, c27:w, c28:w);
writeln(c29:w, c30:w);

}

implementation

end.