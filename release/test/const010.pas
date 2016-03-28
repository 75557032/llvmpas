(*
<test>
  <description>packed array and sizeof</description>
  <command>%%pc %%source -E -dump </command>
  <expect>
    <output action="contains">{"vt":"Int64","value":16}</output>
  </expect>
</test>
*)
unit const010;

interface

type
  tmyarr1 = packed array[0..2] of Integer;

  {$A1}
  tmyarr2 = array[0..2] of Integer;
  {$A8}

  tmyrec1 = record
    c: byte;
    arr: tmyarr1;
  end;

  tmyrec2 = record
    c: byte;
    arr: tmyarr2;
  end;

  tmyrec3 = record
    c: byte;
    arr: packed array[0..2] of Integer;
  end;

const
	c1 =sizeof(tmyrec1);
	c2 =sizeof(tmyrec2);
	c3 =sizeof(tmyrec3);
implementation

end.