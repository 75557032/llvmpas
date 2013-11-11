unit a18;

interface

type
	myobj = class
		a: smallint;
		b: byte;
	end;

	myobj2 = class(myobj)
		c: Byte;
    d: smallint;
	end;

	{$a1}
	mycls = class
		a: smallint;
		b: byte;
	end;

	{$a8}
	mycls2 = class(mycls)
		c: byte;
  //  d: smallint;
	end;

{$a1}
  myobject = object
    a: integer;
    b:char;
  end;
{$a8}

  myobject2 = object(myobject)
    c: char;
  end;

  myobject3 = object
    a: myobject;
    b: char;
  end;
{$a2}
  myobject4 = object(myobject2)
    d: char;
  end;
{$a8}
  myr = record
    a: integer;
    b:char;
  end;

  myr2 = record
    a: myr;
    b: char;
  end;

procedure test;
implementation

procedure test;
begin
	writeln('myobj size:', myobj.InstanceSize);
	writeln('myobj2 size:', myobj2.InstanceSize);
	writeln('mycls size:', mycls.InstanceSize);
	writeln('mycls2 size:', mycls2.InstanceSize);
	writeln('myobject size:', sizeof(myobject));
	writeln('myobject2 size:', sizeof(myobject2));
	writeln('myobject3 size:', sizeof(myobject3));
	writeln('myobject4 size:', sizeof(myobject4));
	writeln('myr size:', sizeof(myr));
	writeln('myr2 size:', sizeof(myr2));
end;

end.