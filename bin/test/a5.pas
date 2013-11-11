unit a2;

interface

implementation

type
	IMyIntf = interface
		procedure show;
		procedure hide;
	end;
	
	MyObj = class(TObject, IMyIntf)
		procedure show;
		procedure hide;
	end;

	MyObj2 = class(MyObj)
	end;

procedure MyObj.show;
var
	a: array[0..5, byte] of char;
	b: array[0..4] of byte;
	c: array of char;
begin
	a[0,1] := 'a';
	a[2][1] := 'c';
end;

procedure MyObj.hide;
begin
end;

end.