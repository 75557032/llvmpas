unit freecode.a1;

interface

const
	d1 = System.RTLVersion;
implementation

{ test builtin function in constant expression }

const
	i1 = sizeof('aabb');
	i2 = sizeof('a');
	i3 = Integer('a'); // cast
	i4 = Ord('a');
	
procedure sss(const args: array of const);
var
	i: Integer;
begin
	//i := byte(['a']);
end;

procedure aa;
begin
  sss([1]);
  try
    try
    except
      on E: TObject do;
      on TObject do;
      else
    end;
  finally
  end;
end;

end.