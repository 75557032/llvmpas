unit a10;

interface

implementation

procedure test1;
var
	c: TClass;
	i: Integer;
	g: PGuid;
begin
	c := TObject;
	i := c.InstanceSize;
	g.d1 := 22;
end;

end.