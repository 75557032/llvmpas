unit for001;

interface

function test(p: PByte; count: Integer): Integer;

implementation

function test(p: PByte; count: Integer): Integer;
var
	i, x: Integer;
begin
	result := 0;
	for i := 0 to count - 1 do
		result := result + p[i];
	Cardinal(x) := 2;
	inc(result, x);
end;

end.