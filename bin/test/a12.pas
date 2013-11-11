unit a12;

interface

implementation

type
	TMethod = record
		Data, Code: Pointer;
	end;
	
procedure test_cast;
type
	TMyFunc = function: IInterface of object;
var
  M: function: IInterface of object;
  Intf: IInterface;
  P: Pointer;
	//i1, i2: TGuid;
begin
//	if (Int64(i1.D1) = Int64(i2.D1)) and
//	   (Int64(i1.D4) = Int64(i2.D4)) then Exit;
	{TMethod(M).Data := nil;
	TMethod(P^).Data := nil;
	TMethod(TMyFunc(P^)).Data := nil;}
	Intf := M;
end;

end.