unit a6;

interface
type
	TMyRec = class
	strict private type
		TMyData = class
		private
			const aa = 1;
		end;
	public
		type MM = TMyData.aa..5;
	end;
	
	tMy2 = class(a6.TMyRec, System.IUnknown)
	end;
{
	MySubrange2 = TMyRec.TMyData.a div 1 .. (100);

	MySubrange = System.MagicTag1 * 2 div 1 .. (100);
	My2 = 1>2..True;
	My3 = sizeof(Integer)..Sizeof(Double);
	my4 = type string;
	my5 = type My2;
	
	sstr = 2*1..5;
	sstr2 = string[20];

	ccc = set of 'a'..'z';
	prec = ^rec;
	rec = record
		v: set of 'a'..'z';
	end;}

implementation
{
	cupersist
}
end.