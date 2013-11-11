unit a3;

interface

{ inner type }
type
	TPrdInfo = class
	private
		const aa = 'aa';
	private
		type
			TMyInfo = class
			const
				aa = TPrdInfo.aa;
			private
				procedure DoCheck;
			end;

		var
		fprd_no: string;
	strict private
		type
			TInner2 = class
			public const
				bb = 'bb';
			end;
	public
		property prd_no: string read fprd_no;
		procedure check;
		class procedure check(prd: TPrdInfo);
		
	end;
implementation

procedure TPrdInfo.TMyInfo.DoCheck;
begin
	if TPrdInfo.aa = '' then
		if TInner2.bb = '' then;
end;

procedure TPrdInfo.check;
begin
	if prd_no = '' then ;
end;

class procedure TPrdInfo.check(prd: TPrdInfo);
begin

end;

end.