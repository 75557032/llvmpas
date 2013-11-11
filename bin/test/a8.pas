unit a8;

{ test method resolution }
interface
type
	ictrl = interface
		procedure show;
		procedure hide;
	end;
	
	tmyobj = class(TObject, ictrl)
	private
		procedure ictrl_show;
		procedure ictrl_hide;
		procedure ictrl.show = ictrl_show;
		procedure ictrl.hide = ictrl_hide;
	end;

implementation

procedure tmyobj.ictrl_show;
begin
end;

procedure tmyobj.ictrl_hide;
begin
end;

end.