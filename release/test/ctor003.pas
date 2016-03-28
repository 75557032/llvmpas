program ctor003;

type
	tmyobj = class
		class function getIntfEntry(const IID: TGUID): PInterfaceEntry;
		destructor destroy; override;
	end;

function ScException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HResult($8000FFFF); { E_UNEXPECTED }
end;

class function tmyobj.getIntfEntry(const IID: TGUID): PInterfaceEntry;
begin
	result := nil;
end;

destructor tmyobj.destroy; 
begin
	//inherited;
end;

begin
end.