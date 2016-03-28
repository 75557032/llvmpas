(*
<test>
  <description>interface 基本检查</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit intf001;

interface

type
	IMyIntf = interface
	['{0D733898-C65C-4B57-BC85-A6DB7928159B}']
		procedure test1(a: Integer); overload;
		procedure test2(b: double);
		procedure test1; overload;
	end;
	
	TMyObj = class(TObject, IMyIntf)
	private
		FRefCount: Integer;
	protected
		
		procedure test1(a: Integer); overload;
		procedure test2(b: double);
		procedure test1; overload;
		function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
		function _AddRef: Integer; stdcall;
		function _Release: Integer; stdcall;
	end;

implementation

function TMyObj.QueryInterface(const IID: TGUID; out Obj): HResult; 
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMyObj._AddRef: Integer;
begin
	Inc(FRefCount);
	Result := FRefCount;
end;

function TMyObj._Release: Integer;
begin
	Dec(FRefCount);
	Result := FRefCount;
	if Result = 0 then Destroy;
end;

procedure TMyObj.test1(a: Integer); 
begin
end;

procedure TMyObj.test2(b: double);
begin
end;

procedure TMyObj.test1; 
begin
end;

end.