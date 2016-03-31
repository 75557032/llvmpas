(*
<test>
  <description>
	catch块异常对象的释放
  </description>
  <command>%%pc %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
program e3;
{$mode delphi}

function printf(s: PChar): Integer; cdecl; varargs; external name 'printf';

procedure writeln(const s: string);
begin
	//printf('%s'#13#10, PChar(s));
	printf(PChar(s));
	printf(#13#10);
end;

type
	tmyobj = class
	private
		fdata: Integer;
	public
		constructor create;
		destructor destroy; override;
	end;

constructor tmyobj.Create;
begin	
	writeln('tmyobj.create');
end;

constructor tmyobj.destroy;
begin
	writeln('tmyobj.destroy');
end;

procedure testobj;
begin
	try
		raise tmyobj.create;
	except
		writeln('catch a exception');
	end;
	
	try
		raise tmyobj.create;
	except
		on e: tmyobj do
			writeln('catch a tmyobj object');
	end;
end;

begin
	writeln('hello');
	testobj;
	writeln('bye');
end.