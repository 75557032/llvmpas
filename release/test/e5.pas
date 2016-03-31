(*
<test>
  <description>
    exception handle.
	异常处理块中的 try finally
  </description>
  <command>%%pc %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
program e5;
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

//declare i32 @getch() nounwind
function getch: Integer; cdecl; external name 'getch';

procedure raiseincatch;
begin
	try
		raise tmyobj.create;
	except
		writeln('enter');
		try
			raise tmyobj.create;
		finally
			writeln('finally block in except block');
		end;
		writeln('leave');
	end;
end;

begin
	writeln('hello');
	try
		raiseincatch;
	//	fn;
	except
		writeln('error from raiseincatch');
	end;
	writeln('bye');
end.