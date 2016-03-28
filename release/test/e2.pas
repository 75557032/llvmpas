(*
<test>
  <description>
    exception handle.
	异常处理
  </description>
  <command>%%pc %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
program e2;
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
		class procedure classProc;
	end;

	tmydata = class
	public
		constructor create;
	end;
	
class procedure tmyobj.classProc;
begin
end;

constructor tmyobj.Create;
begin
	classProc;
//	fdata := 1;
	
	writeln('tmyobj.create');
end;

constructor tmyobj.destroy;
begin
	writeln('tmyobj.destroy');
end;

constructor tmydata.create;
begin
	writeln('tmydata.create: enter');
	writeln('tmydata.create: before raise');
	try
		raise tobject.create;
	finally
		writeln('tmydata.create: after raise');
	end;
	writeln('tmydata.create: leave');
end;

//declare i32 @getch() nounwind
function getch: Integer; cdecl; external name 'getch';

procedure raiseincatch;
begin
	try
		raise tobject.create;
	except
		on e: tmyobj do
			writeln('catch a tmyobj yet');
	else
		writeln('enter');
		try
			raise tmyobj.create;
		finally
			writeln('leave');
		end;
	end;
end;

procedure raiseerr(a: Integer);
begin
	try
		try
			raise tobject.create;
		except
			on e: tmyobj do
				writeln('catch a tmyobj yet');
		end;
	except
		writeln('raiseerr: outter try');
		raise;
	end;
end;

procedure testobj;
var
	obj:tmyobj;
begin
	obj := tmyobj.create;
	if obj is tmyobj then
		writeln('obj is tmyobj');
	obj.free;

	try
		writeln('before raise');
		try
			raise tmyobj.Create;
		finally
			writeln('in finally');
		end;
	except
		on e: tmyobj do
			writeln('catch a tmyobj');
		on e: tobject do
			writeln('catch an object');
	else
		writeln('catch an unknown exception');
		getch;
	end;

	try
		raiseerr(1);
	except
		writeln('catch a object from raiseerr');
	end;
	
	try
		tmydata.create;
	except
		writeln('error from tmydata.create');
	end;
end;

begin
	writeln('hello llvmpas');
	testobj;
	writeln('hello end');
end.