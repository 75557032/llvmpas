(*
<test>
  <description>类引用使用虚函数</description>
  <command>%%pc %%source </command>
  <expect>
    <output action="contains"></output>
  </expect>
</test>
*)
program classref002;

function printf(s: PChar): Integer; cdecl; varargs; external name 'printf';

procedure writeln(const s: string);
begin
	printf(PChar(s));
	printf(#13#10);
end;

type
	tmyobj = class
		destructor destroy; override;
		class procedure m1; virtual;
	end;

	tmyclass = class of tmyobj;

	tmyobj2 = class(tmyobj)
		class procedure m1; override;
	end;
	
destructor tmyobj.destroy; 
begin
	//inherited;
end;

class procedure tmyobj.m1; 
begin
	writeln('tmyobj1.m1');
end;

{ tmyobj2 }

class procedure tmyobj2.m1; 
begin
	writeln('tmyobj2.m1');
end;

procedure test(klass: tmyclass);
begin
	klass.m1;
	tmyobj.m1;
	tmyclass.m1;
end;

begin
	test(tmyobj2);
end.