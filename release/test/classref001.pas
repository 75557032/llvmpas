(*
<test>
  <description>传递类引用给参数</description>
  <command>%%pc %%source </command>
  <expect>
    <output action="contains"></output>
  </expect>
</test>
*)
program classref001;

type
	tmyobj = class
		destructor destroy; override;
	end;

	tmyclass = class of tmyobj;

destructor tmyobj.destroy; 
begin
	//inherited;
end;

procedure test(klass: tmyclass);
var
	obj: tmyobj;
begin
	obj := klass.Create;
	obj.Free;
end;

begin
	test(tmyobj);
end.