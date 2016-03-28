(*
<test>
  <description>class基本检查</description>
  <command>%%pc %%source -E </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
unit class001;

interface

type
	tmybase = class
	{strict} private
		procedure show(i: byte); virtual;
	protected
		procedure show; virtual;
	end;
	
	tmyobj = class(tmybase)
	protected
		procedure show; override;
		procedure show(i: byte); override;
	end;
	
	
implementation

procedure tmybase.show(i: byte);
begin
end;

procedure tmybase.show;
begin
end;

procedure tmyobj.show;
begin
	//inherited;
end;

procedure tmyobj.show(i: Byte);
begin
	//inherited;
end;

end.