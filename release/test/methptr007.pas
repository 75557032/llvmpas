(*
<test>
  <description>方法指针比较</description>
  <command>%%pc -E %%source </command>
  <expect>
    <exitcode>0</exitcode>
  </expect>
</test>
*)
program methptr007;
{$mode delphi}

type
	TMyEvent = procedure (s: tobject) of object;
	tmyobj = class
	public
//		constructor create;
		procedure doclick(s: tobject);
		procedure doshow(s: tobject);
	end;
{
constructor tmyobj.create;
begin
end;}
procedure tmyobj.doclick(s: tobject);
begin
end;
procedure tmyobj.doshow(s: tobject);
begin
end;

function printf(s: PChar): Integer; cdecl; varargs; external name 'printf';

procedure writeln(const s: string);
begin
	//printf('%s'#13#10, PChar(s));
	printf(PChar(s));
	printf(#13#10);
end;

function test2(E1, E2: TMyEvent): Boolean;
begin
	result := @E1 = @E2;
end;
function test3(obj: tmyobj): boolean;
begin
	result := test2(obj.doshow, obj.doclick);
	obj.free;
end;

var obj: tmyobj;
begin
	obj := tmyobj.create;
	if test2(obj.doclick, obj.doclick) then
		writeln('success')
	else
		writeln('failed');
	obj.free;
	if test3(tmyobj.create) then
		writeln('success')
	else
		writeln('failed');
end.