unit overloadchk1;

interface
{ 检查overload}
implementation

function aaa: Integer; overload;
begin
	result := 0;
end;

function aaa(a:integer):Integer; overload;
begin
end;

procedure test2;
var
  p: function:Integer;
  p2: function(a:Integer): Integer;
  p3: pointer;
begin
  p := @aaa;
  p2 := @aaa;
  //p2 := @test2;
  p3 := @test2;
  p3 := @aaa;
//  showmessagefmt('%p,%p', [@p, @p2]);
//  showmessagefmt('%p,%p', [@aaa, @aaa]);
end;

end.