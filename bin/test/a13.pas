unit a13;

interface
{ 测试case 语句 }
implementation

procedure test;
var
	a: Integer;
begin
	case a of
		1,3: begin
		end;
	end;
	case 'a' of
		'a': begin
		end;
	else
		;
	end;
	
	case 'a' of
		'a','b':;
    'b':
	else
	end;

end;

end.