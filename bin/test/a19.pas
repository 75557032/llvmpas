unit a19;

// 测试object的size和字段offset
interface

type
	mycls = class
	end;
	
	mycls2 = class(mycls)
		a: byte;
	end;
	
	mycls3 = class(mycls2)
		b: Integer;
	end;
	
	myobj = object
	private
		a: byte;
	end;
	
	myobj2 = object(myobj)
	private
		b: char;
	public
		procedure show; virtual;
	end;
	
	myobj3 = object(myobj2)
	private
		c: char;
	end;
	
implementation

procedure myobj2.show; 
begin
end;

end.