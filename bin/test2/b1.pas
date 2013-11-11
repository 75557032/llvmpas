unit b1;

interface
uses a, b;

type
	TMyRec = record
		a: Integer;
	end;
	{
	TMyObj = object
		i: Integer;
	end;
	}
	myenum = (aaa,bbb,ccc);
	mysub = 5..6;
	myproc1 = procedure ;
	myproc2 = function : string;
	//myproc3 = procedure (a:string; b: array of string);
	myproc4 = function (a: integer; b: array of const): string;
	myproc5 = function (s: string): word;
	
var
	v1: byte = 5;
	v2: string = 'aaaa';
	v3: unicodestring = 'fsksjd';
	//v3: shortstring = 'aa';
	
	v4: char = 'a';
	v5: shortstring = 'shortstr';
implementation
uses c;

end.