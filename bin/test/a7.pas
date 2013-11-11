unit a7;

{ test array property or array field}
interface
type
	tproc = procedure;

	TMyData = class
	private
		function Get(Index: Integer): string;
		procedure Put(Index: Integer; const S: string);
		function GetValue(Index: Integer): string;
	public
		property Item[Index: Integer]: string read Get write Put; default;
		property Values[Index: Integer]: string read GetValue;
	end;

	TMyInfo = class
	private
		fdata: TMyData;
		function getarr(index: Integer): TMyData;
		function getprocs(index: Integer): tproc;
	public
		farr: array[0..1] of TMyData;
		fprocs: array[0..1] of tproc;
		property arr[index: Integer]: tmydata read getarr;
		property data: TMyData read fdata;
		property procs[index: Integer]: tproc read getprocs;
	end;
	
implementation

function TMyData.Get(Index: Integer): string;
begin
end;

procedure TMyData.Put(Index: Integer; const S: string);
begin
end;

function TMyData.GetValue(Index: Integer): string;
begin
end;

function TMyInfo.getarr(index: Integer): TMyData;
begin
end;

function TMyInfo.getprocs(index: Integer): tproc;
begin
end;

procedure Test2;
	procedure dotest(s: string);
	begin
		
	end;
type
	myid = (aa,bb,cc);
var
  rec: record
  end platform;
type
	mrec = record
	end deprecated;
var
	info: tmyinfo;
	a: array[myid] of string;
	i: smallint;
begin
//	dotest(a[bb]);
	info.procs[1];
	dotest(info.arr[0][0]);
	info.arr[0][0] := 'a';
//	info.data[0] := 'a';
//	dotest(info.data[0]);
end;
{
procedure Test;
var
	obj: TMydata;
begin
	obj[1] := '2';
	obj.Values[1] := '3';
end;}

end.