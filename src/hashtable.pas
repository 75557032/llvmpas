unit hashtable;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
   // inifiles
uses SysUtils;

const
  MaxHashListSize = Maxint div 16;
  SListCapacityError = 'The maximum list capacity is reached (%d)';
  SListIndexOutOfBound = 'The index %d out of bound'; 
type
  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    HashCode: Cardinal;
    Key: string;
    Value: Pointer;
  end;

  EHashListError = class(Exception);

  TCustomHashTable = class
  private
    Buckets: array of PHashItem;
    FCount: Integer;
    FItems: array of PHashItem;
  //  FItems: array of PHashItem;    // order list
    FCapacity: Integer;
    FCaseSensitive: Boolean;
    procedure Expand;
    procedure SetCapacity(const Value: Integer);
    function GetKey(Index: Integer): string;
  protected
    function Lookup(const Key: string): PPHashItem; overload;
    function Lookup(const Key: string; HashCode: Cardinal): PPHashItem; overload;

    function HashOf(const Key: string): Cardinal; virtual;
    function Get(const Key: string): Pointer; overload;
    function Get(const Key: string; HashCode: Cardinal): Pointer; overload;
    procedure Put(const Key: string; Value: Pointer); overload;
    procedure Put(const Key: string; HashCode:Cardinal; Value: Pointer); overload;
    function Modify(const Key: string; Value: Pointer): Boolean;
    procedure Rehash;
    function ValueByIndex(Index: Integer): Pointer;
  public
    constructor Create(Size: Integer = 256; CaseSens: Boolean = False);
    destructor Destroy; override;
    class procedure Error(const Msg: string; Data: Integer);
    // 清空,但不释放已经分配的Buckets和FItems数组
    procedure Clear;
    // 删除Key
    procedure Remove(const Key: string);
    // 确保已有容量不小于Count + IncSize
    procedure EnsureCapacity(IncSize: Integer);
    // 判断是否存在指定的Key
    function IsExists(const Key: string): Boolean; overload;
    // 判断是否存在指定的Key和HashCode组合, 提供HashCode是为了避免再次调用HashOf
    function IsExists(const Key: string; HashCode: Cardinal): Boolean; overload;
    // 返回Value的位置
    function IndexOf(Value: Pointer): Integer;
    // Buckets的容量
    property Capacity: Integer read FCapacity write SetCapacity;
    // 当前已经加入的数目
    property Count: Integer read FCount;
    // 取Key
    property Keys[Index: Integer]: string read GetKey;
    // 比较Key的方式, True: 区分大小写, False: 不区分大小写
    property CaseSensitive: Boolean read FCaseSensitive;

    // 计算某些性能信息
    function Statistics: string;
  end;

  THashTable = class(TCustomHashTable)
  public
    procedure Put(const Key: string; Value: Pointer);
    function Get(const Key: string): Pointer;
    function Add(const Key: string; Value: Pointer): Boolean;
  end;

  THashObjectList = class(TCustomHashTable)
  public
    procedure Put(const Key: string; Value: TObject);
    function Get(const Key: string): TObject;
  end;
{
  THashItemManager = class
  private
    FItems: array of PHashItem;
    FFrees: TList;
  public
    procedure Allocate(Count: Integer);
    function Get: PHashItem;
  end; }

implementation

{ TCustomHashTable }

procedure TCustomHashTable.Clear;
var
	I: Integer;
{	P, N: PHashItem;}
begin
{	for I := 0 to Length(Buckets) - 1 do
	begin
		P := Buckets[I];
		while P <> nil do
		begin
			N := P^.Next;
			Dispose(P);
			P := N;
		end;
		Buckets[I] := nil;
	end;}
  for I := 0 to FCount - 1 do
    Dispose(FItems[I]);
  FCount := 0;
  FillChar(FItems[0], Length(FItems) * SizeOf(Pointer), 0);
  FillChar(Buckets[0], Length(Buckets) * SizeOf(Pointer), 0);
end;

constructor TCustomHashTable.Create(Size: Integer; CaseSens: Boolean);
begin
	inherited Create;
  FCaseSensitive := CaseSens;
  if Size > 0 then
  begin
    SetLength(Buckets, Size);
    SetLength(FItems, Size);
    FCapacity := Size;
  end;
end;

destructor TCustomHashTable.Destroy;
begin
	Clear;
	inherited;
end;

procedure TCustomHashTable.EnsureCapacity(IncSize: Integer);
begin
  if IncSize > 0 then
    if IncSize + Count > Capacity then
      Capacity := IncSize + Count;
end;

class procedure TCustomHashTable.Error(const Msg: string; Data: Integer);
begin
  raise EHashListError.CreateFmt(Msg, [Data]);
end;

procedure TCustomHashTable.Expand;
var
  Delta: Integer;
begin
  if FCapacity < 64 then
    Delta := 16
  else if FCapacity < 256 then
    Delta := 64
  else if FCapacity < 1024 then
    Delta := 128
  else
    Delta := 256;
  SetCapacity(FCapacity + Delta);
end;

function TCustomHashTable.Get(const Key: string): Pointer;
var
	P: PHashItem;
begin
	P := Lookup(Key)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TCustomHashTable.Get(const Key: string;
  HashCode: Cardinal): Pointer;
var
	P: PHashItem;
begin
	P := Lookup(Key, HashCode)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TCustomHashTable.GetKey(Index: Integer): string;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Key;
end;

function TCustomHashTable.HashOf(const Key: string): Cardinal;
var
	I: Integer;
begin
	Result := 0;
  if FCaseSensitive then
  begin
	  for I := 1 to Length(Key) do
      Result := Cardinal(Integer(Result shl 5) - Integer(Result)) xor LongWord(Ord(Key[I]));
//		  Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[I]);
  end
  else
  begin
	  for I := 1 to Length(Key) do
      Result := Cardinal(Integer(Result shl 5) - Integer(Result)) xor LongWord(Ord(Upcase(Key[I])));
//		  Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Upcase(Key[I]));
  end;
end;

function TCustomHashTable.IsExists(const Key: string): Boolean;
begin
  Result := Lookup(Key)^ <> nil;
end;

function TCustomHashTable.IndexOf(Value: Pointer): Integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if FItems[i] = Value then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TCustomHashTable.IsExists(const Key: string;
  HashCode: Cardinal): Boolean;
begin
  Result := Lookup(Key, HashCode)^ <> nil;
end;

function TCustomHashTable.Lookup(const Key: string): PPHashItem;
begin
  Result := Lookup(Key, HashOf(Key));
end;

function TCustomHashTable.Lookup(const Key: string;
  HashCode: Cardinal): PPHashItem;
const
  BlankBucket: PHashItem = nil;
var
	Hash: Integer;
begin
  if FCapacity = 0 then
  begin
    Result := @BlankBucket;
    Exit;
  end;

	Hash := HashCode mod Cardinal(Length(Buckets));
//	Hash := HashCode mod Cardinal(FCapacity);
	Result := @Buckets[Hash];
	while Result^ <> nil do
	begin
    if FCaseSensitive then
    begin
      if Result^.Key = Key then
        Exit
      else
        Result := @Result^.Next;
    end
    else
    begin
      if SameText(Result^.Key, Key) then
        Exit
      else
        Result := @Result^.Next;
    end;
	end;
end;

function TCustomHashTable.Modify(const Key: string; Value: Pointer): Boolean;
var
	P: PHashItem;
begin
	P := Lookup(Key)^;
	if P <> nil then
	begin
		Result := True;
		P^.Value := Value;
	end
	else
		Result := False;
end;

procedure TCustomHashTable.Put(const Key: string; Value: Pointer);
var
	bucketIndex: Integer;
  HashCode: Cardinal;
	Bucket: PHashItem;
begin
  if FCount >= FCapacity then Expand;

  HashCode := HashOf(Key);
	bucketIndex := HashCode mod Cardinal(Length(Buckets));
	New(Bucket);
  FItems[FCount] := Bucket;
  Inc(FCount);

	Bucket^.Key := Key;
	Bucket^.Value := Value;
  Bucket^.HashCode := HashCode;
	Bucket^.Next := Buckets[bucketIndex];
	Buckets[bucketIndex] := Bucket;
end;

procedure TCustomHashTable.Put(const Key: string; HashCode: Cardinal;
  Value: Pointer);
var
	bucketIndex: Integer;
	Bucket: PHashItem;
begin
  if FCount >= FCapacity then Expand;

	bucketIndex := HashCode mod Cardinal(Length(Buckets));
	New(Bucket);
  FItems[FCount] := Bucket;
  Inc(FCount);

	Bucket^.Key := Key;
	Bucket^.Value := Value;
  Bucket^.HashCode := HashCode;
	Bucket^.Next := Buckets[bucketIndex];
	Buckets[bucketIndex] := Bucket;
end;

procedure TCustomHashTable.Rehash;
var
  i: Integer;
  BucketSize: Cardinal;
  BucketIndex: Integer;
begin
  FillChar(Buckets[0], Length(Buckets) * SizeOf(Pointer), 0);
  BucketSize := Cardinal(Length(Buckets));
  for i := 0 to FCount - 1 do
  begin
    with FItems[i]^ do
    begin
      BucketIndex := HashCode mod BucketSize;
  	  Next := Buckets[BucketIndex];
	    Buckets[BucketIndex] := FItems[i];
    end;
  end;
end;

procedure TCustomHashTable.Remove(const Key: string);

  procedure RemoveItem(P: Pointer);
  var
    i: Integer;
  begin
    for i := 0 to FCount - 1 do
      if FItems[i] = P then
      begin
        Dec(FCount);
        if i < FCount then
          Move(FItems[i + 1], FItems[i], (FCount - i) * SizeOf(Pointer));
        Exit;
      end;
  end;
var
	P: PHashItem;
	Prev: PPHashItem;
begin
	Prev := Lookup(Key);
	P := Prev^;
	if P <> nil then
	begin
		Prev^ := P^.Next;
    RemoveItem(P);
		Dispose(P);
	end;
end;

procedure TCustomHashTable.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxHashListSize) then
    Error(SListCapacityError, Value);

  SetLength(FItems, Value);
  SetLength(Buckets, Value);
  FCapacity := Value;
  Rehash;
end;

function TCustomHashTable.Statistics: string;
var
  Bucket: PhashItem;
  Item: array[1..10] of Integer;
  I, j: Integer;
  Conflict: Integer;
  HashMean,
  HashStdDev : Double;
begin
  for I := Low(Item) to High(Item) do
    Item[I] := 0;
  Conflict := 0;
  HashMean := 0;
  HashStdDev := 0;
  for I := 0 to Length(Buckets) - 1 do
  begin
    Bucket := Buckets[I];
    if (Bucket <> nil) and (Bucket^.Next <> nil) then
    begin
      Inc(Conflict);
      j := 0;
      while Bucket^.Next <> nil do
      begin
        Inc(j);
        Bucket := Bucket^.next;
      end;
      if j < 10 then
        Inc(Item[j])
      else
        Inc(Item[10]);
      HashMean := HashMean + j;
      HashStdDev := HashStdDev + Sqr(j);
    end;
  end;
  HashMean := HashMean / Self.FCapacity;
  HashStdDev := (HashStdDev - FCapacity * Sqr(HashMean));
  if FCapacity > 1 then
    HashStdDev := Sqrt(HashStdDev / (FCapacity-1))
  else
    HashStdDev := 0;
  Result := Result + 'HashMean: ' + FloatToStr(HashMean);
  Result := result + ', HashStdDev: ' + FloatToStr(HashStdDev);
  Result := Result + ', Conflict bucket: ' + IntToStr(Conflict);
  for I := Low(Item) to High(Item) do
    if Item[I] > 0 then
      Result := Result + Format(', %d: %d', [I, Item[I]]);
end;

function TCustomHashTable.ValueByIndex(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Value;
end;

{ THashObjectList }

function THashObjectList.Get(const Key: string): TObject;
begin
  Result := TObject(inherited Get(Key));
end;

procedure THashObjectList.Put(const Key: string; Value: TObject);
begin
  inherited Put(Key, Value);
end;

{ THashTable }

function THashTable.Add(const Key: string; Value: Pointer): Boolean;
var
  hc: Cardinal;
begin
  hc := HashOf(Key);
  Result := inherited IsExists(Key, hc);
  if not Result then
    inherited Put(Key, hc, Value);
end;

function THashTable.Get(const Key: string): Pointer;
begin
  Result := inherited Get(Key);
end;

procedure THashTable.Put(const Key: string; Value: Pointer);
begin
  inherited Put(Key, Value);
end;

end.

