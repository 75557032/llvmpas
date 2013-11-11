unit ptrhashtable;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses SysUtils;

const
  MaxHashListSize = Maxint div 16;
  SListCapacityError = 'The maximum list capacity is reached (%d)';
  SListIndexOutOfBound = 'The index %d out of bound'; 
type
  PPPtrHashItem = ^PPtrHashItem;
  PPtrHashItem = ^TPtrHashItem;
  TPtrHashItem = record
    Next: PPtrHashItem;
    HashCode: Cardinal;
    Key: Pointer;
    Value: Pointer;
  end;

  EHashListError = class(Exception);

  TCustomPtrHashTable = class
  private
    Buckets: array of PPtrHashItem;
    FCount: Integer;
    FItems: array of PPtrHashItem;
  //  FItems: array of PPtrHashItem;    // order list
    FCapacity: Integer;
    procedure Expand;
    procedure SetCapacity(const Value: Integer);
    function GetKey(Index: Integer): Pointer;
  protected
    function Lookup(Key: Pointer): PPPtrHashItem; overload;
    function Lookup(Key: Pointer; HashCode: Cardinal): PPPtrHashItem; overload;

    function HashOf(Key: Pointer): Cardinal; virtual;
    function Get(Key: Pointer): Pointer; overload;
    function Get(Key: Pointer; HashCode: Cardinal): Pointer; overload;
    procedure Put(Key: Pointer; Value: Pointer); overload;
    procedure Put(Key: Pointer; HashCode:Cardinal; Value: Pointer); overload;
    function Modify(Key: Pointer; Value: Pointer): Boolean;
    procedure Rehash;
    function ValueByIndex(Index: Integer): Pointer;
  public
    constructor Create(Size: Integer = 256);
    destructor Destroy; override;
    class procedure Error(const Msg: string; Data: Integer);
    // 清空,但不释放已经分配的Buckets和FItems数组
    procedure Clear;
    // 删除Key
    procedure Remove(Key: Pointer);
    // 确保已有容量不小于Count + IncSize
    procedure EnsureCapacity(IncSize: Integer);
    // 判断是否存在指定的Key
    function IsExists(Key: Pointer): Boolean; overload;
    // 判断是否存在指定的Key和HashCode组合, 提供HashCode是为了避免再次调用HashOf
    function IsExists(Key: Pointer; HashCode: Cardinal): Boolean; overload;
    // 返回Value的位置
    function IndexOf(Value: Pointer): Integer;
    // Buckets的容量
    property Capacity: Integer read FCapacity write SetCapacity;
    // 当前已经加入的数目
    property Count: Integer read FCount;
    // 取Key
    property Keys[Index: Integer]: Pointer read GetKey;

    // 计算某些性能信息
    function Statistics: string;
  end;

  TPtrHashTable = class(TCustomPtrHashTable)
  public
    procedure Put(Key: Pointer; Value: Pointer);
    function Get(Key: Pointer): Pointer;
    function Add(Key: Pointer; Value: Pointer): Boolean;
  end;

{
  THashItemManager = class
  private
    FItems: array of PPtrHashItem;
    FFrees: TList;
  public
    procedure Allocate(Count: Integer);
    function Get: PHashItem;
  end; }

implementation

{ TCustomPtrHashTable }

procedure TCustomPtrHashTable.Clear;
var
	I: Integer;
{	P, N: PPtrHashItem;}
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

constructor TCustomPtrHashTable.Create(Size: Integer);
begin
	inherited Create;
  if Size > 0 then
  begin
    SetLength(Buckets, Size);
    SetLength(FItems, Size);
    FCapacity := Size;
  end;
end;

destructor TCustomPtrHashTable.Destroy;
begin
	Clear;
	inherited;
end;

procedure TCustomPtrHashTable.EnsureCapacity(IncSize: Integer);
begin
  if IncSize > 0 then
    if IncSize + Count > Capacity then
      Capacity := IncSize + Count;
end;

class procedure TCustomPtrHashTable.Error(const Msg: string; Data: Integer);
begin
  raise EHashListError.CreateFmt(Msg, [Data]);
end;

procedure TCustomPtrHashTable.Expand;
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

function TCustomPtrHashTable.Get(Key: Pointer): Pointer;
var
	P: PPtrHashItem;
begin
	P := Lookup(Key)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TCustomPtrHashTable.Get(Key: Pointer;
  HashCode: Cardinal): Pointer;
var
	P: PPtrHashItem;
begin
	P := Lookup(Key, HashCode)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TCustomPtrHashTable.GetKey(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Key;
end;

{function TCustomPtrHashTable.HashOf(Key: Pointer): Cardinal;
var
	I: Integer;
  p: PByte;
begin
  p := @Key;
	Result := 0;
  for I := 0 to SizeOf(Pointer) - 1 do
  begin
    Result := Cardinal(Integer(Result shl 5) - Integer(Result)) xor LongWord(p^);
    Inc(p);
  end;
end; }

function TCustomPtrHashTable.HashOf(Key: Pointer): Cardinal;
begin
{$IFDEF FPC}
  Result := SizeInt(Key);
{$ELSE}
  Result := Cardinal(Key);
{$ENDIF}
  Result := Result + not (Result shl 9);
  Result := Result xor (Result shr 14);
  Result := Result + (Result shl 4);
  Result := Result xor (Result shr 10);
end;

function TCustomPtrHashTable.IsExists(Key: Pointer): Boolean;
begin
  Result := Lookup(Key)^ <> nil;
end;

function TCustomPtrHashTable.IndexOf(Value: Pointer): Integer;
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

function TCustomPtrHashTable.IsExists(Key: Pointer;
  HashCode: Cardinal): Boolean;
begin
  Result := Lookup(Key, HashCode)^ <> nil;
end;

function TCustomPtrHashTable.Lookup(Key: Pointer): PPPtrHashItem;
begin
  Result := Lookup(Key, HashOf(Key));
end;

function TCustomPtrHashTable.Lookup(Key: Pointer;
  HashCode: Cardinal): PPPtrHashItem;
const
  BlankBucket: PPtrHashItem = nil;
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
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
	end;
end;

function TCustomPtrHashTable.Modify(Key: Pointer; Value: Pointer): Boolean;
var
	P: PPtrHashItem;
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

procedure TCustomPtrHashTable.Put(Key: Pointer; Value: Pointer);
var
	bucketIndex: Integer;
  HashCode: Cardinal;
	Bucket: PPtrHashItem;
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

procedure TCustomPtrHashTable.Put(Key: Pointer; HashCode: Cardinal;
  Value: Pointer);
var
	bucketIndex: Integer;
	Bucket: PPtrHashItem;
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

procedure TCustomPtrHashTable.Rehash;
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

procedure TCustomPtrHashTable.Remove(Key: Pointer);

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
	P: PPtrHashItem;
	Prev: PPPtrHashItem;
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

procedure TCustomPtrHashTable.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxHashListSize) then
    Error(SListCapacityError, Value);

  SetLength(FItems, Value);
  SetLength(Buckets, Value);
  FCapacity := Value;
  Rehash;
end;

function TCustomPtrHashTable.Statistics: string;
var
  Bucket: PPtrHashItem;
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

function TCustomPtrHashTable.ValueByIndex(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Value;
end;

{ TPtrHashTable }

function TPtrHashTable.Add(Key: Pointer; Value: Pointer): Boolean;
var
  hc: Cardinal;
begin
  hc := HashOf(Key);
  Result := inherited IsExists(Key, hc);
  if not Result then
    inherited Put(Key, hc, Value);
end;

function TPtrHashTable.Get(Key: Pointer): Pointer;
begin
  Result := inherited Get(Key);
end;

procedure TPtrHashTable.Put(Key: Pointer; Value: Pointer);
begin
  inherited Put(Key, Value);
end;

end.

