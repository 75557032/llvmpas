unit hashtable;
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
  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    HashCode: Cardinal;
    Key: Pointer;
    Value: Pointer;
  end;

  EHashListError = class(Exception);

  THashTablePosition = Pointer;

  { TBaseHashTable }

  TBaseHashTable = class
  private
    Buckets: array of PHashItem;
    FCount: Integer;
    FItems: array of PHashItem;
    FCapacity: Integer;
    procedure Expand;
    procedure SetCapacity(const Value: Integer);
  protected
    procedure KeyDtor(var Key: Pointer); virtual;
    function KeyEquals(Key1, Key2: Pointer): Boolean; virtual; abstract;
    function HashOf(const Key: Pointer): Cardinal; virtual; abstract;
    function Lookup(const Key: Pointer): PPHashItem; overload;
    function Lookup(const Key: Pointer; HashCode: Cardinal): PPHashItem; overload;

    function Get(const Key: Pointer): Pointer; overload;
    function Get(const Key: Pointer; HashCode: Cardinal): Pointer; overload;
    procedure Put(const Key: Pointer; Value: Pointer); overload;
    procedure Put(const Key: Pointer; HashCode: Cardinal; Value: Pointer); overload;
    function Modify(const Key: Pointer; Value: Pointer): Boolean;
    procedure Rehash;
    function ValueByIndex(Index: Integer): Pointer;
    // 删除Key
    procedure Remove(const Key: Pointer);
    // 判断是否存在指定的Key
    function IsExists(const Key: Pointer): Boolean; overload;
    // 判断是否存在指定的Key和HashCode组合, 提供HashCode是为了避免再次调用HashOf
    function IsExists(const Key: Pointer; HashCode: Cardinal): Boolean; overload;
    // 查找第一个Key
    function FindFirst(const Key: Pointer): THashTablePosition;
    // 查找下一个Key
    function FindNext(var Pos: THashTablePosition): Pointer;
  public
    constructor Create(Size: Integer = 256);
    destructor Destroy; override;
    class procedure Error(const Msg: string; Data: Integer);
    // 清空,但不释放已经分配的Buckets和FItems数组
    procedure Clear;
    // 确保已有容量不小于Count + IncSize
    procedure EnsureCapacity(IncSize: Integer);
    // 返回Value的位置
    function IndexOf(Value: Pointer): Integer;
    // Buckets的容量
    property Capacity: Integer read FCapacity write SetCapacity;
    // 当前已经加入的数目
    property Count: Integer read FCount;

    // 计算某些性能信息
    function Statistics: string;
  end;

  { TCustomHashTable }

  TCustomHashTable = class(TBaseHashTable)
  private
    FCaseSensitive: Boolean;
    function GetItem(Index: Integer): Pointer;
    function GetKey(Index: Integer): string;
  protected
    function HashOf(const Key: Pointer): Cardinal; override;
    function KeyEquals(Key1, Key2: Pointer): Boolean; override;
    procedure KeyDtor(var Key: Pointer); override;
    function HashOfStr(const Key: string): Cardinal;
    procedure PutStr(const Key: string; hc: Cardinal; Value: Pointer);
    function GetStr(const Key: string): Pointer;
  public
    constructor Create(Size: Integer = 256; CaseSens: Boolean = False);
    procedure Remove(const Key: string);
    function IsExists(const Key: string): Boolean; overload;
    function IsExists(const Key: string; HashCode: Cardinal): Boolean; overload;

    property CaseSensitive: Boolean read FCaseSensitive;
    // 取Key
    property Keys[Index: Integer]: string read GetKey;
    // 按索引取
    property Item[Index: Integer]: Pointer read GetItem;
  end;

  { THashTable }

  THashTable = class(TCustomHashTable)
  public
    procedure Put(const Key: string; Value: Pointer);
    function Get(const Key: string): Pointer;
    function FindFirst(const Key: string): THashTablePosition;
    function FindNext(var Pos: THashTablePosition): Pointer;
    // 增加映射。如果已经存在返回True, 否则返回False, 不增加重复值
    function Add(const Key: string; Value: Pointer): Boolean;
  end;

  THashObjectList = class(THashTable)
  public
    procedure Put(const Key: string; Value: TObject);
    function Get(const Key: string): TObject;
  end;

  { TPtrHashTable }

  TPtrHashTable = class(TBaseHashTable)
  private
    function GetKey(Index: Integer): Pointer;
  protected
    function HashOf(const Key: Pointer): Cardinal; override;
    function KeyEquals(Key1, Key2: Pointer): Boolean; override;
    procedure KeyDtor(var Key: Pointer); override;
  public
    // 增加映射, 如果已经存在返回True, 否则返回False
    function Add(Key: Pointer; Value: Pointer): Boolean;
    procedure Remove(const Key: Pointer);
    function IsExists(const Key: Pointer): Boolean; overload;
    function IsExists(const Key: Pointer; HashCode: Cardinal): Boolean; overload;
    // 取Key
    property Keys[Index: Integer]: Pointer read GetKey;
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

{ TBaseHashTable }

procedure TBaseHashTable.Clear;
var
	I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    KeyDtor(FItems[I].Key);
    Dispose(FItems[I]);
  end;
  FCount := 0;
  FillChar(FItems[0], Length(FItems) * SizeOf(Pointer), 0);
  FillChar(Buckets[0], Length(Buckets) * SizeOf(Pointer), 0);
end;

constructor TBaseHashTable.Create(Size: Integer);
begin
	inherited Create;
  if Size > 0 then
  begin
    SetLength(Buckets, Size);
    SetLength(FItems, Size);
    FCapacity := Size;
  end;
end;

destructor TBaseHashTable.Destroy;
begin
	Clear;
	inherited;
end;

procedure TBaseHashTable.EnsureCapacity(IncSize: Integer);
begin
  if IncSize > 0 then
    if IncSize + Count > Capacity then
      Capacity := IncSize + Count;
end;

class procedure TBaseHashTable.Error(const Msg: string; Data: Integer);
begin
  raise EHashListError.CreateFmt(Msg, [Data]);
end;

procedure TBaseHashTable.Expand;
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

function TBaseHashTable.FindFirst(const Key: Pointer): THashTablePosition;
begin
  Result := Lookup(Key);
  if PPHashItem(Result)^ = nil then Result := nil;
end;

function TBaseHashTable.FindNext(var Pos: THashTablePosition): Pointer;
var
  Item, Old: PPHashItem;
begin
  Item := Pos;

  if not Assigned(Item) or not Assigned(Item^) then
  begin
    Result := nil;
    Exit;
  end
  else
    Result := Item^.Value;

  // Search next
  Old := Item;
  Item := @Item^.Next;
	while Item^ <> nil do
	begin
    if KeyEquals(Item^.Key, Old^.Key) then
       Break
    else
      Item := @Item^.Next;
	end;

  if not Assigned(Item^) then
    Pos := nil
  else
    Pos := Item;
end;

function TBaseHashTable.Get(const Key: Pointer): Pointer;
var
	P: PHashItem;
begin
	P := Lookup(Key)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TBaseHashTable.Get(const Key: Pointer;
  HashCode: Cardinal): Pointer;
var
	P: PHashItem;
begin
	P := Lookup(Key, HashCode)^;
	if P <> nil then
		Result := P^.Value else
		Result := nil;
end;

function TBaseHashTable.IndexOf(Value: Pointer): Integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if PHashItem(FItems[i]).Value = Value then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

function TBaseHashTable.IsExists(const Key: Pointer): Boolean;
begin
  Result := Lookup(Key)^ <> nil;
end;

function TBaseHashTable.IsExists(const Key: Pointer;
  HashCode: Cardinal): Boolean;
begin
  Result := Lookup(Key, HashCode)^ <> nil;
end;

procedure TBaseHashTable.KeyDtor(var Key: Pointer);
begin
  Key := nil;
end;

function TBaseHashTable.Lookup(const Key: Pointer): PPHashItem;
begin
  Result := Lookup(Key, HashOf(Key));
end;

function TBaseHashTable.Lookup(const Key: Pointer;
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
    if Self.KeyEquals(Result^.Key, Key) then
       Exit
    else
      Result := @Result^.Next;
	end;
end;

function TBaseHashTable.Modify(const Key: Pointer; Value: Pointer): Boolean;
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

procedure TBaseHashTable.Put(const Key: Pointer; Value: Pointer);
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

procedure TBaseHashTable.Put(const Key: Pointer; HashCode: Cardinal;
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

procedure TBaseHashTable.Rehash;
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

procedure TBaseHashTable.Remove(const Key: Pointer);

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
    KeyDtor(P^.Key);
		Dispose(P);
	end;
end;

procedure TBaseHashTable.SetCapacity(const Value: Integer);
begin
  if (Value < FCount) or (Value > MaxHashListSize) then
    Error(SListCapacityError, Value);

  SetLength(FItems, Value);
  SetLength(Buckets, Value);
  FCapacity := Value;
  Rehash;
end;

function TBaseHashTable.Statistics: string;
var
  Bucket: PhashItem;
  Item: array[1..10] of Integer;
  I, j: Integer;
  Conflict: Integer;
  HashMean,
  HashStdDev : Double;
begin
  Result := '';
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

function TBaseHashTable.ValueByIndex(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Value;
end;

{ TCustomHashTable }

constructor TCustomHashTable.Create(Size: Integer; CaseSens: Boolean);
begin
  inherited Create(Size);
  FCaseSensitive := CaseSens;
end;

function TCustomHashTable.GetItem(Index: Integer): Pointer;
begin
  Result := inherited ValueByIndex(Index);
end;

function TCustomHashTable.GetKey(Index: Integer): string;
begin
  if (Index < 0) or (Index > Count) then
    Error(SListIndexOutOfBound, Index);
  Result := string(FItems[Index]^.Key);
end;

function TCustomHashTable.GetStr(const Key: string): Pointer;
begin
  Result := inherited Get(PChar(Key));
end;

function TCustomHashTable.HashOf(const Key: Pointer): Cardinal;
var
	I: Integer;
begin
	Result := 0;
  if FCaseSensitive then
  begin
	  for I := 1 to Length(string(Key)) do
      Result := Cardinal(Integer(Result shl 5) - Integer(Result)) xor LongWord(Ord(string(Key)[I]));
//		  Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[I]);
  end
  else
  begin
	  for I := 1 to Length(string(Key)) do
      Result := Cardinal(Integer(Result shl 5) - Integer(Result)) xor LongWord(Ord(Upcase(string(Key)[I])));
//		  Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Upcase(Key[I]));
  end;
end;

function TCustomHashTable.HashOfStr(const Key: string): Cardinal;
begin
  Result := HashOf(PChar(Key));
end;

function TCustomHashTable.IsExists(const Key: string): Boolean;
begin
  Result := inherited IsExists(PChar(Key));
end;

function TCustomHashTable.IsExists(const Key: string; HashCode: Cardinal
  ): Boolean;
begin
  Result := inherited IsExists(PChar(Key), HashCode);
end;

procedure TCustomHashTable.KeyDtor(var Key: Pointer);
begin
  string(Key) := '';
end;

function TCustomHashTable.KeyEquals(Key1, Key2: Pointer): Boolean;
begin
  if FCaseSensitive then
    Result := string(Key1) = string(Key2)
  else
    Result := SameText(string(Key1), string(Key2));
end;

procedure TCustomHashTable.PutStr(const Key: string; hc: Cardinal;
  Value: Pointer);
var
  keyVal: string;
begin
  keyVal := Key;
  inherited Put(PChar(Key), hc, Value);
  PPointer(@keyVal)^ := nil;
end;

procedure TCustomHashTable.Remove(const Key: string);
begin
  inherited Remove(PChar(Key));
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
  keyStr: string;
  keyPtr: Pointer;
begin
  keyStr := Key;
  keyPtr := PChar(Key);
  hc := HashOf(keyPtr);
  Result := inherited IsExists(keyPtr, hc);
  if not Result then
  begin
    inherited Put(keyPtr, hc, Value);
    PPointer(@keyStr)^ := nil;  // prevent from string release
  end;
end;

function THashTable.FindFirst(const Key: string): THashTablePosition;
begin
  Result := inherited FindFirst(PChar(Key));
end;

function THashTable.FindNext(var Pos: THashTablePosition): Pointer;
begin
  Result := inherited FindNext(Pos);
end;

function THashTable.Get(const Key: string): Pointer;
begin
  Result := inherited Get(PChar(Key));
end;

procedure THashTable.Put(const Key: string; Value: Pointer);
var
  KeyVal: string;
begin
  KeyVal := Key;
  inherited Put(PChar(KeyVal), Value);
  PPointer(@KeyVal)^ := nil; // 阻止释放
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

function TPtrHashTable.GetKey(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexOutOfBound, Index);
  Result := FItems[Index]^.Key;
end;

function TPtrHashTable.HashOf(const Key: Pointer): Cardinal;
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

function TPtrHashTable.IsExists(const Key: Pointer): Boolean;
begin
  Result := inherited IsExists(Key);
end;

function TPtrHashTable.IsExists(const Key: Pointer; HashCode: Cardinal
  ): Boolean;
begin
  Result := inherited IsExists(Key, HashCode);
end;

procedure TPtrHashTable.KeyDtor(var Key: Pointer);
begin
  //
end;

function TPtrHashTable.KeyEquals(Key1, Key2: Pointer): Boolean;
begin
  Result := Key1 = Key2;
end;

procedure TPtrHashTable.Remove(const Key: Pointer);
begin

end;

end.

