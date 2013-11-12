unit cupersist;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast, cntx;

type
  ECUError = class(Exception);
  ECUWriteError = class(ECUError);
  ECUReadError = class(ECUError);

  TModuleHeader = record
    Version: LongWord;
    UnitName: string;
    TimeStamp: TFileTimeStamp;
  end;

  TCUWriter = class
  private
    FHandle: Integer;
    FExternalSymbols: TStringList; // dupIgnore, sort
    FModule: TModule;

    procedure Write(const Buffer; Size: Cardinal);
    procedure WriteBuf(const S: AnsiString);
    procedure WriteStr(const S: AnsiString);
    procedure WriteChar(C: AnsiChar);
    procedure WriteByte(b: Byte);
    procedure WriteUInt32(i: Cardinal);
    procedure WriteSInt32(i: Integer);
    procedure WriteSInt64(i: Int64);
    procedure WriteId(const s: string);
    procedure WriteBool(Value: Boolean);

    procedure WriteSymbol(Sym: TSymbol);
    procedure WriteType(Typ: TType);
    procedure WriteRef(Ref: TSymbol);
    procedure WriteValue(const V: TValueRec);
    procedure GetAllExternalSymbols(M: TModule);
  public
    procedure WriteModule(const M: TModule; const UnitFile: string);
    constructor Create; 
    destructor Destroy; override;
  end;

  TFixupKind = (fkAddr, fkRangeBaseType);

  TFixup = class
  public
    Name: string;
    Inst: TSymbol;
    Ref: Pointer;
    Kind: TFixupKind;
    AllowNull: Boolean;
    ExpectNodes: TAstNodeKinds;
    ExpectTypes: TTypeCodes;
  end;

  TCUReader = class
  private
    fStream: TStream;
    FExternalSymbols: TStringList;// not sort
    FFixups: TList;
    FModule: TModule;
    FCntx: TCompileContext;
    FPointerSize: Byte;
    procedure FmtError(const s: string = '');
    procedure FmtErrorFmt(const s: string; const Args: array of const);
    function ReadBuf(Len: Integer): AnsiString;
    function ReadStr: AnsiString;
    function ReadSInt32: Integer;
    function ReadUInt32: Cardinal;
    function ReadSInt64: Int64;
    function ReadChar: AnsiChar;
    function ReadByte: Byte;
    function ReadBool: Boolean;
    function GetSymbol: TSymbol;
    function GetType: TType;
    function FindSymbol(const s: string): TSymbol;
    procedure GetRef(Inst: TSymbol; Ref: Pointer; Kind: TFixupKind;
        ExpectNodes: TAstNodeKinds; ExpectTypes: TTypeCodes;
        AllowNull: Boolean = True);
    procedure ResolveSym(const s: string; AddFixup: Boolean;
        Inst: TSymbol; Ref: Pointer; Kind: TFixupKind;
        ExpectNodes: TAstNodeKinds; ExpectTypes: TTypeCodes;
        AllowNull: Boolean);
    procedure ResolveRef(Sym: TSymbol; Inst: TSymbol; Ref: Pointer; Kind: TFixupKind;
        ExpectNodes: TAstNodeKinds; ExpectTypes: TTypeCodes;
        AllowNull: Boolean);
    procedure GetValue(var V: TValueRec);
    procedure Read(var Buffer; Size: Cardinal);
    procedure ClearFixups;
    procedure FixupRefs;
    function CreateSymbol(SymClass: TSymbolClass): TSymbol;
    procedure CheckFileType(typ: TType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open(const UnitFile: string);
    procedure Close;
    procedure GetHeader(out Header: TModuleHeader);
    procedure ReadModule(M: TModule; Cntx: TCompileContext);
  end;

implementation

type
  TByteData = array[0..10] of Byte;
  PByteData = ^TByteData;

procedure EncodeUInt32(value: Cardinal; out buf: TByteData; out len: Integer);
begin
  len := 0;
  while value >= $80 do
  begin
    buf[len] := Byte(value or $80);
    value := value shr 7;
    Inc(len);
  end;
  buf[len] := value;
  Inc(len);
end;

procedure EncodeSInt32(value: Integer; out buf: TByteData; out len: Integer);
var
  endVal, signExt: Integer;
  b: Byte;
  hasMore: Boolean;
begin
  hasMore := True;
  if Value < 0 then
  begin
    endVal := -1;
    signExt := Integer($fe000000);
  end
  else
  begin
    endVal := 0;
    signExt := 0;
  end;
  len := 0;
  while hasMore do
  begin
    b := Byte(value) and $7f;
    value := (value shr 7) or signExt;  // pascal的shr是不带符号移位的，因此要手工补上
    // 另一种写法：
    // hasMore := !((value == 0 && sign bit of byte is clear) || (value == -1 && sign bit of byte is set))
    hasMore := (value <> endVal) or ((value and 1) <> ((b shr 6) and 1));
    if hasMore then
      buf[len] := b or $80
    else
      buf[len] := b;
    Inc(len);
  end;
end;

procedure EncodeInt64(value: Int64; out buf: TByteData; out len: Integer);
var
  endVal, signExt: Int64;
  b: Byte;
  hasMore: Boolean;
begin
  hasMore := True;
  if Value < 0 then
  begin
    endVal := -1;
    signExt := $7e00000000000000;
  end
  else
  begin
    endVal := 0;
    signExt := 0;
  end;

  len := 0;
  while hasMore do
  begin
    b := Byte(value) and $7f;
    value := (value shr 7) or signExt;
    hasMore := (value <> endVal) or ((value and 1) <> ((b shr 6) and 1));
    if hasMore then
      buf[len] := b or $80
    else
      buf[len] := b;
    Inc(len);
  end;
end;

(*
procedure EncodeInt(value: Integer; buf: PByteData; out len: Integer);
var
  endVal: Integer;
  remaining: Integer;
  hasMore: Boolean;
begin
  remaining := value shl 7;
  hasMore := True;
  if Value < 0 then
    endVal := -1
  else
    endVal := 0;
  len := 0;
  while hasMore do
  begin
    hasMore := (remaining <> endVal) or ((remaining and 1) <> ((value shr 6) and 1));
    if hasMore then
      buf[len] := Byte(value or $80)
    else
      buf[len] := Byte(value);
    Inc(len);
    value := remaining;
    remaining := remaining shr 7;
  end;
end;  *)

(*
public static void writeUnsignedLeb128(ByteOutput out, int value) {
        int remaining = value >>> 7;

        while (remaining != 0) {
            out.writeByte((byte) ((value & 0x7f) | 0x80));
            value = remaining;
            remaining >>>= 7;
        }

        out.writeByte((byte) (value & 0x7f));
    }
 public static void writeSignedLeb128(ByteOutput out, int value) {
        int remaining = value >> 7;
        boolean hasMore = true;
        int end = ((value & Integer.MIN_VALUE) == 0) ? 0 : -1;

        while (hasMore) {
            hasMore = (remaining != end)
                    || ((remaining & 1) != ((value >> 6) & 1));

            out.writeByte((byte) ((value & 0x7f) | (hasMore ? 0x80 : 0)));
            value = remaining;
            remaining >>= 7;
        }
    }*)

{ TCUWriter }

constructor TCUWriter.Create;
begin
  FHandle := -1;
  FExternalSymbols := TStringList.Create;
  FExternalSymbols.CaseSensitive := False;
  FExternalSymbols.Sorted := True;
  FExternalSymbols.Duplicates := dupIgnore;
end;

destructor TCUWriter.Destroy;
begin
  if FHandle <> -1 then
    FileClose(FHandle);
  FExternalSymbols.Free;
  inherited;
end;

procedure TCUWriter.GetAllExternalSymbols(M: TModule);

  procedure Add(sym: TSymbol);
  var
    Module: TModule;
    s: string;
  begin
    if (sym = nil) or (sym.Name = '') then Exit;

    Module := sym.Module;
    if (Module <> M) and (sym.Name <> '') then
    begin
      s := sym.FullName;
      FExternalSymbols.Add(s);
    end;
  end;

  procedure DoAdd(sym: TSymbol);

    procedure DoAddArgs(List: TList);
    var
      i: Integer;
    begin
      if List = nil then Exit;
      for i := 0 to List.Count - 1 do
        Add(TArgument(List[i]).ArgType);
    end;

    procedure DoAddFunc(Func: TFunctionDecl);
    begin
      while Func <> nil do
      begin
        Add(Func.ReturnType);
        DoAddArgs(Func.Args);
        Func := Func.NextOverload;
      end;
    end;

    procedure DoAddClass(Typ: TClassType);
    var
      i: Integer;
    begin
      Add(Typ.Base);

      if Typ.Interfaces <> nil then
      begin
        for i := 0 to Typ.Interfaces.Count - 1 do
          Add(TInterfaceType(Typ.Interfaces[i]));
      end;

      for i := 0 to Typ.Symbols.Count - 1 do
        DoAdd(Typ.Symbols[i]);
    end;

    procedure DoAddObject(Typ: TObjectType);
    var
      i: Integer;
    begin
      Add(Typ.Base);

      for i := 0 to Typ.Symbols.Count - 1 do
        DoAdd(Typ.Symbols[i]);
    end;

    procedure DoAddIntf(Typ: TInterfaceType);
    var
      i: Integer;
    begin
      Add(Typ.Base);

      for i := 0 to Typ.Symbols.Count - 1 do
        DoAdd(Typ.Symbols[i]);
    end;

    procedure DoAddRecord(typ: TRecordType);
    var
      i: Integer;
    begin
      for i := 0 to Typ.Symbols.Count - 1 do
        DoAdd(Typ.Symbols[i]);
    end;
  begin
    if Sym = nil then Exit;
    case sym.NodeKind of
      nkType:
        case TType(sym).TypeCode of
          typPointer: Add(TPointerType(sym).RefType);
          typFile: begin
            DoAdd(TFileType(sym).ElementType);
            Add(TFileType(sym).ElementType);
          end;
          typProcedural: begin
              Add(TProceduralType(sym).ReturnType);
              DoAddArgs(TProceduralType(sym).Args);
            end;
          typClassRef: Add(TClassRefType(sym).RefType);
          typSubrange: Add(TSubrangeType(sym).BaseType);
          typSet: Add(TSetType(sym).RangeType);
          typArray: begin
              DoAdd(TArrayType(sym).ElementType);
              Add(TArrayType(sym).ElementType);
              DoAdd(TArrayType(sym).Range);
            end;
          typDynamicArray: begin
              DoAdd(TDynamicArrayType(sym).ElementType);
              Add(TDynamicArrayType(sym).ElementType);
            end;
          typClass:
            DoAddClass(TClassType(sym));
          typObject:
            DoAddObject(TObjectType(sym));
          typInterface, typDispInterface:
            DoAddIntf(TInterfaceType(sym));
          typRecord:
            DoAddRecord(TRecordType(sym));
          typAlias, typClonedType:
            Add(TAliasType(sym).RefType);
        end;
      nkVariable: begin
        DoAdd(TVariable(sym).VarType);
        Add(TVariable(sym).VarType);
        Add(TVariable(sym).AbsVar);
      end;
      nkArgument:
        Add(TArgument(sym).ArgType);
      nkConstant: begin
        DoAdd(TConstant(sym).ConstType);
        Add(TConstant(sym).ConstType);
      end;
      nkField: begin
        DoAdd(TField(sym).FieldType);
        Add(TField(sym).FieldType);
      end;
      nkProperty: begin
          Add(TProperty(sym).PropType);
          DoAddArgs(TProperty(sym).Args);
        end;
      nkIntfProperty: begin
          Add(TIntfProperty(sym).PropType);
          DoAddArgs(TIntfProperty(sym).Args);
        end;
      nkMethod, nkFunc, nkExternalFunc: DoAddFunc(TFunctionDecl(sym));
    end;
  end;

  procedure EnumSymbols(sym: TSymbol);
  var
    i: Integer;
    symbols: TSymbolTable;
  begin
    symbols := nil;
    case sym.NodeKind of
      nkModule: symbols := TModule(sym).Symbols;
      nkType:
        case TType(sym).TypeCode of
          typClass: symbols := TClassType(sym).Symbols;
          typObject: symbols := TObjectType(sym).Symbols;
          typInterface, typDispInterface: symbols := TInterfaceType(sym).Symbols;
          typRecord: symbols := TRecordType(sym).Symbols;
        end;
      nkFunc, nkMethod: symbols := TFunction(sym).LocalSymbols;
    end;

    if symbols = nil then Exit;

    for i := 0 to symbols.Count - 1 do
    begin
      DoAdd(symbols[i]);
    end;
  end;
begin
  EnumSymbols(M);
end;

procedure TCUWriter.Write(const Buffer; Size: Cardinal);
begin
  if FileWrite(FHandle, Buffer, Size) < 0 then
    SysUtils.RaiseLastOSError;
end;

procedure TCUWriter.WriteBool(Value: Boolean);
begin
  if Value then
    WriteByte(1)
  else
    WriteByte(0);
end;

procedure TCUWriter.WriteBuf(const S: AnsiString);
begin
  Write(PAnsiChar(S)^, Length(S));
end;

procedure TCUWriter.WriteByte(b: Byte);
begin
  Write(b, 1);
end;

procedure TCUWriter.WriteChar(C: AnsiChar);
begin
  Write(C, 1);
end;

procedure TCUWriter.WriteId(const s: string);
begin
  WriteSInt32(Length(s));
  WriteBuf(s);
end;

procedure TCUWriter.WriteModule(const M: TModule; const UnitFile: string);
  procedure Open(const s: string);
  begin
    FHandle := FileCreate(s);
    if FHandle = -1 then
      SysUtils.RaiseLastOSError;
  end;
var
  i: Integer;
  sym: TSymbol;
begin
  Open(UnitFile);
  try
    FModule := M;
    GetAllExternalSymbols(M);
    WriteBuf('CU;');
    WriteSInt32(1); // version
    WriteID(M.Name);
    WriteUInt32(M.TimeStamp.Date); // check sum
    WriteUInt32(M.TimeStamp.Time); // timestamp
    WriteByte(M.PointerSize); // pointersize;
    WriteUInt32(M.LoadedUnits.Count);
    for i := 0 to M.LoadedUnits.Count - 1 do
    begin
      sym := M.LoadedUnits[i];
      Assert(sym.NodeKind = nkModule, 'Symbol in LoadedUnits not module!');
      WriteId(TModule(sym).Name);
      WriteUInt32(TModule(sym).TimeStamp.Date);
      WriteUInt32(TModule(sym).TimeStamp.Time);
    end;

    WriteUInt32(FExternalSymbols.Count);
    for i := 0 to FExternalSymbols.Count - 1 do
      WriteId(FExternalSymbols[i]);

    WriteUInt32(M.Symbols.Count);
    for i := 0 to M.Symbols.Count - 1 do
    begin
      sym := TSymbol(M.Symbols[i]);
      try
        if sym.NodeKind = nkModule then
          WriteSymbol(nil)
        else
          WriteSymbol(sym);
      except
        raise Exception.CreateFmt('Write module error in index %d, %s', [i, sym.Name]);
      end;
    end;
  finally
    FileClose(FHandle);
    FHandle := -1;
    FModule := nil;
  end;
end;

procedure TCUWriter.WriteRef(Ref: TSymbol);
var
  Index: Integer;
begin
  if Ref = nil then
  begin
    WriteChar('R');
    WriteId('');
    Exit;
  end;

  if Ref.NodeKind = nkAccessor then
  begin
    WriteChar('A');
    WriteUInt32(TMultiAccessor(Ref).FieldCount);
    Assert(TMultiAccessor(Ref).FieldCount > 1);
    WriteRef(TMultiAccessor(Ref).Fields[0]);

    for Index := 1 to TMultiAccessor(Ref).FieldCount do
      WriteId(TMultiAccessor(Ref).Fields[Index].Name);
    Exit;
  end;

  // 一些类型没有名称或匿名
  if Ref.Name = '' then
  begin
    Assert(not (saPrimitive in Ref.Attr));
    WriteChar('U');
    WriteSymbol(Ref);
    Exit;
  end;

  if Ref.Module = FModule then
  begin
    WriteChar('R');
    WriteId(Ref.SymName);
  end
  else
  begin
    if not FExternalSymbols.Find(Ref.FullName, Index) then
      raise ECUError.CreateFmt('External symbol "%s" not in list', [Ref.FullName]);
    WriteChar('X');
    WriteUInt32(Index);
  end;
end;

procedure TCUWriter.WriteSInt32(i: Integer);
var
  d: TByteData;
  len: Integer;
begin
  EncodeSInt32(i, d, len);
  Write(d, len);
end;

procedure TCUWriter.WriteSInt64(i: Int64);
var
  d: TByteData;
  len: Integer;
begin
  EncodeInt64(i, d, len);
  Write(d, len);
end;

procedure TCUWriter.WriteStr(const S: AnsiString);
begin
  WriteSInt32(Length(s));
  WriteBuf(s);
end;

procedure TCUWriter.WriteSymbol(Sym: TSymbol);
const
  SymTags: array[nkType..nkAccessor] of Char = (
  // nkType, nkNameScope, nkModule, nkVariable, nkConstant,
  't', 'N', 'M', 'v', 'c',
  // nkField, nkProperty, nkIntfProperty, nkMethod, nkMethodResolution,
  'f', 'p', 'P', 'm', 'r',
  // nkArgument, nkEnumElement, nkFunc, nkExternalFunc, nkBuiltinFunc,
  'a', 'e', 'F', 'x', 'u',
  // nkAccessor
  'A'
  );

  procedure WriteVar(V: TVariable);
  begin
    WriteRef(V.VarType);
    WriteRef(V.AbsVar);
    WriteValue(V.Value);
    WriteUInt32(Byte(V.Attr));
  end;

  procedure WriteArg(V: TArgument);
  begin
    WriteId(V.Name);
    WriteByte(Ord(V.ArgKind));
    if V.ArgKind = akNormal then
      WriteRef(V.ArgType)
    else if V.ArgKind = akArrayOfType then
      WriteRef(TOpenArrayType(V.ArgType).ElementType);
//    if V.ArgKind in [akNormal, akArrayOfType] then
//      WriteRef(V.ArgType);
    WriteValue(V.DefaultValue);
    WriteUInt32(Byte(V.Modifier));
//    WriteBool(V.IsOpenArray);
  end;

  procedure WriteArgs(List: TList);
  var
    i: Integer;
  begin
    if List <> nil then
    begin
      WriteUInt32(List.Count);
      for i := 0 to List.Count - 1 do
        WriteArg(TArgument(List[i]));
    end
    else
      WriteUInt32(0);
  end;

  procedure WriteConst(Sym: TConstant);
  begin
    WriteRef(Sym.ConstType);
    if SameText(Sym.Name, 'vmtSelfPtr') then

      sym.Name := Sym.Name;
    WriteValue(Sym.Value);
    WriteBool(Sym.IsResStr);
  end;

  procedure WriteField(Sym: TField);
  begin
    WriteRef(Sym.FieldType);
    WriteSInt64(Sym.Offset);
    WriteUInt32(Byte(Sym.FieldAttr));
  end;

{  procedure WriteAccessor(Accessor: TMultiAccessor);
  var
    i: Integer;
  begin
    WriteUInt32(Accessor.FieldCount);
    for i := 0 to Accessor.FieldCount - 1 do
      WriteRef(Accessor.Fields[i]);
  end; }

  procedure WriteProp(Sym: TProperty);
  begin
    WriteRef(Sym.PropType);
    WriteValue(Sym.DefaultValue);
    WriteUInt32(Byte(Sym.PropAttr));
    WriteRef(Sym.Getter);
    WriteRef(Sym.Setter);
    WriteRef(Sym.Stored);
    WriteSInt32(Sym.Index);
    WriteArgs(Sym.Args);
  end;

  procedure WriteIntfProp(Sym: TIntfProperty);
  begin
    WriteRef(Sym.PropType);
    WriteUInt32(Byte(Sym.PropAttr));
    WriteSymbol(Sym.Getter);
    WriteSymbol(Sym.Setter);
    WriteSInt32(Sym.DispID);
    WriteArgs(Sym.Args);
  end;

  procedure WriteFunc(Sym: TFunctionDecl);
  begin
    WriteRef(Sym.ReturnType);
    WriteUInt32(Integer(Sym.Modifiers));
    WriteUInt32(Byte(Sym.CallConvention));
    WriteArgs(Sym.Args);
    case Sym.NodeKind of
      nkMethod: begin
        WriteSInt32(TMethod(Sym).VTIndex);
      end;
      nkExternalFunc: begin
        WriteStr(TExternalFunction(Sym).FileName);
        WriteStr(TExternalFunction(Sym).RoutineName);
        WriteSInt32(TExternalFunction(Sym).RoutineNo);
      end;
    end;

    Sym := Sym.NextOverload;
    while (Sym <> nil) and (saInternal in Sym.Attr) do
      Sym := Sym.NextOverload;

    if Sym <> nil then
    begin
      WriteChar('1');
      WriteSymbol(Sym);
    end
    else
      WriteChar('0');
  end;

  function CanWrite(Sym: TSymbol): Boolean;
  begin
    case Sym.NodeKind of
      nkBuiltinFunc: Result := False;
      nkType: begin
        Result := not (TType(Sym).TypeCode in [typShortint..typOleVariant, typText]);
        if Result then
          if TType(Sym).TypeCode = typFile then
            Result := TFileType(Sym).ElementType <> nil;
      end;
    else
      Result := True;
    end;
  end;
begin
  if Sym = nil then
  begin
    WriteChar('z');
    Exit;
  end;

  if saPrimitive in Sym.Attr then
  begin
    WriteChar('z');
    Exit;
  end;

  WriteChar(SymTags[Sym.NodeKind]);
  WriteId(Sym.Name);
  WriteUInt32(Integer(Sym.Visibility));
  WriteUInt32(byte(Sym.Hints));
  Exclude(Sym.Attr, saUsed);
  WriteUInt32(Byte(Sym.Attr));
  case Sym.NodeKind of
    nkType: WriteType(TType(Sym));
    nkVariable: WriteVar(TVariable(Sym));
    nkConstant: WriteConst(TConstant(Sym));
    nkField: WriteField(TField(Sym));
    nkProperty: WriteProp(TProperty(Sym));
    nkIntfProperty: WriteIntfProp(TIntfProperty(Sym));
    nkMethod: WriteFunc(TFunctionDecl(Sym));
//    nkMethodResolution,
    nkArgument: WriteArg(TArgument(Sym));
    nkEnumElement: begin end; // 已经在WriteEnum中
    nkFunc: WriteFunc(TFunctionDecl(Sym));
    nkExternalFunc: WriteFunc(TFunctionDecl(Sym));
//    nkAccessor: WriteAccessor(TMultiAccessor(Sym));
//    nkLabel: label不会出现在interface段
//    nkBuiltinFunc: begin
//    end;
  else
    raise ECUWriteError.CreateFmt('Invalid node: %d', [Ord(Sym.NodeKind)]);
  end;
end;

procedure TCUWriter.WriteType(Typ: TType);
  procedure WriteEnum(typ: TEnumType);
  var
    i: Integer;
    v: TEnumValue;
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteByte(typ.MinEnumSize);
    WriteUInt32(typ.Values.Count);
    for i := 0 to typ.Values.Count - 1 do
    begin
      v := typ.Values[i];
      WriteId(v.Name);
      WriteSInt32(v.Value);
    end;
  end;

  procedure WriteArg(V: TArgument);
  begin
    WriteId(V.Name);
    WriteByte(Ord(V.ArgKind));
    if V.ArgKind = akNormal then
      WriteRef(V.ArgType)
    else if V.ArgKind = akArrayOfType then
      WriteRef(TOpenArrayType(V.ArgType).ElementType);
    // todo 1: Error
//    if V.ArgKind in [akNormal, akArrayOfType] then
//      WriteRef(V.ArgType);
    WriteValue(V.DefaultValue);
    WriteUInt32(Byte(V.Modifier));
//    WriteBool(V.IsOpenArray);
  end;

  procedure WriteArgs(List: TList);
  var
    i: Integer;
  begin
    if List <> nil then
    begin
      WriteUInt32(List.Count);
      for i := 0 to List.Count - 1 do
        WriteArg(TArgument(List[i]));
    end
    else
      WriteUInt32(0);
  end;

  procedure WriteProc(typ: TProceduralType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.ReturnType);
    WriteUInt32(byte(typ.CallConvention));
    WriteBool(typ.IsMethodPointer);
    WriteArgs(typ.Args);
  end;

  procedure WriteRecordBody(Body: TRecordBody);
  var
    i: Integer;
  begin
    if Body = nil then
    begin
      WriteByte(0);
      Exit;
    end;

    if Body.ClassType = TRecordBody then
      WriteByte(1)
    else
      WriteByte(2);

    WriteUInt32(Body.Members.Count);
    for i := 0 to Body.Members.Count - 1 do
      WriteSymbol(TField(Body.Members[i]));
    WriteSymbol(Body.Selector);
    WriteByte(Body.MaxAlignSize);
    WriteRecordBody(Body.Variants);
    if Body.ClassType = TRecordVariant then
    begin
      WriteSInt64(TRecordVariant(Body).Offset);
      WriteRecordBody(TRecordVariant(Body).Next);
    end;
  end;

  procedure WriteRecord(typ: TRecordType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteByte(typ.GlobalAlignSize);
    WriteRecordBody(typ.Body);
  end;

  procedure WriteInterface(typ: TInterfaceType);
  var
    i: Integer;
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteBool(typ.IsDisp);
    WriteRef(typ.Base);
    WriteStr(GUIDToString(typ.Guid));
    WriteRef(typ.DefaultProp);
    WriteUInt32(typ.Symbols.Count);
    for i := 0 to typ.Symbols.Count - 1 do
      WriteSymbol(typ.Symbols[i]);
  end;
  {
  procedure WriteObject(typ: TObjectType);
  var
    i: Integer;
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.Base);
//    WriteRef(typ.DefaultProp);
    WriteUInt32(typ.GlobalAlignSize);
    WriteUInt32(Byte(typ.ObjectAttr));

  end; }

  procedure WriteClass(typ: TClassType);
  var
    i: Integer;
    MR: TMethodResolution;
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.Base);
    WriteRef(typ.DefaultProp);
    WriteUInt32(typ.GlobalAlignSize);
    WriteUInt32(Byte(typ.ClassAttr));
    if typ.Interfaces <> nil then
    begin
      WriteUInt32(typ.Interfaces.Count);
      for i := 0 to typ.Interfaces.Count - 1 do
        WriteRef(TSymbol(typ.Interfaces[i]));
    end
    else
      WriteUInt32(0);
    WriteUInt32(typ.Symbols.Count);
    for i := 0 to typ.Symbols.Count - 1 do
      WriteSymbol(typ.Symbols[i]);

    MR := typ.MR;
    while MR <> nil do
    begin
      WriteByte(1);
      WriteRef(MR.ImplementingMethod);
      WriteRef(MR.InterfaceMethod);
      MR := TMethodResolution(MR.Next);
    end;
    WriteByte(0);
  end;

  procedure WriteClassRef(typ: TClassRefType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.RefType);
  end;
  procedure WriteFileType(typ: TFileType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.ElementType);
  end;
  procedure WriteSubrange(typ: TSubrangeType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.BaseType);
    WriteSInt64(typ.RangeBegin);
    WriteSInt64(typ.RangeEnd);
  end;
  procedure WriteSet(typ: TSetType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.RangeType);
  end;

  procedure WriteArrayEl(El: TType);
  begin
    while El <> nil do
    begin
      if El.Name = '' then
      begin
        WriteByte(1);
        WriteSymbol(El);
      end
      else
      begin
        WriteByte(2);
        WriteRef(El);
        Exit;
      end;

      case El.TypeCode of
        typArray: El := TArrayType(El).ElementType;
        typDynamicArray: El := TDynamicArrayType(El).ElementType;
      else
        El := nil;
      end;
    end;
  end;

  procedure WriteArray(typ: TArrayType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.Range);
    WriteBool(typ.IsPacked);
    WriteArrayEl(typ.ElementType);
  end;

  procedure WriteDynArray(typ: TDynamicArrayType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteBool(typ.IsPacked);
    WriteArrayEl(typ.ElementType);
  end;

  procedure WriteAlias(typ: TAliasType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.RefType);
  end;

  procedure WriteClonedAlias(typ: TClonedAliasType);
  begin
    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.RefType);
  end;

  procedure WritePointer(typ: TPointerType);
  begin
    if typ.RefType = nil then Exit;

    WriteByte(Ord(typ.TypeCode));
    WriteRef(typ.RefType);
  end;
begin
  case typ.TypeCode of
    typShortint..typWideChar, typPAnsiChar, typPWideChar,
    typAnsiString, typWideString, typUnicodeString, typShortString,
    typVariant, typOleVariant, typText: begin
    end;

    typPointer: WritePointer(TPointerType(typ));
    typRecord: WriteRecord(TRecordType(Typ));
    typClass: WriteClass(TClassType(typ));
    typClassRef: WriteClassRef(TClassRefType(typ));
    typInterface, typDispInterface: WriteInterface(TInterfaceType(typ));
    typEnum: WriteEnum(TEnumType(typ));
    typProcedural: WriteProc(TProceduralType(typ));
    typFile: WriteFileType(TFileType(typ));
    typSet: WriteSet(TSetType(typ));
    typSubrange: WriteSubrange(TSubrangeType(typ));
    typArray: WriteArray(TArrayType(typ));
    typDynamicArray: WriteDynArray(TDynamicArrayType(typ));
    typAlias: WriteAlias(TAliasType(typ));
    typClonedType: WriteClonedAlias(TClonedAliasType(typ));
  else
    raise ECUWriteError.CreateFmt('Invalid type: %s', [ast.TypeNames[ typ.TypeCode]]);
  end;
end;

procedure TCUWriter.WriteUInt32(i: Cardinal);
var
  d: TByteData;
  len: Integer;
begin
  EncodeUInt32(i, d, len);
  Write(d, len);
end;

procedure TCUWriter.WriteValue(const V: TValueRec);

  procedure WriteSet(s: TSetValue);
  begin
    WriteByte(s.BitStart);
    WriteByte(s.BitCount);
    Write(s.Bits[s.BitStart], s.BitCount);
  end;

  procedure WriteFloat(d: Double);
  begin
    WriteStr(FloatToStr(d));
  end;

  procedure WriteCurr(c: Currency);
  begin
    WriteStr(CurrToStr(c));
  end;
begin
  WriteChar(Chr(Ord('0') + Ord(V.VT)));
  case V.VT of
    vtEmpty: begin
    end; 
    vtInt: WriteSInt32(V.VInt);

    vtInt64: WriteSInt64(V.VInt64);

    vtReal: WriteFloat(V.VReal);

    vtCurr: WriteCurr(V.VCurr);

    vtSet: WriteSet(TSetValue(V.VSet));

    vtBool: WriteBool(V.VBool);
    vtStr: WriteStr(AnsiString(V.VStr));
    vtAChr: WriteChar(V.VAChr);
    vtWChr: WriteUInt32(Word(V.VWChr));
    vtSymbol:
      WriteRef(V.VSymbol);

    //vtArray, vtRecord
  else
    raise ECUWriteError.CreateFmt('Invalid value type: %d', [Ord(V.VT)]);
  end;
end;

{ TCUReader }

procedure TCUReader.CheckFileType(typ: TType);
var
  i: Integer;
begin
  if typ = nil then Exit;
  
  case Typ.TypeCode of
    typFile..typText:
      FmtError('File type not allowed in here');
    typAnsiString..typUnicodeString,
    typVariant..typOleVariant,
    typDynamicArray, typInterface, typDispInterface:
      FmtError('Type need finalization, not allowed in file type');
    typArray:
      CheckFileType(TArrayType(Typ).ElementType);
    typRecord:
      for I := 0 to TRecordType(Typ).Symbols.Count - 1 do
        CheckFileType(TField(TRecordType(Typ).Symbols[I]).FieldType);
  end;
end;

procedure TCUReader.ClearFixups;
var
  i: Integer;
begin
  for i := 0 to FFixups.Count - 1 do
    TFixup(FFixups[i]).Free;
  FFixups.Count := 0;
end;

procedure TCUReader.Close;
begin
  if FStream <> nil then
  begin
    FStream.Free;
    FStream := nil;
    ClearFixups;
  end;
end;

constructor TCUReader.Create;
begin
  FExternalSymbols := TStringList.Create;
  FFixups := TList.Create;
  FFixups.Capacity := 100;
end;

function TCUReader.CreateSymbol(SymClass: TSymbolClass): TSymbol;
begin
  Result := FCntx.CreateSymbol(SymClass);
//  Result.Coord.FileName := FModule
end;

destructor TCUReader.Destroy;
begin
  FStream.Free;
  ClearFixups;
  FFixups.Free;
  FExternalSymbols.Free;
  inherited;
end;

function TCUReader.FindSymbol(const s: string): TSymbol;
  function Next(const s: string; var start: Integer): string;
  var
    i: Integer;
  begin
    for i := start to Length(s) do
    begin
      if s[i] = '.' then
      begin
        Result := Copy(s, start, i - 1);
        start := i + 1;
        Exit;
      end
    end;
    if start = 1 then
      Result := s
    else
      Result := Copy(s, start, Length(s));
    start := Length(s) + 1;
  end;

  function Lookup(sym: TSymbol; const s: string): TSymbol;
  begin
    case sym.NodeKind of
      nkNameScope: Result := TNameScope(sym).FindSymbol(s);
      nkModule: Result := TModule(sym).FindSymbol(s);
      nkType:
        case TType(sym).TypeCode of
          typClass: Result := TClassType(sym).FindSymbol(s);
          typInterface, typDispInterface: Result := TInterfaceType(sym).FindSymbol(s);
          typRecord: Result := TRecordType(sym).FindSymbol(s);
          typObject: Result := TObjectType(sym).FindSymbol(s);
        else
          raise ECUReadError.CreateFmt('Invalid type symbol: %s', [s]);
        end
    else
      raise ECUReadError.CreateFmt('Invalid symbol: %s', [s]);
    end;
  end;
var
  id: string;
  i: Integer;
  sym: TSymbol;
begin
  sym := nil;
  i := 1;
  id := Next(s, i);
  repeat
    if sym = nil then
      sym := FModule.FindSymbol(s)
    else
      sym := Lookup(sym, s);
    if sym = nil then Break;
    id := Next(s, i);
  until id = '';
  Result := sym;
end;

procedure TCUReader.FixupRefs;
var
  i: Integer;
  Fixup: TFixup;
begin
  for i := 0 to FFixups.Count - 1 do
  begin
    Fixup := TFixup(FFixups[i]);
    with Fixup do
      ResolveSym(Name, False, Inst, Ref, Kind, ExpectNodes, ExpectTypes, AllowNull);
  end;
end;

procedure TCUReader.FmtError(const s: string);
begin
  if s = '' then
    raise ECUReadError.Create('Invalid stream format')
  else
    raise ECUReadError.CreateFmt('Invalid stream format: %s', [s]);
end;

procedure TCUReader.FmtErrorFmt(const s: string;
  const Args: array of const);
begin
  FmtError(Format(s, Args));
end;

procedure TCUReader.GetHeader(out Header: TModuleHeader);
var
  s: string;
begin
  s := ReadBuf(3);
  if s <> 'CU;' then
    FmtError;
  Header.Version := ReadUInt32; // version
  Header.UnitName := ReadStr;
  Header.TimeStamp.Date := ReadUInt32;
  Header.TimeStamp.Time := ReadUInt32;
end;

procedure TCUReader.GetRef(Inst: TSymbol; Ref: Pointer;
  Kind: TFixupKind; ExpectNodes: TAstNodeKinds;
  ExpectTypes: TTypeCodes; AllowNull: Boolean);

  procedure GetAccessor;
  var
    i, count: Integer;
    s: string;
    acc: TMultiAccessor;
  begin
    if ExpectNodes <> [] then
      if not (nkAccessor in ExpectNodes) then
        raise ECUReadError.Create('Invalid symbol');
    count := ReadUInt32;
    if count < 2 then FmtError;
    acc := TMultiAccessor(CreateSymbol(TMultiAccessor));
    SetLength(acc.Fields, count);  // 事先分配空间，避免重分配空间，导致Fixups失败
    GetRef(nil, @acc.Fields[0], fkAddr, [nkVariable, nkField], []);
    for i := 1 to count - 1 do
    begin
      acc.Add(nil);
      s := ReadStr;
      GetRef(nil, @acc.Fields[i], fkAddr, [nkField], []);
    end;
    TSymbol(Ref) := acc;
  end;
var
  s: string;
  i: Integer;
  sym: TSymbol;
begin
  case ReadChar of
    'U': begin
      sym := GetSymbol;
      ResolveRef(sym, Inst, Ref, Kind, ExpectNodes, ExpectTypes, AllowNull);
      Exit;
    end;

    'A': begin
      GetAccessor;
      Exit;
    end;

    'R': s := ReadStr;

    'X': begin
      i := ReadUInt32;
      s := FExternalSymbols[i];
    end;
  else
    FmtError;
    Exit;
  end;

  ResolveSym(s, True, Inst, Ref, Kind, ExpectNodes, ExpectTypes, AllowNull);
end;

function TCUReader.GetSymbol(): TSymbol;

  function GetVar: TVariable;
  begin
    Result := TVariable(CreateSymbol(TVariable));
    GetRef(Result, @Result.VarType, fkAddr, [nkType], [], False);
    GetRef(Result, @Result.AbsVar, fkAddr, [nkVariable, nkAccessor], []);
    GetValue(Result.Value);
    Result.VarAttr := TVarAttributes(Byte(ReadUInt32));
  end;

  function GetConst: TConstant;
  begin
    Result := TConstant(CreateSymbol(TConstant));
    GetRef(Result, @Result.ConstType, fkAddr, [nkType], [], False);
    GetValue(Result.Value);
    Result.IsResStr := ReadBool;
  end;

  function GetField: TField;
  begin
    Result := TField(CreateSymbol(TField));
    GetRef(Result, @Result.FieldType, fkAddr, [nkType], [], False);
    Result.Offset := ReadSInt64;
    Result.FieldAttr := TFieldAttributes(Byte(ReadUInt32));
  end;

  function GetArg: TArgument;
  begin
    Result := TArgument(CreateSymbol(TArgument));
    Result.Name := ReadStr;
    Result.ArgKind := TArgumentKind(ReadByte);
    case Result.ArgKind of
      akNormal, akArrayOfType:
        GetRef(Result, @Result.ArgType, fkAddr, [nkType], [], False);
      akArrayOfConst:
        Result.ArgType := FCntx.FVarOpenArrayType;
      akUntype:
        Result.ArgType := FCntx.FUntype;
    else
      FmtError('Invalid argument kind');
    end;

    GetValue(Result.DefaultValue);
    Result.Modifier := TArgumentModifier(Byte(ReadUInt32));
  end;

  procedure GetArgs(List: TList; Count: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      List.Add(GetArg);
  end;

  function GetProp: TProperty;
  var
    i: Integer;
  begin
    Result := TProperty(CreateSymbol(TProperty));
    GetRef(Result, @Result.PropType, fkAddr, [nkType], [], False);
    Result.PropAttr := TPropertyAttributes(Byte(ReadUInt32));
    GetRef(Result, @Result.Getter, fkAddr, [nkField, nkMethod, nkAccessor], []);
    GetRef(Result, @Result.Setter, fkAddr, [nkField, nkMethod, nkAccessor], []);
    GetRef(Result, @Result.Stored, fkAddr, [nkField, nkMethod, nkAccessor], []);
    Result.Index := ReadSInt32;
    i := ReadUInt32;
    if i > 0 then
    begin
      Result.CreateArgs;
      GetArgs(Result.Args, i);
    end;
  end;

  function GetIntfProp: TIntfProperty;
  var
    i: Integer;
  begin
  {
      WriteRef(Sym.PropType);
    WriteUInt32(Byte(Sym.PropAttr));
    WriteSymbol(Sym.Getter);
    WriteSymbol(Sym.Setter);
    WriteSInt32(Sym.DispID);
    WriteArgs(Sym.Args);
}
    Result := TIntfProperty(CreateSymbol(TIntfProperty));
    GetRef(Result, @Result.PropType, fkAddr, [nkType], [], False);
    Result.PropAttr := TIntfPropertyAttributes(Byte(ReadUInt32));
    GetRef(Result, @Result.Getter, fkAddr, [nkMethod, nkAccessor], []);
    GetRef(Result, @Result.Setter, fkAddr, [nkMethod, nkAccessor], []);
    Result.DispID := ReadSInt32;
    i := ReadUInt32;
    if i > 0 then
    begin
      Result.CreateArgs;
      GetArgs(Result.Args, i);
    end;
  end;

  function GetFunc(nk: TAstNodeKind): TFunctionDecl;
  var
    i: Integer;
    sym: TSymbol;
  begin
    case nk of
      nkFunc: Result := TFunction(CreateSymbol(TFunction));
      nkMethod: Result := TMethod(CreateSymbol(TMethod));
      nkExternalFunc: Result := TExternalFunction(CreateSymbol(TExternalFunction));
    else
      Assert(False);
      Result := nil;
    end;
    GetRef(Result, @Result.ReturnType, fkAddr, [nkType], []);
    Result.Modifiers := TFunctionModifiers(Integer(ReadUInt32));
    Result.CallConvention := TCallingConvention(Byte(ReadUInt32));
    i := ReadUInt32;
    if i > 0 then
    begin
      Result.CreateArgs;
      GetArgs(Result.Args, i);
    end;

    case nk of
      nkMethod:
        TMethod(Result).VTIndex := ReadSInt32;
      nkExternalFunc: begin
        TExternalFunction(Result).FileName := ReadStr;
        TExternalFunction(Result).RoutineName := ReadStr;
        TExternalFunction(Result).RoutineNo := ReadSInt32;
      end;
    end;

    if ReadChar = '1' then
    begin
      sym := GetSymbol;
      if not (sym is TFunctionDecl) then
        Result.AddOverload(TFunctionDecl(sym))
      else
        FmtError;
    end;
  end;
var
  tag: AnsiChar;
  name: string;
  vis, hints, attr: Integer;
begin
  tag := ReadChar;
  if tag = 'z' then
  begin
    Result := nil;
    Exit;
  end;

  name := ReadStr;
  vis := ReadUInt32;
  hints := ReadUInt32;
  attr := ReadUInt32;
  case tag of
    'z': begin
      Result := nil;
      Exit;
    end;
    't': Result := GetType;
    'v': Result := GetVar;
    'c': Result := GetConst;
    'f': Result := GetField;
    'p': Result := GetProp;
    'P': Result := GetIntfProp;
    'm': Result := GetFunc(nkMethod);
    'F': Result := GetFunc(nkFunc);
    'x': Result := GetFunc(nkExternalFunc);
  else
    FmtErrorFmt('Invalid symbol tag: %s, %s', [tag, name]);
    Result := nil;
  end;
  Result.Name := name;
  Result.Visibility := TMemberVisibility(vis);
  Result.Hints := TMemberHints(Byte(hints));
  Result.Attr := TSymbolAttributes(Byte(attr));
//  AddSymbol(Result);
end;

function TCUReader.GetType: TType;

  function GetPtr: TPointerType;
  begin
    Result := TPointerType(CreateSymbol(TPointerType));
    GetRef(Result, @Result.RefType, fkAddr, [nkType], []);
    Result.Size := FModule.PointerSize;
  end;

  function GetEnum: TEnumType;
  var
    i: Integer;
    E: TEnumValue;
  begin
    Result := TEnumType(CreateSymbol(TEnumType));
    Result.MinEnumSize := ReadByte;
    if not (Result.MinEnumSize in [1..4]) then
      FmtError;
    for i := 0 to ReadUInt32 - 1 do
    begin
      E := TEnumValue(CreateSymbol(TEnumValue));
      E.Name := ReadStr;
      E.Value := ReadSInt32;
      E.EnumType := Result;
      Result.Values.Add(E);
    end;
    Result.Update;
  end;

  function GetArg: TArgument;
  begin
    Result := TArgument(CreateSymbol(TArgument));
    Result.Name := ReadStr;
    Result.ArgKind := TArgumentKind(ReadByte);
    case Result.ArgKind of
      akNormal, akArrayOfType:
        GetRef(Result, @Result.ArgType, fkAddr, [nkType], [], False);
      akArrayOfConst:
        Result.ArgType := FCntx.FVarOpenArrayType;
      akUntype:
        Result.ArgType := FCntx.FUntype;
    else
      FmtError('Invalid argument kind');
    end;

    GetValue(Result.DefaultValue);
    Result.Modifier := TArgumentModifier(Byte(ReadUInt32));
  end;

  procedure GetArgs(List: TList; Count: Integer);
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      List.Add(GetArg);
  end;

  function GetProc: TProceduralType;
  var
    i: Integer;
  begin
    Result := TProceduralType(CreateSymbol(TProceduralType));
    GetRef(Result, @Result.ReturnType, fkAddr, [nkType], []);
    Result.CallConvention := TCallingConvention(Byte(ReadUInt32));
    Result.IsMethodPointer := ReadBool;
    i := ReadUInt32;
    if i > 0 then
    begin
      Result.CreateArgs;
      GetArgs(Result.Args, i);
    end;
    Result.Size := FModule.PointerSize;
  end;

  function GetRecBody: TRecordBody;
  var
    i: Integer;
    sym: TSymbol;
    vbody: TRecordBody;
  begin
    case ReadByte of
      0: Result := nil;
      1: Result := TRecordBody.Create;
      2: Result := TRecordVariant.Create;
    else
      FmtError;
      Result := nil;
    end;
    if Result = nil then Exit;

    for i := 0 to ReadUInt32 - 1 do
    begin
      Sym := GetSymbol;
      if Sym = nil then Continue;
      if Sym.NodeKind <> nkField then
        FmtError('Expect field');
      Result.Members.Add(Sym);
    end;

    Sym := GetSymbol;
    if Sym <> nil then
    begin
      if Sym.NodeKind <> nkField then
        FmtError('Field expected');
      Result.Selector := TField(Sym);
    end;

    Result.MaxAlignSize := ReadByte;
    vbody := GetRecBody;
    if vbody <> nil then
    begin
      if vbody.ClassType <> TRecordVariant then
        FmtError('Record variant body expected');
      Result.Variants := TRecordVariant(vbody);
    end;

    if Result.ClassType = TRecordVariant then
    begin
      TRecordVariant(Result).Offset := ReadSInt64;
      vbody := GetRecBody;
      if vbody <> nil then
      begin
        if vbody.ClassType <> TRecordVariant then
          FmtError('Record variant body expected');
        TRecordVariant(Result).Next := TRecordVariant(vbody);
      end;
    end;
  end;

  procedure UpdateRec(Rec: TRecordType);
  begin
    if Rec.Body <> nil then
      Rec.Body.AddFields(Rec.Symbols);
    //for i := 0 to Rec.Symbols.Count - 1 do
  end;

  function GetRecord: TRecordType;
  begin
    Result := TRecordType(CreateSymbol(TRecordType));
    Result.GlobalAlignSize := ReadByte;
    Result.Body := GetRecBody;
    UpdateRec(Result);
    Result.Update;
  end;

  function GetGuid: TGuid;
  var
    s: string;
  begin
    s := ReadStr;
    try
      Result := StringToGuid(s);
    except
      FmtError('Invalid guid');
    end;
  end;

  function GetIntf: TInterfaceType;
  var
    i: Integer;
    ExpectTypes: TTypeCodes;
    sym: TSymbol;
  begin
    Result := TInterfaceType(CreateSymbol(TInterfaceType));

    Result.IsDisp := ReadBool;
    if Result.IsDisp then
      ExpectTypes := [typDispInterface]
    else
      ExpectTypes := [typInterface];
    GetRef(Result, @Result.Base, fkAddr, [nkType], ExpectTypes);
    Result.Guid := GetGuid;
    GetRef(Result, @Result.DefaultProp, fkAddr, [nkIntfProperty], []);
    for i := 0 to ReadUInt32 - 1 do
    begin
      sym := GetSymbol;
      if sym = nil then Continue;

      if not (sym.NodeKind in [nkMethod, nkIntfProperty]) then
        FmtError('Property or method expected');
      Result.Symbols.Add(sym);
    end;
    Result.Size := FModule.PointerSize;
  end;

  function GetClass: TClassType;
  var
    i, count: Integer;
    sym: TSymbol;
    MR: TMethodResolution;
  begin
    Result := TClassType(CreateSymbol(TClassType));
    GetRef(Result, @Result.Base, fkAddr, [nkType], [typClass]);
    GetRef(Result, @Result.DefaultProp, fkAddr, [nkProperty], []);
    Result.GlobalAlignSize := ReadUInt32;
    Result.ClassAttr := TClassAttributes(Byte(ReadUInt32));
    count := ReadUInt32;
    if count > 0 then
    begin
      Result.CreateInterfaces;
      Result.Interfaces.Count := count;
      for i := 0 to count - 1 do
        GetRef(nil, @Result.Interfaces.List^[i], fkAddr,
                [nkType], [typInterface], False);
    end;

    for i := 0 to ReadUInt32 - 1 do
    begin
      sym := GetSymbol;
      if sym = nil then Continue;
      if not (sym.NodeKind in [nkField, nkProperty, nkMethod]) then
        FmtError;
      Result.Symbols.Add(sym);
    end;

    while ReadByte = 1 do
    begin
      MR := TMethodResolution(CreateSymbol(TMethodResolution));
      GetRef(nil, @MR.ImplementingMethod, fkAddr, [nkMethod], []);
      GetRef(nil, @MR.InterfaceMethod, fkAddr, [nkMethod], []);
      MR.Next := Result.MR;
      Result.MR := MR;
    end;
//    Result.Size := FModule.PointerSize;
    Result.Update(Self.FPointerSize);
  end;

  function GetClassRef: TClassRefType;
  begin
    Result := TClassRefType(CreateSymbol(TClassRefType));
    GetRef(Result, @Result.RefType, fkAddr, [nkType], [typClass], False);
    Result.Size := FModule.PointerSize;
  end;

  function GetFile: TFileType;
  begin
    Result := TFileType(CreateSymbol(TFileType));
    GetRef(Result, @Result.ElementType, fkAddr, [nkType], [], False);
    Result.Size := FModule.PointerSize;
  end;

  function GetSubrange: TSubrangeType;
  begin
    Result := TSubrangeType(CreateSymbol(TSubrangeType));
    GetRef(Result, nil, fkRangeBaseType, [nkType], [], False);
    Result.RangeBegin := ReadSInt64;
    Result.RangeEnd := ReadSInt64;
  end;

  function GetSet: TSetType;
  begin
    Result := TSetType(CreateSymbol(TSetType));
    GetRef(Result, @Result.RangeType, fkAddr, [nkType], [typSubrange]);
    Result.Update;
  end;

  procedure GetArrayEl(Arr: TType; var Result: TType);
  var
    i: Integer;
    sym: ^TType;
  begin
    sym := @Result;
    i := ReadByte;
    while i > 0 do
    begin
      if i = 1 then
      begin
        TSymbol(sym^) := GetSymbol;
        if sym^ = nil then
          FmtError('Null symbol');
        if sym^.NodeKind <> nkType then
          FmtError('Type expected');
        case TType(sym^).TypeCode of
          typArray: sym := @TArrayType(sym^).ElementType;
          typDynamicArray: sym := @TDynamicArrayType(sym^).ElementType;
        else
          Break;
        end;
        i := ReadByte;
      end
      else
      begin
        GetRef(Arr, sym, fkAddr, [nkType], [], False);
        Exit;
      end;
    end;
  end;

  function GetArray: TArrayType;
  begin
    Result := TArrayType(CreateSymbol(TArrayType));
    GetRef(Result, @Result.Range, fkAddr, [nkType], [typSubrange], False);
    Result.IsPacked := ReadBool;
    GetArrayEl(Result, Result.ElementType);
    Result.Update;
  end;

  function GetDynArray: TDynamicArrayType;
  begin
    Result := TDynamicArrayType(CreateSymbol(TDynamicArrayType));
    Result.IsPacked := ReadBool;
    GetArrayEl(Result, Result.ElementType);
  end;

  function GetAlias: TAliasType;
  begin
    Result := TAliasType(CreateSymbol(TAliasType));
    GetRef(Result, @Result.RefType, fkAddr, [nkType], [], False);
    Result.Update;
  end;

  function GetClonedAlias: TClonedAliasType;
  begin
    Result := TClonedAliasType(CreateSymbol(TClonedAliasType));
    GetRef(Result, @Result.RefType, fkAddr, [nkType], [], False);
    Result.Update;
  end;
begin
  case TTypeCode(ReadByte) of
    typPointer: Result := GetPtr;
    typEnum: Result := GetEnum;
    typProcedural: Result := GetProc;
    typRecord: Result := GetRecord;
    typInterface: Result := GetIntf;
    typClass: Result := GetClass;
    typClassRef: Result := GetClassRef;
    typFile: Result := GetFile;
    typSubrange: Result := GetSubrange;
    typSet: Result := GetSet;
    typArray: Result := GetArray;
    typDynamicArray: Result := GetDynArray;
    typAlias: Result := GetAlias;
    typClonedType: Result := GetClonedAlias;
  else
    FmtError;
    Result := nil;
  end;
end;

procedure TCUReader.GetValue(var V: TValueRec);
  function ReadFloat: Double;
  var
    s: string;
  begin
    s := ReadStr;
    if not TryStrToFloat(s, Result) then
      FmtError;
  end;

  function ReadCurr: Currency;
  var
    s: string;
  begin
    s := ReadStr;
    if not TryStrToCurr(s, Result) then
      FmtError;
  end;

  function ReadSet: TSetValue;
  begin
    Result := TSetValue.Create;
    Result.BitStart := ReadByte;
    Result.BitCount := ReadByte;
    Read(Result.Bits[Result.BitStart], Result.BitCount);
  end;

  function ReadWChar: WideChar;
  var
    w: Integer;
  begin
    w := ReadUInt32;
    if w > $ffff then FmtError;
    Result := WideChar(w);
  end;
begin
  case ReadChar of
    '0': ValClear(V);
    '1': V := ValFromInt(ReadSInt32);
    '2': V := ValFromInt(ReadSInt64);
    '3': V := ValFromReal(ReadFloat);
    '4': V := ValFromCurr(ReadCurr);
    '5': V := ValFromSet(ReadSet);
    '6': V := ValFromBool(ReadBool);
    '7': V := ValFromStr(ReadStr);
    '8': V := ValFromChar(ReadChar);
    '9': V := ValFromWChar(ReadWChar);
//    #58: V := ValFromPtr(
    #59: GetRef(nil, @V.VSymbol, fkAddr, [], []);
//    #60:
//    #61
{
    vtInt, vtInt64, vtReal, vtCurr, vtSet, vtBool, vtStr,
    vtAChr, vtWChr, vtPtr, vtSymbol, vtArray, vtRecord
}
  else
    FmtError;
  end;
end;

procedure TCUReader.Open(const UnitFile: string);
begin
  Close;
  fStream := TFileStream.Create(UnitFile, fmOpenRead or fmShareDenyNone);
end;

procedure TCUReader.Read(var Buffer; Size: Cardinal);
begin
  fStream.ReadBuffer(Buffer, Size);
end;

function TCUReader.ReadBool: Boolean;
begin
  Result := ReadByte <> 0; 
end;

function TCUReader.ReadBuf(Len: Integer): AnsiString;
begin
  SetLength(Result, Len);
  Read(Result[1], Len);
end;

function TCUReader.ReadByte: Byte;
begin
  fStream.Read(Result, 1);
end;

function TCUReader.ReadChar: AnsiChar;
begin
  fStream.Read(Result, 1);
end;

procedure TCUReader.ReadModule(M: TModule; Cntx: TCompileContext);
  procedure SplitNameScope(M: TModule);
  begin
    
  end;
var
  s: string;
  i: Integer;
  sym: TSymbol;
  ts: TFileTimeStamp;
  CurMod: TModule;
begin
  FModule := M;
  FCntx := Cntx;
  FStream.Position := 0;
  s := ReadBuf(3);
  if s <> 'CU;' then
    FmtError;
  i := ReadUInt32; // version
  if i <> 1 then
    FmtError;
  M.Name := ReadStr;
  // todo 1: 将M.Name分解成Names
  SplitNameScope(M);

  M.TimeStamp.Date := ReadUInt32; // checksum
  M.TimeStamp.Time := ReadUInt32; // timestamp

  FCntx.FModules.Add(M);
  FPointerSize := ReadByte;
  if (FPointerSize <> 4) and (FPointerSize <> 8) then
    FmtError; 
  // load units
  for i := 0 to ReadUInt32 - 1 do
  begin
    s := ReadStr;
    ts.Date := ReadUInt32;
    ts.Time := ReadUInt32;
    if SameText(s, 'system') then Continue;

    CurMod := FCntx.LoadUnit(s);
    if (CurMod.TimeStamp.Date <> ts.Date) or
        (CurMod.TimeStamp.Time <> ts.Time) then
    begin
      raise EDifferenceVersion.Create('');
    end;
    FModule.LoadedUnits.Add(CurMod);
  end;

  // read external symbols
  FExternalSymbols.Clear;
  i := ReadUInt32;
  FExternalSymbols.Capacity := i;
  for i := 0 to i - 1 do
  begin
    FExternalSymbols.Add(ReadStr)
  end;

  // load symbols
//  FSymbols := M.Symbols;
  for i := 0 to ReadUInt32 - 1 do
  begin
  try
      sym := GetSymbol;
    if sym <> nil then
      M.Symbols.Add(sym);
  except
    on E: Exception do
      raise ECUReadError.CreateFmt('%d,%s', [i, E.Message]);
  end;
  end;

  FixupRefs;
end;

function TCUReader.ReadSInt32: Integer;
var
  b, shift: Integer;
begin
  shift := 0;
  Result := 0;
  repeat
    b := ReadByte;
    Result := Result or ((b and $7f) shl shift);
    Inc(shift, 7);
    if shift > 35 then FmtError;
    if b and $80 = 0 then Break;
  until False;
  if (shift < 32) and (b and $40 <> 0) then
    Result := Result or -(1 shl shift);      // sign extend,

(*
public static int readSignedLeb128(ByteInput in) {
        int result = 0;
        int cur;
        int count = 0;
        int signBits = -1;

        do {
            cur = in.readByte() & 0xff;
            result |= (cur & 0x7f) << (count * 7);
            signBits <<= 7;
            count++;
        } while (((cur & 0x80) == 0x80) && count < 5);

        if ((cur & 0x80) == 0x80) {
            throw new DexException("invalid LEB128 sequence");
        }

        // Sign extend if appropriate
        if (((signBits >> 1) & result) != 0 ) {
            result |= signBits;
        }

        return result;
    }*)
end;

function TCUReader.ReadSInt64: Int64;
var
  b, shift: Integer;
begin
  shift := 0;
  Result := 0;
  repeat
    b := ReadByte;
    Result := Result or ((b and $7f) shl shift);
    Inc(shift, 7);
    if shift > 63 then FmtError;
    if b and $80 = 0 then Break;
  until False;
  if (shift < 64) and (b and $40 <> 0) then
    Result := Result or -(1 shl shift);      // sign extend,
(*
result = 0;
shift = 0;
size = number of bits in signed integer;
while(true) {
  byte = next byte in input;
  result |= (low order 7 bits of byte << shift);
  shift += 7;
  /* sign bit of byte is second high order bit (0x40) */
  if (high order bit of byte == 0)
    break;
}

if ((shift <size) && (sign bit of byte is set))
  /* sign extend */
  result |= - (1 << shift);
  *)
end;

function TCUReader.ReadStr: AnsiString;
var
  Len: Integer;
begin
  Len := ReadUInt32;
  if Len > 0 then
    Result := ReadBuf(Len);
end;

function TCUReader.ReadUInt32: Cardinal;
var
  shift: Integer;
  b: Cardinal;
begin
  Result := 0;
  shift := 0;
  while True do
  begin
    if shift = 35 then
      FmtError;
    b := ReadByte;
    Result := Result or ((b and $7f) shl shift);
    if b and $80 = 0 then Break; 
    Inc(shift, 7);
  end;
  (*
result = 0;
shift = 0;
while(true) {
  byte = next byte in input;
  result |= (low order 7 bits of byte << shift);
  if (high order bit of byte == 0)
    break;
  shift += 7;
}
protected internal int Read7BitEncodedInt()
{
    byte num3;
    int num = 0;
    int num2 = 0;
    do
    {
        if (num2 == 0x23)
        {
            throw new FormatException();
        }
        num3 = this.ReadByte();
        num |= (num3 & 0x7f) << num2;
        num2 += 7;
    }
    while ((num3 & 0x80) != 0);
    return num;
}
  *)
end;

procedure TCUReader.ResolveRef(Sym, Inst: TSymbol; Ref: Pointer;
  Kind: TFixupKind; ExpectNodes: TAstNodeKinds; ExpectTypes: TTypeCodes;
  AllowNull: Boolean);
begin
  if ExpectNodes <> [] then
    if not (Sym.NodeKind in ExpectNodes) then
      raise ECUReadError.CreateFmt('Invalid symbol: %s', [Sym.Name]);
  if (ExpectTypes <> []) and (Sym.NodeKind = nkType) then
    if not (TType(Sym).TypeCode in ExpectTypes) then
      raise ECUReadError.CreateFmt('Invalid type: %s', [sym.Name]);
  if Kind = fkRangeBaseType then
  begin
    if (Inst= nil) or (Inst.NodeKind <> nkType) or (TType(Inst).TypeCode <> typSubrange) then
      raise ECUReadError.CreateFmt('Invalid instant type: %s', [Inst.Name]);
    TSubrangeType(Inst).BaseType := TType(Sym);
  end
  else begin
    TSymbol(Ref^) := Sym;
  end;

  // Do check
  case Sym.NodeKind of
    nkType:
      case TType(Sym).TypeCode of
        typFile: CheckFileType(TFileType(Sym).ElementType);
      end;
  end;

  if Inst <> nil then
    case Inst.NodeKind of
      nkArgument:
        if TArgument(Inst).ArgKind = akArrayOfType then
          TArgument(Inst).ArgType := FCntx.GetOpenArrayType(TArgument(Inst).ArgType);
    end;
end;

procedure TCUReader.ResolveSym(const s: string; AddFixup: Boolean;
  Inst: TSymbol; Ref: Pointer; Kind: TFixupKind;
  ExpectNodes: TAstNodeKinds; ExpectTypes: TTypeCodes;
  AllowNull: Boolean);
var
  Sym: TSymbol;
  Fixup: TFixup;
begin
  if s = '' then
  begin
    if not AllowNull then
      raise ECUReadError.Create('Invalid null symbol');
    Exit;
  end;

  Sym := FindSymbol(s);
  if Sym = nil then
  begin
    if not AddFixup then
      raise ECUReadError.CreateFmt('Identifier %s not found', [s]);
    Fixup := TFixup.Create;
    Fixup.Name := s;
    Fixup.Inst := Inst;
    Fixup.Ref := Ref;
    Fixup.Kind := Kind;
    Fixup.AllowNull := AllowNull;
    Fixup.ExpectNodes := ExpectNodes;
    Fixup.ExpectTypes := ExpectTypes;
    FFixups.Add(Fixup);
  end
  else
  begin
    ResolveRef(Sym, Inst, Ref, Kind, ExpectNodes, ExpectTypes, AllowNull);
  end;                                                                    
end;

end.
