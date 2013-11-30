unit cntx;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast;

type
  ECompileContextError = class(Exception);
  EDifferenceVersion = class(Exception);

  EParserError = class(Exception)
  private
    FFileName: String;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFileName: String;
      ARow, AColumn: Integer);
    property FileName: String read FFileName;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;

  TErrorLevel = (elError, elWarning, elHint);

  TParserErrorInfo = class
  private
    FRow: Integer;
    FColumn: Integer;
    FFileName: string;
    FErrorMessage: string;
    FErrorLevel: TErrorLevel;
  public
    property FileName: string read FFileName write FFileName;
    property Row: Integer read FRow write FRow;
    property Column: Integer read FColumn write FColumn;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property ErrorLevel: TErrorLevel read FErrorLevel write FErrorLevel;
  end;

  TParserErrorEvent = procedure (ErrInfo: TParserErrorInfo) of object;

  // 系统函数
  TSystemRoutine = (
    srIntOverflow, srOutOfRange, srIOCheck,
    srRaiseExcept, srSafecallCheck, srHandleSafecallExcept,
    srHandleCtorExcept,

    srInt64Div, srInt64Mod, srRound, srTrunc,

    srAStrClr, srAStrAddRef, srAStrNew, srAStrPtr, srAStrLength,
    srAStrComp, srAStrEqual, srAStrAsg, srAStrAsgCopy, srAStrSetLength,
    srAStrCopy, srAStrDelete, srAStrInsert, srAStrFromSStr, srAStrFromWStr,
    srAStrFromUStr, srAStrFromACh, srAStrFromWCh, srAStrFromPACh,
    srAStrFromPAChLen, srAStrFromPWCh, srAStrFromPWChLen,
    srAStrFromAArray, srAStrFromWArray, srAStrCat, srAStrCat3, srAStrCatN,

    srWStrClr, srWStrAddRef, srWStrNew, srWStrPtr, srWStrLength,
    srWStrComp, srWStrEqual, srWStrAsg, srWStrAsgCopy, srWStrSetLength,
    srWStrCopy, srWStrDelete, srWStrInsert, srWStrFromSStr, srWStrFromAStr,
    srWStrFromUStr, srWStrFromACh, srWStrFromWCh, srWStrFromPACh,
    srWStrFromPAChLen, srWStrFromPWCh, srWStrFromPWChLen,
    srWStrFromAArray, srWStrFromWArray, srWStrCat, srWStrCat3, srWStrCatN,

    srUStrClr, srUStrAddRef, srUStrNew, srUStrPtr, srUStrLength,
    srUStrComp, srUStrEqual, srUStrAsg, srUStrAsgCopy, srUStrSetLength,
    srUStrCopy, srUStrDelete, srUStrInsert, srUStrFromSStr, srUStrFromAStr,
    srUStrFromWStr, srUStrFromACh, srUStrFromWCh, srUStrFromPACh,
    srUStrFromPAChLen, srUStrFromPWCh, srUStrFromPWChLen,
    srUStrFromAArray, srUStrFromWArray, srUStrCat, srUStrCat3, srUStrCatN,

    srSStrComp, srSStrEqual, srSStrAsg,
    srSStrCopy, srSStrDelete, srSStrInsert, srSStrFromAStr, srSStrFromWStr,
    srSStrFromUStr, srSStrFromACh, srSStrFromWCh, srSStrFromPACh,
    srSStrFromPAChLen, srSStrFromPWCh, srSStrFromPWChLen,
    srSStrFromAArray, srSStrFromWArray, srSStrCat, srSStrCat3, srSStrCatN,

    srVarClr, srVarAddRef, srVarOp, srVarNot, srVarNeg, srVarCopy,
    srVar2AStr, srVar2WStr, srVar2UStr, srVar2SStr,
    srVar2Shortint, srVar2Byte, srVar2Smallint, srVar2Word,
    srVar2Longint, srVar2LongWord, srVar2Int64, srVar2UInt64,
    srVar2Single, srVar2Double, srVar2Currency, srVar2DateTime,
    
    srRecordClr, srRecordInit, srRecordAddRef,

    srArrayClr, srArrayInit, srArrayAddRef,

    srIntfClr, srIntfAddRef,

    srDynArrayClr, srDynArrayAddRef
    // 以后再增加
  );

  TCompileContext = class
  private
    FOpenArrayTypes: TList;
    FSystemLoaded: Boolean;
    FCUReader, FCUWriter: TObject;
    FOnError: TParserErrorEvent;
    FPendingParsers: TList;
//    FLoadingUnits: TList;
  public
    FNodes: TList;
    FModules: TSymbolTable;
    FSystemUnit: TModule;
    FTypes: array[typShortint..typText] of TType;

    // 通用类型
    FStringType: TType;
    FCharType: TType;
    FIntegerType: TType;
    FCardinalType: TType;
    FRealType: TType;
    FPCharType: TType;
    FNativeIntType, FNativeUIntType: TType;

    FUntype: TType;     // 无类型(void)
    FAnytype: TType;    // 没有返回值的表达式, 它的类型就是这个, 代表异常情况

    FShortintRangeType, FByteRangeType,
    FSmallintRangeType, FWordRangeType,
    FLongintRangeType, FLongWordRangeType,
    FInt64RangeType, FUInt64RangeType,
    FCharRangeType, FWideCharRangeType, FBoolRangeType: TSubrangeType;

    FCharSetType, FByteSetType, FBoolSetType, FEmptySetType: TSetType;
    FTObjectType: TClassType;
    FIUnknownType, FIDispatchType: TInterfaceType;
    FVarDataType, FVarRecType, FDateTimeType: TType;
    FVarOpenArrayType: TOpenArrayType;

    FTrueConst, FFalseConst: TConstant;

    FSystemRoutines: array [TSystemRoutine] of TFunction;

    procedure ClearNodes;
    procedure ClearParsers;
    function CreateAstNode(Typ: TAstNodeClass): TAstNode;
    function GetSubrangeType(Ordinal: TTypeCode): TSubrangeType;
    procedure AddBuiltinProcs(M: TModule);
    procedure AddTypes(M: TModule);
    procedure AddInternalTypes(M: TModule);
    procedure AddConstants(M: TModule);
  public
    IncludeDirs, UnitDirs: TStringList;
    UnitOutputDir: string;
    HasError: Boolean;

    constructor Create;
    destructor Destroy; override;
    function TypeOfRange(r1, r2: Int64): TType;
    procedure AddPredefinedElements(M: TModule);
    function CreateSymbol(SymClass: TSymbolClass): TSymbol;
    procedure LoadSystemUnit;
    function LoadUnit(const UnitName: string): TModule; overload;
    function GetSystemRoutine(Routine: TSystemRoutine): TFunction;
//    function LoadUnit(const UnitName: string; const ExpectTimeStamp: TTimeStamp): TModule; overload;
    function Compile(const SrcFile, UnitFile: string; IsSys, DoGenCode: Boolean): TModule;
    function GetOpenArrayType(const typ: TType): TOpenArrayType;
    function GetUnitFile(const APasFile: string): String;
    property OnError: TParserErrorEvent read FOnError write FOnError;
  end;

implementation

uses cupersist, parser, fileutils, llvmemit;

{ EParserError }

constructor EParserError.Create(const AReason, AFileName: String; ARow,
  AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

{ TCompileContext }

procedure TCompileContext.AddBuiltinProcs(M: TModule);

  function CreateFunc(const Name: string; Kind: TBuiltinFunctionKind): TBuiltinFunction;
  begin
    Result := TBuiltinFunction.Create;
    Result.Name := Name;
    Result.Kind := Kind;
    FNodes.Add(Result);
  end;
begin
  M.Symbols.Add(CreateFunc('abs', bfAbs));
  M.Symbols.Add(CreateFunc('addr', bfAddr));
  M.Symbols.Add(CreateFunc('assigned', bfAssigned));
  M.Symbols.Add(CreateFunc('break', bfBreak));
  M.Symbols.Add(CreateFunc('chr', bfChr));
  M.Symbols.Add(CreateFunc('continue', bfContinue));
  M.Symbols.Add(CreateFunc('copy', bfCopy));
  M.Symbols.Add(CreateFunc('dec', bfDec));
  M.Symbols.Add(CreateFunc('dispose', bfDispose));
  M.Symbols.Add(CreateFunc('exclude', bfExclude));
  M.Symbols.Add(CreateFunc('exit', bfExit));
  M.Symbols.Add(CreateFunc('finalize', bfFinalize));
  M.Symbols.Add(CreateFunc('freemem', bfFreeMem));
  M.Symbols.Add(CreateFunc('getmem', bfGetMem));
  M.Symbols.Add(CreateFunc('hi', bfHi));
  M.Symbols.Add(CreateFunc('high', bfHigh));
  M.Symbols.Add(CreateFunc('inc', bfInc));
  M.Symbols.Add(CreateFunc('include', bfInclude));
  M.Symbols.Add(CreateFunc('initialize', bfInitialize));
  M.Symbols.Add(CreateFunc('length', bfLength));
  M.Symbols.Add(CreateFunc('lo', bfLo));
  M.Symbols.Add(CreateFunc('low', bfLow));
  M.Symbols.Add(CreateFunc('new', bfNew));
  M.Symbols.Add(CreateFunc('odd', bfOdd));
  M.Symbols.Add(CreateFunc('ord', bfOrd));
  M.Symbols.Add(CreateFunc('pred', bfPred));
  M.Symbols.Add(CreateFunc('ptr', bfPtr));
  M.Symbols.Add(CreateFunc('round', bfRound));
  M.Symbols.Add(CreateFunc('succ', bfSucc));
  M.Symbols.Add(CreateFunc('setlength', bfSetLength));
  M.Symbols.Add(CreateFunc('sizeof', bfSizeOf));
  M.Symbols.Add(CreateFunc('swap', bfSwap));
  M.Symbols.Add(CreateFunc('trunc', bfTrunc));
  M.Symbols.Add(CreateFunc('typeinfo', bfTypeInfo));
end;

procedure TCompileContext.AddConstants(M: TModule);
begin
  FFalseConst := TConstant(CreateAstNode(TConstant));
  FFalseConst.Name := 'False';
  FFalseConst.ConstType := FTypes[typBoolean];
  FFalseConst.Value := ValFromBool(False);
  M.Symbols.Add(FFalseConst);

  FTrueConst := TConstant(CreateAstNode(TConstant));
  FTrueConst.Name := 'True';
  FTrueConst.ConstType := FTypes[typBoolean];
  FTrueConst.Value := ValFromBool(True);
  M.Symbols.Add(FTrueConst);
end;

procedure TCompileContext.AddInternalTypes(M: TModule);

  function CreateRangeType(BaseType: TTypeCode; const Name: string; R1, R2: Int64): TSubrangeType;
  begin
    Result := TSubrangeType(CreateAstNode(TSubrangeType));
    Result.BaseType := FTypes[BaseType];
    Result.RangeBegin := R1;
    Result.RangeEnd := R2;
    Result.Name := Name;
    M.Symbols.Add(Result);
  end;

  function CreateSetType(Base: TSubrangeType; const Name: string): TSetType;
  begin
    Result := TSetType(CreateAstNode(TSetType));
    Result.RangeType := Base;
    Result.Update;
    Result.Name := Name;
  // todo 1: 这里还得再思考一下
    M.Symbols.Add(Result);
  end;
begin
// Name是必须的
  FShortintRangeType := CreateRangeType(typShortint, '$ShortintRange', -128, 127);
  FSmallintRangeType := CreateRangeType(typSmallint, '$SmallintRange', -32768, 32767);
  FLongintRangeType := CreateRangeType(typLongint, '$LongintRange', Longint($80000000), $7fffffff);
  FInt64RangeType := CreateRangeType(typInt64, '$Int64Range', Int64($8000000000000000), $7fffffffffffffff);
  FByteRangeType := CreateRangeType(typByte, '$ByteRange', 0, 255);
  FWordRangeType := CreateRangeType(typWord, '$WordRange', 0, $ffff);
  FLongWordRangeType := CreateRangeType(typLongWord, '$LongWordRange', 0, $ffffffff);
  FUInt64RangeType := CreateRangeType(typUInt64, '$UInt64Range', 0, $ffffffffffffffff);
  FCharRangeType := CreateRangeType(typAnsiChar, '$AnsiCharRange', 0, 255);
  FWideCharRangeType := CreateRangeType(typWideChar, '$WideCharRange', 0, $ffff);
  FBoolRangeType := CreateRangeType(typBoolean, '$BooleanRange', 0, 1);

  FCharSetType := CreateSetType(FCharRangeType, '$CharSet');
  FByteSetType := CreateSetType(FByteRangeType, '$ByteSet');
  FBoolSetType := CreateSetType(FBoolRangeType, '$BooleanSet');
  FEmptySetType := CreateSetType(nil, '$EmptySet');

  FUntype := TType(CreateAstNode(TUnresolvedType));
//  FUntype.Name := '$Untype';
//  M.Symbols.Add(FUntype);
  FAnytype := TType(CreateAstNode(TUnresolvedType));
  FVarOpenArrayType := TOpenArrayType(CreateAstNode(TOpenArrayType));
  FVarOpenArrayType.ElementType := FUntype;
end;

procedure TCompileContext.AddPredefinedElements(M: TModule);
var
  i: Integer;
begin
  AddTypes(M);
  AddInternalTypes(M);
  AddBuiltinProcs(M);
  AddConstants(M);
  for i := 0 to M.Symbols.Count - 1 do
    Include(M.Symbols[i].Attr, saPrimitive);
end;

procedure TCompileContext.AddTypes(M: TModule);

  function CreateType(size: Cardinal; code: TTypeCode;
      const name: string): TType; overload;
  begin
    Result := TPrimitiveType.Create(code);
    Result.Name := name;
    Result.Size := size;
    FNodes.Add(Result);
  end;

  function CreateType(typ: TTypeClass; size: Cardinal;
      const name: string): TType; overload;
  begin
    Result := typ.Create;
    Result.Name := name;
    Result.Size := size;
    FNodes.Add(Result);
  end;
const
  typSizes: array[typShortint..typText] of Integer = (
    // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
    1, 1, 2, 2, 4, 4, 8, 8,
    // typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
    8, 6, 4, 8, 10, 8,
    // typBoolean, typByteBool, typWordBool, typLongBool,
    1, 1, 2, 4,
    // typAnsiChar, typWideChar,
    1, 2,
    // typPointer, typPAnsiChar, typPWideChar,
    4, 4, 4,
    // typAnsiString, typWideString, typUnocodeString, typShortString,
    4, 4, 4, 256, // shortstring max size is 256
    // typVariant, typOleVariant,
    16, 16,
    // typFile, typText,
    4, 4
  );
  typNames: array[typShortint..typText] of string = (
    // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
    'Shortint', 'Byte', 'Smallint', 'Word', 'Longint', 'LongWord', 'Int64', 'UInt64',
    // typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
    'Comp', 'Real48', 'Single', 'Double', 'Extended', 'Currency',
    // typBoolean, typByteBool, typWordBool, typLongBool,
    'Boolean', 'ByteBool', 'WordBool', 'LongBool',
    // typAnsiChar, typWideChar,
    'AnsiChar', 'WideChar',
    // typPointer, typPAnsiChar, typPWideChar,
    'Pointer', 'PAnsiChar', 'PWideChar',
    // typAnsiString, typWideString, typUnicodeString, typShortString,
    'AnsiString', 'WideString', 'UnicodeString', 'ShortString',
    // typVariant, typOleVariant,
    'Variant', 'OleVariant',
    // typFile, typText,
    'File', 'Text'
  );
var
  T: TTypeCode;
begin
  for T := Low(FTypes) to High(FTypes) do
  begin
    if T in [typPointer, typFile, typAnsiString, typShortString] then Continue;

    FTypes[T] := CreateType(typSizes[T], T, typNames[T]);
    M.Symbols.Add(FTypes[T]);
  end;

  TPrimitiveType(FTypes[typVariant]).FAlign := 8;
  TPrimitiveType(FTypes[typOleVariant]).FAlign := 8;
  FTypes[typPointer] := CreateType(TPointerType, 4, 'Pointer');
  FTypes[typFile] := CreateType(TFileType, 4, 'File');
  FTypes[typAnsiString] := CreateType(TAnsiStringType, 4, 'AnsiString');
  fTypes[typShortString] := CreateType(TShortStringType, 257, 'ShortString');
  TShortStringType(FTypes[typShortString]).CharCount := 255;
  M.Symbols.Add(FTypes[typPointer]);
  M.Symbols.Add(FTypes[typFile]);
  M.Symbols.Add(FTypes[typAnsiString]);
  M.Symbols.Add(FTypes[typShortString]);

  // general types / alias
  FIntegerType := CreateType(4, typLongint, 'Integer');
  FCardinalType := CreateType(4, typLongWord, 'Cardinal');
  FCharType := CreateType(1, typAnsiChar, 'Char');
  FRealType := CreateType(8, typDouble, 'Real');
  FStringType := CreateType(4, typAnsiString, 'String');
  FPCharType := CreateType(4, typPAnsiChar, 'PChar');
  FNativeIntType := CreateType(4, typLongint, 'NativeInt');
  FNativeUIntType := CreateType(4, typLongWord, 'NativeUInt');
  M.Symbols.Add(FIntegerType);
  M.Symbols.Add(FCardinalType);
  M.Symbols.Add(FCharType);
  M.Symbols.Add(FRealType);
  M.Symbols.Add(FStringType);
  M.Symbols.Add(FPCharType);
  M.Symbols.Add(FNativeIntType);
  M.Symbols.Add(FNativeUIntType);

{  // for test
  FObjectType := TClassType(CreateAstNode(TClassType));
  FObjectType.Name := 'TObject';
  FObjectType.Size := 4;
  M.Symbols.Add(FObjectType);

  FUnknownType := TInterfaceType(CreateAstNode(TInterfaceType));
  FUnknownType.Name := 'IUnknown';
  FUnknownType.Guid := IUnknown;
  M.Symbols.Add(FUnknownType);}
end;

procedure TCompileContext.ClearNodes;
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    TObject(FNodes[I]).Free;
  FNodes.Clear;
end;

procedure TCompileContext.ClearParsers;
var
  i: Integer;
begin
  for i := 0 to FPendingParsers.Count - 1 do
    TObject(FPendingParsers[i]).Free;
  FPendingParsers.Clear;
end;

function TCompileContext.Compile(const SrcFile, UnitFile: string;
  IsSys, DoGenCode: Boolean): TModule;

  procedure GenCode(M: TModule);
  var
    cg: TCodeGen;
  begin
    cg := TCodeGen.Create;
    try
      cg.EmitModule(M, Self);
      M.Codes := cg.GetIR;
    finally
      cg.Free;
    end;
  end;

var
  Parser: TParser;
begin
  parser := TParser.Create(Self);
  try
    parser.FIsSystemUnit := IsSys;
    parser.OpenFile(SrcFile);
    parser.OnError := Self.OnError;
    Result := parser.Parse;
    HasError := parser.ErrorCount > 0;
    if not HasError then
    begin
      TCUWriter(FCUWriter).WriteModule(Result, UnitFile);
      if DoGenCode then GenCode(Result);
    end;
  finally
    parser.Free;
  end;
end;

constructor TCompileContext.Create;
begin
  FNodes := TList.Create;
  FNodes.Capacity := 1024;

  FPendingParsers := TList.Create;
  FPendingParsers.Capacity := 100;

  FOpenArrayTypes := TList.Create;
  FOpenArrayTypes.Capacity := 512;

  FModules := TSymbolTable.Create(nil);
  FCUReader := TCUReader.Create;
  FCUWriter := TCUWriter.Create;
  IncludeDirs := TStringList.Create;
  UnitDirs := TStringList.Create;
  FSystemUnit := TModule.Create;
  FSystemUnit.Name := 'System';
  AddPredefinedElements(FSystemUnit);
end;

function TCompileContext.CreateAstNode(Typ: TAstNodeClass): TAstNode;
begin
  Result := Typ.Create;
  FNodes.Add(Result);
end;

function TCompileContext.CreateSymbol(SymClass: TSymbolClass): TSymbol;
begin
  Result := SymClass.Create;
  FNodes.Add(Result);
end;

destructor TCompileContext.Destroy;
begin
  UnitDirs.Free;
  IncludeDirs.Free;
  FSystemUnit.Free;
  ClearNodes;
  ClearParsers;

  FNodes.Free;
  FPendingParsers.Free;
  FModules.Free;
  FOpenArrayTypes.Free;
  FCUReader.Free;
  FCUWriter.Free;
  inherited;
end;

function TCompileContext.GetOpenArrayType(
  const typ: TType): TOpenArrayType;

  function Lookup(typ: TType): TOpenArrayType;
  var
    i: Integer;
  begin
    for i := 0 to FOpenArrayTypes.Count - 1 do
    begin
      Result := TOpenArrayType(FOpenArrayTypes[i]);
      if Result.ElementType = typ then Exit;
    end;
    Result := nil;
  end;
begin
//  todo 1: 要考虑到非全局符号,不宜置于此处
  Result := Lookup(typ);
  if Result = nil then
  begin
    Result := TOpenArrayType(CreateSymbol(TOpenArrayType));
    Include(Result.Attr, saTemp);
    Result.ElementType := typ;
    FOpenArrayTypes.Add(Result);
  end;
end;

function TCompileContext.GetSubrangeType(
  Ordinal: TTypeCode): TSubrangeType;
begin
  case Ordinal of
    typShortint:  Result := FShortintRangeType;
    typByte:      Result := FByteRangeType;
    typSmallint:  Result := FSmallintRangeType;
    typWord:      Result := FWordRangeType;
    typLongint:   Result := FLongintRangeType;
    typLongWord:  Result := FLongWordRangeType;
    typInt64:     Result := FInt64RangeType;
    typUInt64:    Result := FUInt64RangeType;
    typBoolean..typLongBool: Result := FBoolRangeType;
    typAnsiChar:  Result := FCharRangeType;
    typWideChar:  Result := FWideCharRangeType;
  else
    Result := nil;
  end;
end;

function TCompileContext.GetSystemRoutine(
  Routine: TSystemRoutine): TFunction;
const
  FunStr: array[TSystemRoutine] of string = (
    '_IntOverflow', '_OutOfRange', '_IOCheck',
    '_RaiseExcept', '_SafecallCheck', '_HandleSafecallExcept',
    '_HandleCtorExcept',

    '_Int64Div', '_Int64Mod', '_Round', '_Trunc',

    '_AStrClr', '_AStrAddRef', '_AStrNew', '_AStrPtr', '_AStrLength',
    '_AStrComp', '_AStrEqual', '_AStrAsg', '_AStrAsgCopy', '_AStrSetLength',
    '_AStrCopy', '_AStrDelete', '_AStrInsert', '_AStrFromSStr', '_AStrFromWStr',
    '_AStrFromUStr', '_AStrFromACh', '_AStrFromWCh', '_AStrFromPACh',
    '_AStrFromPAChLen', '_AStrFromPWCh', '_AStrFromPWChLen',
    '_AStrFromAArray', '_AStrFromWArray', '_AStrCat', '_AStrCat3', '_AStrCatN',

    '_WStrClr', '_WStrAddRef', '_WStrNew', '_WStrPtr', '_WStrLength',
    '_WStrComp', '_WStrEqual', '_WStrAsg', '_WStrAsgCopy', '_WStrSetLength',
    '_WStrCopy', '_WStrDelete', '_WStrInsert', '_WStrFromSStr', '_WStrFromAStr',
    '_WStrFromUStr', '_WStrFromACh', '_WStrFromWCh', '_WStrFromPACh',
    '_WStrFromPAChLen', '_WStrFromPWCh', '_WStrFromPWChLen',
    '_WStrFromAArray', '_WStrFromWArray', '_WStrCat', '_WStrCat3', '_WStrCatN',

    '_UStrClr', '_UStrAddRef', '_UStrNew', '_UStrPtr', '_UStrLength',
    '_UStrComp', '_UStrEqual', '_UStrAsg', '_UStrAsgCopy', '_UStrSetLength',
    '_UStrCopy', '_UStrDelete', '_UStrInsert', '_UStrFromSStr', '_UStrFromAStr',
    '_UStrFromWStr', '_UStrFromACh', '_UStrFromWCh', '_UStrFromPACh',
    '_UStrFromPAChLen', '_UStrFromPWCh', '_UStrFromPWChLen',
    '_UStrFromAArray', '_UStrFromWArray', '_UStrCat', '_UStrCat3', '_UStrCatN',

    '_SStrComp', '_SStrEqual', '_SStrAsg',
    '_SStrCopy', '_SStrDelete', '_SStrInsert', '_SStrFromAStr', '_SStrFromWStr',
    '_SStrFromUStr', '_SStrFromACh', '_SStrFromWCh', '_SStrFromPACh',
    '_SStrFromPAChLen', '_SStrFromPWCh', '_SStrFromPWChLen',
    '_SStrFromAArray', '_SStrFromWArray', '_SStrCat', '_SStrCat3', '_SStrCatN',

    '_VarClr', '_VarAddRef', '_VarOp', '_VarNot', '_VarNeg', '_VarCopy',
    '_Var2AStr', '_Var2WStr', '_Var2UStr', '_Var2SStr',
    '_Var2Shortint', '_Var2Byte', '_Var2Smallint', '_Var2Word',
    '_Var2Longint', '_Var2LongWord', '_Var2Int64', '_Var2UInt64',
    '_Var2Single', '_Var2Double', '_Var2Currency', '_Var2DateTime',

    '_RecordClr', '_RecordInit', '_RecordAddRef',

    '_ArrayClr', '_ArrayInit', '_ArrayAddRef',

    '_IntfClr', '_IntfAddRef',

    '_DynArrayClr', '_DynArrayAddRef'
  );
var
  Sym: TSymbol;
begin
  Result := FSystemRoutines[Routine];
  if Result = nil then
  begin
    Sym := FSystemUnit.FindSymbol(FunStr[Routine]);
    if Assigned(Sym) and (Sym.NodeKind = nkFunc) then
      Result := TFunction(Sym)
    else
      raise ECompileContextError.CreateFmt('System routine %s not found', [FunStr[Routine]]);

    FSystemRoutines[Routine] := Result;
    // 给某些函数添加属性, 便于llvm优化.
    case Routine of
      srIntOverflow, srOutOfRange,
      srRaiseExcept, srHandleCtorExcept:
        Include(Result.Modifiers, fmNoReturn);
    end;
  end;
end;

function TCompileContext.GetUnitFile(const APasFile: string): String;
begin
  if Self.UnitOutputDir = '' then
    Result := ChangeFileExt(APasFile, '.cu')
  else begin
    Result := UnitOutputDir + ExtractFileName(APasFile);
    Result := ChangeFileExt(Result, '.cu');
  end;
end;

procedure TCompileContext.LoadSystemUnit;

  function GetSymbol(const s: string;
                      ExpectKind: TAstNodeKind;
                      ExpectType: TTypeCode): TSymbol;
  begin
    Result := FSystemUnit.FindSymbol(s);
    if Result = nil then
      raise ECompileContextError.CreateFmt('Identifier %s not found in system unit', [s]);
    if ExpectKind <> nkSymbol then
      if Result.NodeKind <> ExpectKind then
        raise ECompileContextError.CreateFmt('Invalid symbol: %s', [s]);
    if ExpectType <> typUnknown then
      if TType(Result).TypeCode <> ExpectType then
        raise ECompileContextError.CreateFmt('Invalid type: %s', [s]);
  end;

var
  Reader: TCUReader;
begin
  if FSystemLoaded then Exit;
  reader := TCUReader.Create;
  try
    reader.Open('system.cu');
    reader.ReadModule(FSystemUnit, Self);
    Self.FTObjectType := TClassType(GetSymbol('TObject', nkType, typClass));
    Self.FIUnknownType := TInterfaceType(GetSymbol('IInterface', nkType, typInterface));
    Self.FIDispatchType := TInterfaceType(GetSymbol('IDispatch', nkType, typInterface));
    Self.FVarDataType := TType(GetSymbol('TVarData', nkType, typRecord));
    Self.FVarRecType := TType(GetSymbol('TVarRec', nkType, typRecord));
    Self.FDateTimeType := TType(GetSymbol('TDateTime', nkType, typClonedType));

    FSystemLoaded := True;
  finally
    reader.Free;
  end;
end;

function TCompileContext.LoadUnit(const UnitName: string): TModule;

  // 查找单元文件
  function LookupFile(const Ext: string): string;
  var
    I: Integer;
  begin
    for I := 0 to UnitDirs.Count - 1 do
    begin
      Result := UnitDirs[I] + LowerCase(UnitName) + Ext;
      if FileExists(Result) then Exit;
    end;
    Result := '';
  end;

  function DoLoad(const UnitFile: string): TModule;
  var
    Reader: TCUReader;
  begin
    Result := TModule(CreateSymbol(TModule));
    Reader := TCUReader.Create;
    try
      Result.State := msLoading;
      try
        Reader.Open(UnitFile);
        Reader.ReadModule(Result, Self);
      //  Reader.Close;
      except
        on EDifferenceVersion do raise;
        on E: Exception do
          raise ECompileContextError.Create(E.Message);
      end;
      Result.State := msNone;
    finally
      Reader.Free;
    end;
  end;

  function DoRecompile(const SrcFile, UnitFile: string): TModule;
  var
    Parser: TParser;
  begin
    parser := TParser.Create(Self);
    FPendingParsers.Add(parser);

    Result := TModule(CreateSymbol(TModule));
    Result.State := msIntfCompiling;
    GetFileTimeStamp(SrcFile, Result.TimeStamp);

    parser.OpenFile(SrcFile);
    parser.ParseUnitInterface(Result);

    Result.State := msNone;
    TCUWriter(FCUWriter).WriteModule(Result, UnitFile);
  end;

  function NeedRecompile(const SrcFile, UnitFile: string): Boolean;
  var
    SrcTime: TFileTimeStamp;
    Header: TModuleHeader;
  begin
    if not FileExists(UnitFile) then
    begin
      Result := True;
      Exit;
    end;

    try
      TCUReader(FCUReader).Open(UnitFile);
      TCUReader(FCUReader).GetHeader(Header);
      TCUReader(FCUReader).Close;
      GetFileTimeStamp(SrcFile, SrcTime);
      // todo 1: 以后可能要比较更多的内容
      Result := (SrcTime.Date <> Header.TimeStamp.Date) or
                (SrcTime.Time <> Header.TimeStamp.Time);
    except
      Result := True;
    end;
  end;
var
  SrcFile, UnitFile: string;
begin
  Result := FModules.Find(UnitName) as TModule;
  if Result <> nil then Exit;

  // 先找源码,如果不存在,则查找.cu
  // 如果cu和源码都存在,
  SrcFile := LookupFile('.pas');
  if SrcFile = '' then
  begin
    UnitFile := LookupFile('.cu');
    if UnitFile = '' then
      raise ECompileContextError.CreateFmt('Unit %s not found', [UnitName]);
    Result := DoLoad(UnitFile);
  end
  else
  begin
    if Self.UnitOutputDir = '' then
      UnitFile := ChangeFileExt(SrcFile, '.cu')
    else
      UnitFile := UnitOutputDir + UnitName + '.cu';

    if NeedRecompile(SrcFile, UnitFile) then
      Result := DoRecompile(SrcFile, UnitFile)
    else begin
      try
        Result := DoLoad(UnitFile);
      except
        on EDifferenceVersion do
          Result := DoRecompile(SrcFile, UnitFile);
      end;
    end;
  end;
end;

function TCompileContext.TypeOfRange(r1, r2: Int64): TType;
var
  Size1, Size2: Integer;
begin
  if r1 < 0 then
  begin
    if r1 < (-2147483647-1) then
      Size1 := 8
    else if r1 < -32768 then
      Size1 := 4
    else if r1 < -128 then
      Size1 := 2
    else
      Size1 := 1;
    if r2 > 2147483647 then
      Size2 := 8
    else if r2 > 32767 then
      Size2 := 4
    else if r2 > 127 then
      Size2 := 2
    else
      Size2 := 1;

    if Size2 < Size1 then Size2 := Size1;

    case Size2 of
      8: Result := FTypes[typInt64];
      4: Result := FTypes[typLongint];
      2: Result := FTypes[typSmallint];
    else
      Result := FTypes[typShortint];
    end;
  end else
  begin
    if r2 > $ffffffff then
      Result := FTypes[typInt64]
    else if r2 > $7fffffff then
      Result := FTypes[typLongWord]
    else if r2 > $ffff then
      Result := FTypes[typLongint]
    else if r2 > $7fff then
      Result := FTypes[typWord]
    else if r2 > $ff then
      Result := FTypes[typSmallint]
    else if r2 > $7f then
      Result := FTypes[typByte]
    else
      Result := FTypes[typShortint]
  end;
end;

end.
