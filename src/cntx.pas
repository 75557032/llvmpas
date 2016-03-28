unit cntx;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast;

type
  ECompileContextError = class(Exception);
  EDifferenceVersion = class(Exception);

  { EParserError }

  EParserError = class(Exception)
  private
    FFileName: String;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFileName: String;
      ARow, AColumn: Integer);
    constructor CreateCoord(const Coord: TAstNodeCoord; const AReason: string);
    constructor CreateCoordFmt(const Coord: TAstNodeCoord; const AReason: string; const Args: array of const);
    property FileName: String read FFileName;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;

  ENotConstant = class(EParserError);
  EEvalConstant = class(EParserError);
  EParseStop = class(EParserError);

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
    srHandleCtorExcept, srTerminated, srHandleFinally,
    srRethrow, srIsClass, srAsClass, srFreeAndNil,

    srInt64Div, srInt64Mod, srRound, srTrunc,

    srAStrClr, srAStrAddRef, srAStrNew, srAStrPtr, srAStrLength,
    srAStrAsg, srAStrAsgCopy, srAStrSetLength, srAStrCopy, srAStrDelete,
    srAStrInsert, srAStrFromSStr, srAStrFromWStr,
    srAStrFromUStr, srAStrFromACh, srAStrFromWCh, srAStrFromPACh,
    srAStrFromPAChLen, srAStrFromPWCh, srAStrFromPWChLen,
    srAStrFromAArray, srAStrFromWArray, srAStrCat, srAStrCat3, srAStrCatN,

    srWStrClr, srWStrAddRef, srWStrNew, srWStrPtr, srWStrLength,
    srWStrAsg, srWStrAsgCopy, srWStrSetLength, srWStrCopy, srWStrDelete,
    srWStrInsert, srWStrFromSStr, srWStrFromAStr,
    srWStrFromUStr, srWStrFromACh, srWStrFromWCh, srWStrFromPACh,
    srWStrFromPAChLen, srWStrFromPWCh, srWStrFromPWChLen,
    srWStrFromAArray, srWStrFromWArray, srWStrCat, srWStrCat3, srWStrCatN,

    srUStrClr, srUStrAddRef, srUStrNew, srUStrPtr, srUStrLength,
    srUStrAsg, srUStrAsgCopy, srUStrSetLength, srUStrCopy, srUStrDelete,
    srUStrInsert, srUStrFromSStr, srUStrFromAStr,
    srUStrFromWStr, srUStrFromACh, srUStrFromWCh, srUStrFromPACh,
    srUStrFromPAChLen, srUStrFromPWCh, srUStrFromPWChLen,
    srUStrFromAArray, srUStrFromWArray, srUStrCat, srUStrCat3, srUStrCatN,

    srSStrClr, srSStrLength, srSStrAsg, srSStrCopy, srSStrSetLength,
    srSStrDelete, srSStrInsert,
    srSStrFromAStr, srSStrFromWStr, srSStrFromUStr, srSStrFromACh,
    srSStrFromWCh, srSStrFromPACh, srSStrFromPAChLen, srSStrFromPWCh,
    srSStrFromPWChLen, srSStrFromAArray, srSStrFromWArray, srSStrCat,
    srSStrCat3, srSStrCatN,

    srSWStrClr, srSWStrLength, srSWStrAsg, srSWStrCopy, srSWStrSetLength,
    srSWStrDelete, srSWStrInsert,
    srSWStrFromAStr, srSWStrFromWStr, srSWStrFromUStr, srSWStrFromACh,
    srSWStrFromWCh, srSWStrFromPACh, srSWStrFromPAChLen, srSWStrFromPWCh,
    srSWStrFromPWChLen, srSWStrFromAArray, srSWStrFromWArray, srSWStrCat,
    srSWStrCat3, srSWStrCatN,

    srAStrComp, srWStrComp, srUStrComp, srSStrComp, srSWStrComp,
    srAArrComp, srWArrComp,

    srAStrCompWStr, srAStrCompUStr, srAStrCompSStr, srAStrCompSWStr,
    srAStrCompPa, srAStrCompPw, srAStrCompAarr, srAStrCompWarr,
    srAStrCompACh, srAStrCompWCh,

    srWStrCompAStr, srWStrCompUStr, srWStrCompSStr, srWStrCompSWStr,
    srWStrCompPa, srWStrCompPw, srWStrCompAarr, srWStrCompWarr,
    srWStrCompACh, srWStrCompWCh,

    srUStrCompAStr, srUStrCompWStr, srUStrCompSStr, srUStrCompSWStr,
    srUStrCompPa, srUStrCompPw, srUStrCompAarr, srUStrCompWarr,
    srUStrCompACh, srUStrCompWCh,

    srSStrCompAStr, srSStrCompWStr, srSStrCompUStr, srSStrCompSWStr,
    srSStrCompPa, srSStrCompPw, srSStrCompAarr, srSStrCompWarr,
    srSStrCompACh, srSStrCompWCh,

    srSWStrCompAStr, srSWStrCompWStr, srSWStrCompUStr, srSWStrCompSStr,
    srSWStrCompPa, srSWStrCompPw, srSWStrCompAarr, srSWStrCompWarr,
    srSWStrCompACh, srSWStrCompWCh,

    srPaCompAStr, srPaCompWStr, srPaCompUStr, srPaCompSStr,
    srPaCompSWStr, srPaCompAarr, srPaCompWarr, srPaCompACh, srPaCompWCh,

    srPwCompAStr, srPwCompWStr, srPwCompUStr, srPwCompSStr,
    srPwCompSWStr, srPwCompAarr, srPwCompWarr, srPwCompACh, srPwCompWCh,

    srAarrCompAStr, srAarrCompWStr, srAarrCompUStr, srAarrCompSStr,
    srAarrCompSWStr, srAarrCompPa, srAarrCompPw, srAarrCompWarr,
    srAarrCompACh, srAarrCompWCh,

    srWarrCompAStr, srWarrCompWStr, srWarrCompUStr, srWarrCompSStr,
    srWarrCompSWStr, srWarrCompPa, srWarrCompPw, srWarrCompAarr,
    srWarrCompACh, srWarrCompWCh,

    srAChCompAStr, srAChCompWStr, srAChCompUStr, srAChCompSStr,
    srAChCompSWStr, srAChCompPa, srAChCompPw, srAChCompAarr,
    srAChCompWarr,

    srWChCompAStr, srWChCompWStr, srWChCompUStr, srWChCompSStr,
    srWChCompSWStr, srWChCompPa, srWChCompPw, srWChCompAarr,
    srWChCompWarr,

    srVarClr, srVarAddRef, srVarOp, srVarNot, srVarNeg, srVarComp,
    srVarCopy, srVarArrayGet, srVarArraySet,
    srVarFromShortint, srVarFromByte, srVarFromSmallint, srVarFromWord,
    srVarFromLongint, srVarFromLongWord, srVarFromInt64, srVarFromUInt64,
    srVarFromAChr, srVarFromWChr,
    srVarFromReal, srVarFromBool, srVarFromDateTime, srVarFromCurr,
    srVarFromPAChr, srVarFromPWChr, srVarFromAStr,
    srVarFromWStr, srVarFromUStr, srVarFromSStr, srVarFromSWStr,
    srVarFromIntf, srVarFromDisp, srVarFromDynArr,

    srVar2AStr, srVar2WStr, srVar2UStr, srVar2SStr,
    srVar2Shortint, srVar2Byte, srVar2Smallint, srVar2Word,
    srVar2Longint, srVar2LongWord, srVar2Int64, srVar2UInt64,
    srVar2Single, srVar2Double, srVar2Currency, srVar2DateTime,
    srVar2Intf, srVar2Disp, srVar2DynArr,

    srOleVarFromPAChr, srOleVarFromPWChr, srOleVarFromAStr,
    srOleVarFromWStr, srOleVarFromUStr, srOleVarFromSStr,
    srOleVarFromVar, srOleVarFromInt,

    srRecordClr, srRecordInit, srRecordAddRef, srRecordCopy,

    srArrayClr, srArrayInit, srArrayAddRef, srArrayCopy,

    srIntfClr, srIntfAddRef, srIntfCopy, srIntfCast,

    srDynArrayClr, srDynArrayAddRef, srDynArrayAsg,

    srSetIn, srSetUnion, srSetSub, srSetInterset, srSetRange, srSetElem,
    srSetNE, srSetEQ, srSetLE, srSetGE, srSetInclude, srSetExclude,
    srSetCopy, srSetInflate, srSetExpand,
    srNSetIn, srNSetUnion, srNSetSub, srNSetInterset, srNSetRange, srNSetElem,
    srNSetNE, srNSetEQ, srNSetLE, srNSetGE, srNSetInclude, srNSetExclude
  );

  { TCompileContext }

  TCompileContext = class
  private
    FOpenArrayTypes: TList;
    FSystemLoaded: Boolean;
    FCUReader, FCUWriter: TObject;
    FOnError: TParserErrorEvent;

    FCachedUnary, FCachedBinary, FCachedList,
    FCachedConst, FCachedSymbol: TList;
  //  FLoadingUnits: TList;
    FPendingParsers: TList;
    FNodes: TList;
    procedure AddBuiltinProcs(M: TModule);
    procedure AddTypes(M: TModule);
    procedure AddInternalTypes(M: TModule);
    procedure AddConstants(M: TModule);
    procedure AddPredefinedElements(M: TModule);
    procedure ClearNodes;
    procedure ClearParsers;
    function GetCachedExpr(List: TList): TExpr;
  public
    FPointerSize: Integer;       // 4 or 8
    FModules: TSymbolTable;
    FSystemUnit: TModule;

    FShortIntType, FByteType, FSmallIntType, FWordType,
    FLongIntType, FLongWordType, FInt64Type, FUInt64Type: TIntType;

    FDoubleType, FSingleType, FCurrencyType, FCompType, FExtendedType: TNumericType;

    FAnsiCharType, FWideCharType: TCharType;

    FBooleanType, FByteBoolType, FWordBoolType, FLongBoolType: TBoolType;

    FAnsiStringType, FWideStringType,
    FUnicodeStringType, FShortStringType: TStringType;

    FVariantType: TVariantType;
    FOleVariantType: TVariantType;

    FPAnsiCharType, FPWideCharType: TType;
    // 通用类型
    FStringType: TStringType;
    FCharType: TCharType;
    FIntegerType: TIntType;
    FCardinalType: TIntType;
    FRealType: TNumericType;
    FNativeIntType, FNativeUIntType: TIntType;
    FPCharType: TType;
    FPointerType: TType;
    FFileType: TType;

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
    FEmptySetVar: TVariable;

    FNoopFunc: TSymbol;
    FSystemRoutines: array [TSystemRoutine] of TFunction;

  public
  //  DefaultCodePage: Word;
    IncDirs, LibDirs: TStringList;
    UnitOutputDir: string;
    IsSystemUnit: Boolean;

    HasError: Boolean;

    constructor Create;
    destructor Destroy; override;

    function CreateAstNode(Typ: TAstNodeClass): TAstNode;
    function GetCachedUnary: TUnaryExpr;
    function GetCachedBinary: TBinaryExpr;
    function GetCachedList: TListExpr;
    function GetCachedConst: TConstExpr;
    function GetCachedSymbol: TSymbolExpr;
    procedure ReleaseExpr(E: TExpr);

    function CreateSymbol(SymClass: TSymbolClass): TSymbol;

    function GetSubrangeType(Ordinal: TType): TSubrangeType;
    function TypeOfRange(r1, r2: Int64): TType;

    procedure AddNode(Node: TAstNode);
    procedure LoadSystemUnit;
    procedure ResolveSystemSymbols;
    function LoadUnit(const UnitName: string): TModule; overload;
    function GetSystemRoutine(Routine: TSystemRoutine): TFunction;
//    function LoadUnit(const UnitName: string; const ExpectTimeStamp: TTimeStamp): TModule; overload;
    function Compile(const SrcFile: string): TModule;
    function GetOpenArrayType(const typ: TType): TOpenArrayType;
    function GetUnitFile(const APasFile: string): String;
    procedure GenCode(Func: TFunction);
    property OnError: TParserErrorEvent read FOnError write FOnError;
  end;

implementation

uses cupersist, parser, fileutils, llvm_codegen, dump;

{ EParserError }

constructor EParserError.Create(const AReason, AFileName: String; ARow,
  AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

constructor EParserError.CreateCoord(const Coord: TAstNodeCoord;
  const AReason: string);
begin
  inherited Create(AReason);
  FFilename := Coord.FileName;
  FRow := Coord.Row;
  FColumn := Coord.Col;
end;

constructor EParserError.CreateCoordFmt(const Coord: TAstNodeCoord;
  const AReason: string; const Args: array of const);
begin
  inherited CreateFmt(AReason, Args);
  FFilename := Coord.FileName;
  FRow := Coord.Row;
  FColumn := Coord.Col;
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
  FNoopFunc := CreateFunc('$noop', bfNoop);
  M.Symbols.Add(FNoopFunc);
end;

procedure TCompileContext.AddConstants(M: TModule);
begin
  FFalseConst := TConstant(CreateAstNode(TConstant));
  FFalseConst.Name := 'False';
  FFalseConst.ConstType := FBooleanType;
  FFalseConst.Value := ValFromBool(False);
  M.Symbols.Add(FFalseConst);

  FTrueConst := TConstant(CreateAstNode(TConstant));
  FTrueConst.Name := 'True';
  FTrueConst.ConstType := FBooleanType;
  FTrueConst.Value := ValFromBool(True);
  M.Symbols.Add(FTrueConst);

  FEmptySetVar := TVariable(CreateAstNode(TVariable));
  FEmptySetVar.Name := '_EmptySet';
  FEmptySetVar.VarType := FByteSetType;
  M.Symbols.Add(FEmptySetVar);
end;

procedure TCompileContext.AddInternalTypes(M: TModule);

  function CreateRangeType(BaseType: TType; const Name: string; R1, R2: Int64): TSubrangeType;
  begin
    Result := TSubrangeType(CreateAstNode(TSubrangeType));
    Result.BaseType := BaseType;
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
  FShortintRangeType := CreateRangeType(FShortIntType, '$ShortintRange', -128, 127);
  FSmallintRangeType := CreateRangeType(FSmallIntType, '$SmallintRange', -32768, 32767);
  FLongintRangeType := CreateRangeType(FLongIntType, '$LongintRange', Longint($80000000), $7fffffff);
  FInt64RangeType := CreateRangeType(FInt64Type, '$Int64Range', Int64($8000000000000000), $7fffffffffffffff);
  FByteRangeType := CreateRangeType(FByteType, '$ByteRange', 0, 255);
  FWordRangeType := CreateRangeType(FWordType, '$WordRange', 0, $ffff);
  FLongWordRangeType := CreateRangeType(FLongWordType, '$LongWordRange', 0, $ffffffff);
  FUInt64RangeType := CreateRangeType(FUInt64Type, '$UInt64Range', 0, $ffffffffffffffff);
  FCharRangeType := CreateRangeType(FAnsiCharType, '$AnsiCharRange', 0, 255);
  FWideCharRangeType := CreateRangeType(FWideCharType, '$WideCharRange', 0, $ffff);
  FBoolRangeType := CreateRangeType(FBooleanType, '$BooleanRange', 0, 1);

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

procedure TCompileContext.AddNode(Node: TAstNode);
begin
  FNodes.Add(Node);
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

const
  IntSizes: array[TIntKind] of Byte = (
    1, 1, 2, 2, 4, 4, 8, 8
  );
  NumSizes: array[TNumericKind] of Byte = (
    //numSingle, numDouble, numExtended, numCurrency, numComp, numReal48
    4, 8, 10, 8, 8, 6
  );
  CharSizes: array[TCharKind] of Byte = (1, 2);
  BoolSizes: array[TBoolKind] of Byte = (1, 1, 2, 4);

  procedure CreateInt(var typ: TIntType; Kind: TIntKind; const Name: TSymString);
  begin
    typ := TIntType(CreateAstNode(TIntType));
    typ.Kind := Kind;
    typ.Name := Name;
    typ.Size := IntSizes[Kind];
    M.Add(typ);
  end;

  procedure CreateNum(var typ: TNumericType; Kind: TNumericKind; const Name: TSymString);
  begin
    typ := TNumericType(CreateAstNode(TNumericType));
    typ.Kind := Kind;
    typ.Name := Name;
    typ.Size := NumSizes[Kind];
    M.Add(typ);
  end;

  procedure CreateChar(var typ: TCharType; Kind: TCharKind; const Name: TSymString);
  begin
    typ := TCharType(CreateAstNode(TCharType));
    typ.Kind := Kind;
    typ.Name := Name;
    typ.Size := CharSizes[Kind];
    M.Add(typ);
  end;

  procedure CreateBool(var typ: TBoolType; Kind: TBoolKind; const Name: TSymString);
  begin
    typ := TBoolType(CreateAstNode(TBoolType));
    typ.Kind := Kind;
    typ.Name := Name;
    typ.Size := BoolSizes[Kind];
    M.Add(typ);
  end;

  function CreateStrType(const AName: TSymString; AKind: TStringKind; Size: Cardinal): TStringType;
  begin
    Result := TStringType.Create(AKind);
    FNodes.Add(Result);
    Result.Name := AName;
    Result.Size := Size;
    M.Add(Result);
  end;

  function CreateVariantType(const AName: string; AIsOle: Boolean): TVariantType;
  begin
    Result := TVariantType.Create;
    Result.Name := AName;
    Result.IsOle := AIsOle;
    Result.Size := 16;
    FNodes.Add(Result);
    M.Add(Result);
  end;

  function CreateType(size: Cardinal; code: TTypeCode;
      const name: string): TType; overload;
  begin
    Result := TPrimitiveType.Create(code);
    Result.Name := name;
    Result.Size := size;
    FNodes.Add(Result);
    M.Add(Result);
  end;

  function CreateType(typ: TTypeClass; size: Cardinal;
      const name: string): TType; overload;
  begin
    Result := typ.Create;
    Result.Name := name;
    Result.Size := size;
    FNodes.Add(Result);
    M.Add(Result);
  end;

begin
  CreateInt(FShortIntType, intS8, 'Shortint');
  CreateInt(FByteType, intU8, 'Byte');
  CreateInt(FSmallIntType, intS16, 'Smallint');
  CreateInt(FWordType, intU16, 'Word');
  CreateInt(FLongIntType, intS32, 'Longint');
  CreateInt(FLongWordType, intU32, 'LongWord');
  CreateInt(FInt64Type, intS64, 'Int64');
  CreateInt(FUInt64Type, intU64, 'UInt64');

  CreateNum(FDoubleType, numDouble, 'Double');
  CreateNum(FSingleType, numSingle, 'Single');
  CreateNum(FCurrencyType, numCurrency, 'Currency');
  CreateNum(FCompType, numComp, 'Comp');
  CreateNum(FExtendedType, numExtended, 'Extended');

  CreateChar(FAnsiCharType, charAnsi, 'AnsiChar');
  CreateChar(FWideCharType, charWide, 'WideChar');

  CreateBool(FBooleanType, bolStd, 'Boolean');
  CreateBool(FByteBoolType, bolByte, 'ByteBool');
  CreateBool(FWordBoolType, bolWord, 'WordBool');
  CreateBool(FLongBoolType, bolLong, 'LongBool');

  // general types / alias
  CreateInt(FIntegerType, intS32, 'Integer');
  CreateInt(FCardinalType, intU32, 'Cardinal');
  CreateChar(FCharType, charAnsi, 'Char');
  CreateNum(FRealType, numDouble, 'Real');

  if FPointerSize = 4 then
  begin
    CreateInt(FNativeIntType, intS32, 'NativeInt');
    CreateInt(FNativeUIntType, intU32, 'NativeUInt');
  end
  else
  begin
    CreateInt(FNativeIntType, intS64, 'NativeInt');
    CreateInt(FNativeUIntType, intU64, 'NativeUInt');
  end;

  FVariantType := CreateVariantType('Variant', False);
  FOleVariantType := CreateVariantType('OleVariant', True);

  FPAnsiCharType := CreateType(FPointerSize, typPAnsiChar, 'PAnsiChar');
  FPWideCharType := CreateType(FPointerSize, typPWideChar, 'PWideChar');

  FPointerType := CreateType(TPointerType, FPointerSize, 'Pointer');
  FFileType := CreateType(TFileType, FPointerSize, 'File');

  // String types
  FAnsiStringType := CreateStrType('AnsiString', strAnsi, FPointerSize);
  FWideStringType := CreateStrType('WideString', strWide, FPointerSize);
  FUnicodeStringType := CreateStrType('UnicodeString', strUnicode, FPointerSize);
  FShortStringType := CreateStrType('ShortString', strAShort, 256);
  FShortStringType.Update;
//  FShortStringType.CharCount := 255;

  // general types / alias
  FStringType := CreateStrType('String', strAnsi, FPointerSize);

  FPCharType := CreateType(FPointerSize, typPAnsiChar, 'PChar');

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

function TCompileContext.Compile(const SrcFile: string): TModule;
 {
  procedure GenCode(M: TModule);
  var
    cg: TCodeGen;
  begin
    cg := TCodeGen.Create(Self);
    try
      cg.EmitModuleDecl(M);
      M.Codes := cg.GetIR;
    finally
      cg.Free;
    end;
  end;

  procedure DoDumpAst(M: TModule);
  var
    d: TDump;
  begin
    d := TJsonDump.Create;
    try
      M.Dump := d.Dump(M, DumpAllOptions);
    finally
      d.Free;
    end;
  end;
  }
var
  Parser: TParser;
begin
  parser := TParser.Create(Self);
  try
    parser.FIsSystemUnit := IsSystemUnit;
    parser.OpenFile(SrcFile);
    parser.OnError := Self.OnError;
    Result := parser.Parse;
    HasError := parser.ErrorCount > 0;
    if not HasError then
    begin
      TCUWriter(FCUWriter).WriteModule(Result, GetUnitFile(SrcFile));
      //if DoGenCode then GenCode(Result);
      //if DumpAst then DoDumpAst(Result);
    end;
  finally
    parser.Free;
  end;
end;

constructor TCompileContext.Create;
begin
//  DefaultCodePage := GetThreadCodePage;
  FPointerSize := 4;
  FNodes := TList.Create;
  FNodes.Capacity := 1024;

  FPendingParsers := TList.Create;
  FPendingParsers.Capacity := 100;

  FOpenArrayTypes := TList.Create;
  FOpenArrayTypes.Capacity := 512;

  FCachedUnary := TList.Create;
  FCachedUnary.Capacity := 8;
  FCachedBinary := TList.Create;
  FCachedBinary.Capacity := 32;
  FCachedList := TList.Create;
  FCachedList.Capacity := 8;
  FCachedConst := TList.Create;
  FCachedConst.Capacity := 32;
  FCachedSymbol := TList.Create;
  FCachedSymbol.Capacity := 32;

  FModules := TSymbolTable.Create(nil);
  FCUReader := TCUReader.Create;
  FCUWriter := TCUWriter.Create;
  IncDirs := TStringList.Create;
  LibDirs := TStringList.Create;
  FSystemUnit := TModule.Create;
  FSystemUnit.Name := 'System';
  AddPredefinedElements(FSystemUnit);
end;

function TCompileContext.CreateAstNode(Typ: TAstNodeClass): TAstNode;
begin
  Result := Typ.Create;
  FNodes.Add(Result);
end;

function TCompileContext.GetCachedBinary: TBinaryExpr;
begin
  Result := TBinaryExpr(GetCachedExpr(FCachedBinary));
end;

function TCompileContext.GetCachedConst: TConstExpr;
begin
  Result := TConstExpr(GetCachedExpr(FCachedConst));
end;

function TCompileContext.GetCachedList: TListExpr;
begin
  Result := TListExpr(GetCachedExpr(FCachedList));
end;

function TCompileContext.GetCachedSymbol: TSymbolExpr;
begin
  Result := TSymbolExpr(GetCachedExpr(FCachedSymbol));
end;

function TCompileContext.GetCachedUnary: TUnaryExpr;
begin
  Result := TUnaryExpr(GetCachedExpr(FCachedUnary));
end;

function TCompileContext.CreateSymbol(SymClass: TSymbolClass): TSymbol;
begin
  Result := SymClass.Create;
  FNodes.Add(Result);
end;

destructor TCompileContext.Destroy;
begin
  LibDirs.Free;
  IncDirs.Free;
  FSystemUnit.Free;
  ClearNodes;
  ClearParsers;

  FCachedUnary.Free;
  FCachedBinary.Free;
  FCachedList.Free;
  FCachedConst.Free;
  FCachedSymbol.Free;

  FNodes.Free;
  FPendingParsers.Free;
  FModules.Free;
  FOpenArrayTypes.Free;
  FCUReader.Free;
  FCUWriter.Free;
  inherited;
end;

procedure TCompileContext.GenCode(Func: TFunction);
var
  cg: TCodeGen;
begin
  cg := TCodeGen.Create(Self);
  try
    cg.Emit(Func);
  finally
    cg.Free;
  end;
end;

function TCompileContext.GetCachedExpr(List: TList): TExpr;
begin
  if List.Count > 0 then
  begin
    Result := TExpr(List.Last);
    List.Delete(List.Count - 1);
  end
  else
    Result := nil;
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
  Ordinal: TType): TSubrangeType;
begin
  case Ordinal.TypeCode of
    typInt:
      case TIntType(Ordinal).Kind of
        intS8:  Result := FShortintRangeType;
        intU8:  Result := FByteRangeType;
        intS16: Result := FSmallintRangeType;
        intU16: Result := FWordRangeType;
        intS32: Result := FLongintRangeType;
        intU32: Result := FLongWordRangeType;
        intS64: Result := FInt64RangeType;
        intU64: Result := FUInt64RangeType;
      else
        Result := FLongintRangeType;
      end;
    typBool: Result := FBoolRangeType;
    typChar:
      if TCharType(Ordinal).Kind = charAnsi then
        Result := FCharRangeType
      else
        Result := FWideCharRangeType;
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
    '_HandleCtorExcept', '_Terminated', '_HandleFinally',
    '_Rethrow', '_IsClass', '_AsClass', 'FreeAndNil',

    '_Int64Div', '_Int64Mod', '_Round', '_Trunc',

    '_AStrClr', '_AStrAddRef', '_AStrNew', '_AStrPtr', '_AStrLength',
    '_AStrAsg', '_AStrAsgCopy', '_AStrSetLength', '_AStrCopy', '_AStrDelete',
    '_AStrInsert', '_AStrFromSStr', '_AStrFromWStr',
    '_AStrFromUStr', '_AStrFromACh', '_AStrFromWCh', '_AStrFromPACh',
    '_AStrFromPAChLen', '_AStrFromPWCh', '_AStrFromPWChLen',
    '_AStrFromAArray', '_AStrFromWArray', '_AStrCat', '_AStrCat3', '_AStrCatN',

    '_WStrClr', '_WStrAddRef', '_WStrNew', '_WStrPtr', '_WStrLength',
    '_WStrAsg', '_WStrAsgCopy', '_WStrSetLength', '_WStrCopy', '_WStrDelete',
    '_WStrInsert', '_WStrFromSStr', '_WStrFromAStr',
    '_WStrFromUStr', '_WStrFromACh', '_WStrFromWCh', '_WStrFromPACh',
    '_WStrFromPAChLen', '_WStrFromPWCh', '_WStrFromPWChLen',
    '_WStrFromAArray', '_WStrFromWArray', '_WStrCat', '_WStrCat3', '_WStrCatN',

    '_UStrClr', '_UStrAddRef', '_UStrNew', '_UStrPtr', '_UStrLength',
    '_UStrAsg', '_UStrAsgCopy', '_UStrSetLength', '_UStrCopy', '_UStrDelete',
    '_UStrInsert', '_UStrFromSStr', '_UStrFromAStr',
    '_UStrFromWStr', '_UStrFromACh', '_UStrFromWCh', '_UStrFromPACh',
    '_UStrFromPAChLen', '_UStrFromPWCh', '_UStrFromPWChLen',
    '_UStrFromAArray', '_UStrFromWArray', '_UStrCat', '_UStrCat3', '_UStrCatN',

    '_SStrClr', '_SStrLength', '_SStrAsg', '_SStrCopy', '_SStrSetLength',
    '_SStrDelete', '_SStrInsert',
    '_SStrFromAStr', '_SStrFromWStr', '_SStrFromUStr', '_SStrFromACh',
    '_SStrFromWCh', '_SStrFromPACh', '_SStrFromPAChLen', '_SStrFromPWCh',
    '_SStrFromPWChLen', '_SStrFromAArray', '_SStrFromWArray', '_SStrCat',
    '_SStrCat3', '_SStrCatN',

    '_SWStrClr', '_SWStrLength', '_SWStrAsg', '_SWStrCopy', '_SWStrSetLength',
    '_SWStrDelete', '_SWStrInsert',
    '_SWStrFromAStr', '_SWStrFromWStr', '_SWStrFromUStr', '_SWStrFromACh',
    '_SWStrFromWCh', '_SWStrFromPACh', '_SWStrFromPAChLen', '_SWStrFromPWCh',
    '_SWStrFromPWChLen', '_SWStrFromAArray', '_SWStrFromWArray', '_SWStrCat',
    '_SWStrCat3', '_SWStrCatN',

    '_AStrComp', '_WStrComp', '_UStrComp', '_SStrComp', '_SWStrComp',
    '_AArrComp', '_WArrComp',

    '_AStrCompWStr', '_AStrCompUStr', '_AStrCompSStr', '_AStrCompSWStr',
    '_AStrCompPa', '_AStrCompPw', '_AStrCompAarr', '_AStrCompWarr',
    '_AStrCompACh', '_AStrCompWCh',

    '_WStrCompAStr', '_WStrCompUStr', '_WStrCompSStr', '_WStrCompSWStr',
    '_WStrCompPa', '_WStrCompPw', '_WStrCompAarr', '_WStrCompWarr',
    '_WStrCompACh', '_WStrCompWCh',

    '_UStrCompAStr', '_UStrCompWStr', '_UStrCompSStr', '_UStrCompSWStr',
    '_UStrCompPa', '_UStrCompPw', '_UStrCompAarr', '_UStrCompWarr',
    '_UStrCompACh', '_UStrCompWCh',

    '_SStrCompAStr', '_SStrCompWStr', '_SStrCompUStr', '_SStrCompSWStr',
    '_SStrCompPa', '_SStrCompPw', '_SStrCompAarr', '_SStrCompWarr',
    '_SStrCompACh', '_SStrCompWCh',

    '_SWStrCompAStr', '_SWStrCompWStr', '_SWStrCompUStr', '_SWStrCompSStr',
    '_SWStrCompPa', '_SWStrCompPw', '_SWStrCompAarr', '_SWStrCompWarr',
    '_SWStrCompACh', '_SWStrCompWCh',

    '_PaCompAStr', '_PaCompWStr', '_PaCompUStr', '_PaCompSStr',
    '_PaCompSWStr', '_PaCompAarr', '_PaCompWarr', '_PaCompACh', '_PaCompWCh',

    '_PwCompAStr', '_PwCompWStr', '_PwCompUStr', '_PwCompSStr',
    '_PwCompSWStr', '_PwCompAarr', '_PwCompWarr', '_PwCompACh', '_PwCompWCh',

    '_AarrCompAStr', '_AarrCompWStr', '_AarrCompUStr', '_AarrCompSStr',
    '_AarrCompSWStr', '_AarrCompPa', '_AarrCompPw', '_AarrCompWarr',
    '_AarrCompACh', '_AarrCompWCh',

    '_WarrCompAStr', '_WarrCompWStr', '_WarrCompUStr', '_WarrCompSStr',
    '_WarrCompSWStr', '_WarrCompPa', '_WarrCompPw', '_WarrCompAarr',
    '_WarrCompACh', '_WarrCompWCh',

    '_AChCompAStr', '_AChCompWStr', '_AChCompUStr', '_AChCompSStr',
    '_AChCompSWStr', '_AChCompPa', '_AChCompPw', '_AChCompAarr',
    '_AChCompWarr',

    '_WChCompAStr', '_WChCompWStr', '_WChCompUStr', '_WChCompSStr',
    '_WChCompSWStr', '_WChCompPa', '_WChCompPw', '_WChCompAarr',
    '_WChCompWarr',

    '_VarClr', '_VarAddRef', '_VarOp', '_VarNot', '_VarNeg', '_VarComp',
    '_VarCopy', '_VarArrayGet', '_VarArraySet',
    '_VarFromShortint', '_VarFromByte', '_VarFromSmallint', '_VarFromWord',
    '_VarFromLongint', '_VarFromLongWord', '_VarFromInt64', '_VarFromUInt64',
    '_VarFromAChr', '_VarFromWChr',
    '_VarFromReal', '_VarFromBool', '_VarFromDateTime', '_VarFromCurr',
    '_VarFromPAChr', '_VarFromPWChr', '_VarFromAStr',
    '_VarFromWStr', '_VarFromUStr', '_VarFromSStr', '_VarFromSWStr',
    '_VarFromIntf', '_VarFromDisp', '_VarFromDynArr',

    '_Var2AStr', '_Var2WStr', '_Var2UStr', '_Var2SStr',
    '_Var2Shortint', '_Var2Byte', '_Var2Smallint', '_Var2Word',
    '_Var2Longint', '_Var2LongWord', '_Var2Int64', '_Var2UInt64',
    '_Var2Single', '_Var2Double', '_Var2Currency', '_Var2DateTime',
    '_Var2Intf', '_Var2Disp', '_Var2DynArr',

    '_OleVarFromPAChr', '_OleVarFromPWChr', '_OleVarFromAStr',
    '_OleVarFromWStr', '_OleVarFromUStr', '_OleVarFromSStr',
    '_OleVarFromVar', '_OleVarFromInt',

    '_RecordClr', '_RecordInit', '_RecordAddRef', '_RecordCopy',

    '_ArrayClr', '_ArrayInit', '_ArrayAddRef', '_ArrayCopy',

    '_IntfClr', '_IntfAddRef', '_IntfCopy', '_IntfCast',

    '_DynArrayClr', '_DynArrayAddRef', '_DynArrayAsg',

    '_SetIn', '_SetUnion', '_SetSub', '_SetInterset', '_SetRange', '_SetElem',
    '_SetNE', '_SetEQ', '_SetLE', '_SetGE', '_SetInclude', '_SetExclude',
    '_SetCopy', '_SetInflate', '_SetExpand',
    '_NSetIn', '_NSetUnion', '_NSetSub', '_NSetInterset', '_NSetRange', '_NSetElem',
    '_NSetNE', '_NSetEQ', '_NSetLE', '_NSetGE', '_NSetInclude', '_NSetExclude'
  );
var
  Sym: TSymbol;
begin
  Result := FSystemRoutines[Routine];
  if Result = nil then
  begin
    Sym := FSystemUnit.FindSymbol(FunStr[Routine]);
    if Assigned(Sym) and (Sym.NodeKind in [nkFunc, nkExternalFunc]) then
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

  function GetSystemUnit: string;
  var
    i: Integer;
  begin
    for i := 0 to LibDirs.Count-1 do
    begin
      Result := LibDirs[i] + 'system.cu';
      if FileExists(Result) then Exit;
    end;
    Result := '';
  end;

var
  Reader: TCUReader;
  fn: string;
begin
  if FSystemLoaded then Exit;
  reader := TCUReader.Create;
  try
    fn := GetSystemUnit;
    if fn = '' then
      raise ECompileContextError.Create('system.cu not found');
    reader.Open(fn);
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
    for I := 0 to LibDirs.Count - 1 do
    begin
      Result := LibDirs[I] + LowerCase(UnitName) + Ext;
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

procedure TCompileContext.ReleaseExpr(E: TExpr);

  function KindOf(E: TExpr): TExprOpKind;
  begin
    if E is TUnaryExpr then
      Result := opkUnary
    else if E is TBinaryExpr then
      Result := opkBinary
    else if E is TSymbolExpr then
      Result := opkSymbol
    else if E is TListExpr then
      Result := opkList
    else if E is TConstExpr then
      Result := opkConst
  end;
var
  kind: TExprOpKind;
begin
  if E = nil then Exit;

  kind := OpKinds[E.OpCode];
  if kind = opkNone then kind := KindOf(E);
  case kind of
    opkUnary: FCachedUnary.Add(E);
    opkBinary: FCachedBinary.Add(E);
    opkConst: FCachedConst.Add(E);
    opkSymbol: FCachedSymbol.Add(E);
    opkList: FCachedList.Add(E);
  end;

  E.Reset;
end;

procedure TCompileContext.ResolveSystemSymbols;

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
begin
  if Self.FVarDataType = nil then
    Self.FVarDataType := TType(GetSymbol('TVarData', nkType, typRecord));
  if Self.FVarRecType = nil then
    Self.FVarRecType := TType(GetSymbol('TVarRec', nkType, typRecord));
  if Self.FDateTimeType = nil then
    Self.FDateTimeType := TType(GetSymbol('TDateTime', nkType, typClonedType));
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
      8: Result := FInt64Type;
      4: Result := FLongIntType;
      2: Result := FSmallIntType;
    else
      Result := FLongIntType;
    end;
  end else
  begin
    if r2 > $ffffffff then
      Result := FInt64Type
    else if r2 > $7fffffff then
      Result := FLongWordType
    else if r2 > $ffff then
      Result := FLongIntType
    else if r2 > $7fff then
      Result := FWordType
    else if r2 > $ff then
      Result := FSmallIntType
    else if r2 > $7f then
      Result := FByteType
    else
      Result := FShortIntType
  end;
end;

end.
