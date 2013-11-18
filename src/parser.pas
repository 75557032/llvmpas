unit parser;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, Variants, lex, ast, cntx, hashtable;

type
  TParseState = (
    psInIntfSect, psInImplSect, psInClass, psInRecord, psInObject,
    psInIntf, psInDispIntf, psInFunc, psInType, psInVar, psInField,
    psInAccessor, psInPacked, psInLeftVal, psInClassPrefix,
    psInWhileStmt, psInForStmt, psInForEachStmt, psInRepeatStmt,
    psInFuncBody
  );
  TParseStates = set of TParseState;

  TParseStateInfo = record
    State: TParseState;
    IsSet: Boolean;
  end;

  TFunctionHeader = class
  public
    Name: string;
    Names: array of string;
    CountOfNames: Integer;
    FileName, RoutineName, ImplementingName: string;
    RoutineNo: Integer;
    MsgNo: Integer;
    DispID: Integer;
    ReturnType: TType;
    Args: TList;
    PrevDecl: TFunctionDecl;
    Directives: TDirectiveIdents;
    Modifiers: TFunctionModifiers;
    CallConvention: TCallingConvention;
    Hints: TMemberHints;
    MethodKind: TMethodKind;
    ObjectKind: TObjectKind;
    ClassPrefix: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure Reset;
  end;

  TQualifiedID = class
  public
    Name: string;
    Names: array of string;
    CountOfNames: Integer;
    constructor Create;
    procedure Reset;
    function Id: string;
    function SameScope(const Scopes: array of string): Boolean;
  end;

  TParser = class
  private
    FOnError: TParserErrorEvent;
    FScanner: TScanner;
    FCurToken: TToken;
    FCurTokenString: string;
    FNodes: TList;
    FExternSymbols: TSymbolTable;
    FDefinedSymbols: THashTable;
    FCurExprList: TList;
    FTempExprList, FExprList: TList;
    FScopeList: TList;
    FWithList: TList;
    FCurOverloadFunc: TFunctionDecl;
    FCurSymbolPos: TSymbolPosition;

    // 处于非公开部分:如program或unit的implementation段
    FInternalSection: Boolean;

    // UngetToken support:
    FTokenBuffer: array[0..1] of TToken;
    FTokenStringBuffer: array[0..1] of string;
    FTokenBufferIndex: Integer; // current index in FTokenBuffer
    FTokenBufferSize: Integer; // maximum valid index in FTokenBuffer

    FCurVisibility: TMemberVisibility;
    FHeader: TFunctionHeader;
    FQId: TQualifiedId;
    FTemp: string;
    FTempValue: TValueRec;

    property Scanner: TScanner read FScanner;
    property CurToken: TToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
    procedure NextToken;
    procedure UngetToken;
    procedure Expect(T: TToken; Stop: Boolean = True);

    procedure OnScannerError(const Msg: string; Stop: Boolean);
    procedure OnScannerDirective(var dinfo: TDirectiveInfo);
    procedure OnScannerIfDefined(const S: string; out IsDefined: Boolean);
    procedure OnScannerIfOpt(C: Char; out IsSet: Boolean);
    procedure OnScannerIfEval(out IsDefined: Boolean);

    // 清除FExprList中的TExpr对象
    procedure ClearExprList;
    // 清除FTempExprList中的TExpr对象
    procedure ClearTempExprList;
    // 设置创建的TExpr是加入到FExprList还是FTempExprList
    // 为True则加到FTempExprList中,反之则加到FExprList
    // 一般在分析语句的时候,要设置为False
    // 而其它地方要设置为True,这样所创建的TExpr加到FTempExprList中,
    // 可以使用ClearTempExprList清除
    procedure SetTempExpr(Value: Boolean);
    function IsTempExpr: Boolean;

    procedure InitAstNode(ANode: TAstNode);
    procedure InitExpr(Expr: TExpr);
    procedure ClearNodes;

    // 添加某单元的符号到ExternSymbols列表中
    procedure AddSymbols(M: TModule);
    // 添加到当前Scope的符号表
    function AddSymbol(Sym: TSymbol): Boolean;
    // 当前符号表
    function CurSymbols: TSymbolTable;

    // scope
//    procedure EnterScope; overload;
    procedure EnterScope(SymTable: TSymbolTable); overload;
    procedure LeaveScope;
    procedure ClearScopes;

    // with scope
    procedure EnterWithStmt(Sym: TSymbolExpr);
    procedure LeaveWithStmt;
    procedure ClearWithList;

    function ParseTypeExpr: TExpr;
    function ParseExpr: TExpr;
    function ParseAddExpr: TExpr;
    function ParseMulExpr: TExpr;
    function ParseFactor: TExpr;
    function ParseDesignator: TExpr;
    function ParseSetConstructor: TExpr;
    function ParseSetElementList: TExpr;
    function ParseExprList: TExpr;
    function SimplifyQualId: TExpr;
    // 分析System.TObject之类的类型全称
    function ParseTypeRef: TType;
    // 分析System.TObject之类的完全限定ID,结果存于FQID
    procedure ParseQualifiedId(const First: string = '');
    // 分析完全限定ID,并查找实际的TSymbol: TType or TConstant
    function ParseQualifiedSym(const First: string = ''): TSymbol;

    function ParseStatement(Parent: TStatement): TStatement;
    function ParseCompoundStmt: TCompoundStmt;
    function ParseSimpleStmt: TStatement;
    function ParseIfStmt: TIfStmt;
    function ParseWhileStmt: TWhileStmt;
    function ParseForStmt: TForStmt;
    function ParseGotoStmt: TGotoStmt;
    function ParseRepeatStmt: TRepeatStmt;
    function ParseWithStmt(Parent: TStatement): TCompoundStmt;
    function ParseStmtList(Parent: TStatement; EndTokens: TTokens): TCompoundStmt;
    function ParseRaiseStmt: TRaiseStmt;
    function ParseTryStmt: TTryStmt;
    function ParseCaseStmt: TCaseStmt;
    function StmtAdjust(Stmt: TStatement): TStatement;

    function ParseFunction(Parent: TSymbol): TSymbol;
    procedure ParseFunctionDirective(Result: TFunctionHeader);
//    function ParseFunctionHeader: TFunction; overload;
    procedure ParseFunctionHeader(Result: TFunctionHeader); overload;
    procedure ParseFunctionBlock(Func: TFunction);
    procedure CheckFunction(F: TFunction);

    // return true, can overload
    function CheckOverloads(ExistsFunc, Func: TFunctionDecl): Boolean;
    procedure ParseArgumentList(Parent: TSymbol; Args: TList);

    function ParseIdList(SymClass: TSymbolClass): TSymbol;
    function ParseHints: TMemberHints;
    procedure ParseBlock(Parent: TSymbol);
    procedure ParseUsesClause;
    procedure ParseInterfaceSection;
    procedure ParseImplementSection;

    procedure ParseTypeSection(Parent: TSymbol);
    function ParseTypeDecl(const TypName: string = ''; Parent: TSymbol = nil): TType;
    function ParseClassType(const TypName: string;
        Parent: TSymbol; out NotAddSym: Boolean): TClassType;
    function ParseClassRefType: TClassRefType;
    function ParseProperty(Parent: TType; IsStatic: Boolean): TProperty;
    function ParseIntfProperty(Parent: TType): TIntfProperty;
    function ParseField(FieldClass: TSymbolClass): TField;
    function ParseRecordType(const TypName: string; Parent: TSymbol): TRecordType;
    function ParseInterfaceType(const IntfName: string;
        Parent: TSymbol; out NotAddSym: Boolean): TInterfaceType;
    function ParseObjectType(const ObjName: string): TObjectType;
    procedure ParseVarSection(Parent: TSymbol);
    procedure ParseConstSection(Parent: TSymbol);
    procedure ParseLabelSection(Parent: TSymbol);
    procedure ParseResStringSection(Parent: TSymbol);

    // 检查Expr, 如果Expr无效或类型不为bool, 则发出一个错误
    function CheckBoolExpr(var Expr: TExpr): Boolean;

    function IsSameArgs(L1, L2: TList): Boolean;

    procedure CheckForward;
    function ParseStrExpr(const DefValue: string): string;
    function ParseIntExpr(const DefValue: Integer = 0): Integer;
  public
    FErrorCount: Integer;
    FMaxErrorCount: Integer;
    FContext: TCompileContext;
    FModule: TModule;              // 当前模块
    FTopFunction: TFunction;       // 当前正在分析的最顶层函数
    FCurFunction: TFunction;       // 当前正在分析的函数
    // 当前位置的上级符号,比如正在分析type段, FCurParent可能是TMoudle,或TClassType
    // 如果分析函数, FCurParent就是FModule,
    // 如果分析方法, FCurParent就是方法所在的class/interface/object
    // 如果分析函数/方法下的type,var,const, FCurParent就是那个TFunction/TMethod
    FCurParent: TSymbol;
    FTempList: TList;
    FCurStates: TParseStates;
    FExpectedProcType: Boolean;
    // compile switch
    FMinEnumSize: Byte; // 1, 2, 4
    FAlignSize: Byte;   // 1, 2, 4, 8, 16
    FPointerSize: Byte; // 4, 8
    FRttiInfo: Boolean;     // $M+
    FTypedAddress: Boolean; // $T+
    FWriteableConst: Boolean; // $J+
    FCodeSwitches: TCodeSwitches;

    FIsSystemUnit: Boolean; // 是否编译System单元

    function GetSetType(typ: TSubrangeType): TSetType; overload;
    function GetSetType(typ: TEnumType): TSetType; overload;
    function GetSubrangeType(typ: TEnumType): TSubrangeType;

    // 取函数的重载列表
    procedure GetOverloadBegin(Func: TFunctionDecl);
    function GetOverloadNext: TFunctionDecl;
    procedure GetOverloadEnd;

    procedure StateSet(State: TParseState; out StateInfo: TParseStateInfo);
    procedure StateClear(State: TParseState; out StateInfo: TParseStateInfo);
    procedure StateRestore(const StateInfo: TParseStateInfo);

    function CreateNode(NodeClass: TAstNodeClass): TAstNode;
    function CreateElement(SymClass: TSymbolClass): TSymbol;
    function CreateType(TypClass: TTypeClass): TType;
    function CreateBinaryExpr(op: TExprOpCode; L: TExpr = nil; R: TExpr = nil): TBinaryExpr;
    function CreateUnaryExpr(op: TExprOpCode; Operand: TExpr = nil): TUnaryExpr;
    function CreateConstExpr(typ: TExprOpCode): TConstExpr;
    function CreateSymbolExpr(const Name: string = ''): TSymbolExpr;
    function CreateStmt(Stmt: TStatementClass): TStatement;

    function FindSymbol(const S: string): TSymbol; overload;
    {function FindSymbol(M: TModule; const S: string): TSymbol; overload;}
    function FindSymbol(Typ: TType; const S: string): TSymbol; overload;

    function FindWith(const S: string; out Sym: TSymbolExpr; out Elem: TSymbol): Boolean;

    function IsVisible(Ref, Referred: TSymbol): Boolean;
    procedure InternalError(const Msg: string);
    procedure ParseError(const Msg: string; Stop: Boolean = False); overload;
    procedure ParseError(const Msg: string; Args: array of const;
                          Stop: Boolean = False); overload;
    procedure ParseError(const Coord: TAstNodeCoord; const Msg: string;
                          Stop: Boolean = False); overload;
    procedure ParseError(const Coord: TAstNodeCoord; const Msg: string;
                          Args: array of const;
                          Stop: Boolean = False); overload;

    procedure DoWarning(const Coord: TAstNodeCoord; const Msg: string; Args: array of const); overload;
    procedure DoWarning(const Coord: TAstNodeCoord; const Msg: string); overload;
    procedure DoHint(const Coord: TAstNodeCoord; const Msg: string; Args: array of const); overload;
    procedure DoHint(const Coord: TAstNodeCoord; const Msg: string); overload;

    function CheckAssignmentCompatibility(T1, T2: TType): Boolean; overload;
  public
  //  procedure FindProper(E: TUnaryExpr; ProcType: TProceduralType);
    // 检查并调整表达式
   // function CheckExpr(var Expr: TExpr): Boolean;
    {$I exprh.inc}

    function ParseConstExpr: TExpr;
    function EvalExpr(Expr: TExpr): TValueRec; overload;
    function EvalExpr(Expr: TExpr; var Value: TValueRec): Boolean; overload;

    constructor Create(AContext: TCompileContext);
    destructor Destroy; override;
    procedure OpenCode(const S: string);
    procedure OpenFile(const FileName: string);

    function ParseExpr2 : TExpr;

    function Parse: TModule;
//    procedure ParseModule(M: TModule);
    function ParseProgram: TModule;
    function ParseUnit: TModule;
    procedure ParseUnitInterface(M: TModule);
    procedure ParseUnitImplementation;

    property ErrorCount: Integer read FErrorCount;
    property OnError: TParserErrorEvent read FOnError write FOnError;
  end;

implementation
uses err, func;

type
  TPropDirective = (
    idNon, idRead, idWrite, idIndex, idDefault, idStored, idNoDefault,
    idReadOnly, idWriteOnly, idDispID
  );

function ParsePropDirective(const S: string): TPropDirective;
begin
  Result := idNon;
  if Length(S) < 4 then Exit; // min 'read'
  case S[1] of
    'R', 'r':
      if SameText(S, 'read') then Result := idRead
      else if SameText(S, 'readonly') then Result := idReadOnly;

    'W', 'w':
      if SameText(S, 'write') then Result := idWrite
      else if SameText(S, 'writeonly') then Result := idWriteOnly;

    'I', 'i': if SameText(S, 'index') then Result := idIndex;

    'D', 'd':
      if SameText(S, 'default') then Result := idDefault
      else if SameText(S, 'dispid') then Result := idDispId;

    'S', 's':
      if SameText(S, 'stored') then Result := idStored;

    'N', 'n':
      if SameText(S, 'nodefault') then Result := idNoDefault;
  end;
end;

function IsCallConv(const S: string; out cc: TCallingConvention): Boolean;
begin
  Result := True;
  if SameText(S, 'stdcall') then
    cc := ccStdCall
  else if SameText(S, 'register') then
    cc := ccRegister
  else if SameText(S, 'pascal') then
    cc := ccPascal
  else if SameText(S, 'safecall') then
    cc := ccSafeCall
  else if SameText(S, 'cdecl') then
    cc := ccCDecl
  else
    Result := False;
end;

function IsModifier(const S: string; out M: TFunctionModifier): Boolean;
const
  ModifierStr: array[TFunctionModifier] of string =
  (
    'virtual', 'dynamic', 'abstract', 'override',
    'overload', 'message', 'reintroduce', 'static',
    'inline', 'assembler', 'varargs', 'local', 'dispid',
    'export', 'near', 'far', 'external', 'forward', 'noreturn'
  );
var
  I: TFunctionModifier;
begin
  for I := Low(ModifierStr) to High(ModifierStr) do
    if SameText(S, ModifierStr[I]) then
    begin
      M := I;
      Result := True;
      Exit;
    end;
  Result := False;
end;

function IsHint(const S: string; var H: TMemberHint): Boolean;
begin
  // hDeprecated, hLibrary, hPlatform, hExperimental, hUnimplemented
  Result := True;
  if SameText(S, 'deprecated') then
    H := hDeprecated
  else if SameText(S, 'library') then
    H := hLibrary
  else if SameText(S, 'platform') then
    H := hPlatform
  else if SameText(S, 'experimental') then
    H := hExperimental
  else if SameText(S, 'unimplemented') then
    H := hUnimplemented
  else
    Result := False;
end;

{ TParser }

function TParser.AddSymbol(Sym: TSymbol): Boolean;
begin
  Result := CurSymbols.Add(Sym);
  if Result then
    ParseError(Sym.Coord, SErr_RedeclaredIdent, [Sym.Name]);
end;

procedure TParser.AddSymbols(M: TModule);

{  procedure Add(List: TList);
  var
    I: Integer;
  begin
    for I := 0 to List.Count - 1 do
      FExternSymbols.Add(TSymbol(List[I]));
  end;}

var
  I: Integer;
begin
// AutoAddToOwner 暂时这样处理
  CurSymbols.AutoAddToOwner := False;
  AddSymbol(M);
  CurSymbols.AutoAddToOwner := True;

  // 所有uses的单元中的符号统一加在一个符号表,以增加查找速度
  FExternSymbols.EnsureCapacity(M.Symbols.Count);

  for I := 0 to M.Symbols.Count - 1 do
    FExternSymbols.Add(M.Symbols[I]);
{  Add(M.InterfaceSection.Types);
  Add(M.InterfaceSection.ResStrings);
  Add(M.InterfaceSection.Constants);
  Add(M.InterfaceSection.Variables);
  Add(M.InterfaceSection.Functions);
  Add(M.InterfaceSection.BuiltinFuncs);
  Add(M.InterfaceSection.ThreadVars);}
end;

function TParser.CheckAssignmentCompatibility(T1, T2: TType): Boolean;

  function IsSameProcType(P1, P2: TProceduralType): Boolean;
  begin
    Result := (P1.ReturnType = P2.ReturnType) and (P1.CallConvention = P2.CallConvention)
        and (P1.IsMethodPointer = P2.IsMethodPointer) and IsSameArgs(P1.Args, P2.Args);
  end;

  function CheckPointee(P1, P2: TPointerType): Boolean;
  begin
  // todo 1: 可能还是有问题，比如PInteger和PLongint
    Result := (P1.RefType = P2.RefType) or (
                  (P1.RefType.TypeCode = typObject)
                and (P2.RefType.TypeCode = typObject)
                and TObjectType(P2.RefType).IsInheritedFrom(
                      TObjectType(P1.RefType)
                    )
              )
  end;
//type
//  TBaseType = (btErr, btInt, btFlt, btCur, btEnu, btBol, btStr, btPtr);
begin
{
T1 and T2 are of the same type, and it is not a file type or structured type that contains a file type at any level.
T1 and T2 are compatible ordinal types.
T1 and T2 are both real types.
T1 is a real type and T2 is an integer type.
T1 is PChar or any string type and the expression is a string constant.
T1 and T2 are both string types.
T1 is a string type and T2 is a Char or packed-string type.

T1 is a long string and T2 is PChar.
T1 and T2 are compatible packed-string types.
T1 and T2 are compatible set types.
T1 and T2 are compatible pointer types.
T1 and T2 are both class, class-reference, or interface types and T2 is a derived from T1.
T1 is an interface type and T2 is a class type that implements T1.
T1 is PChar or PWideChar and T2 is a zero-based character array of the form array[0..n] of Char.

T1 and T2 are compatible procedural types. (A function or procedure identifier is treated, in certain assignment statements, as an expression of a procedural type.
See procedural types in statements and expressionson page 5-29.)
T1 is Variant and T2 is an integer, real, string, character, Boolean, or interface type.
T1 is an integer, real, string, character, or Boolean type and T2 is Variant.
T1 is the IUnknown or IDispatch interface type and T2 is Variant. (The variant's type code must be varEmpty, varUnknown, or varDispatch if T1 is IUnknown, and varEmpty or varDispatch if T1 is IDispatch.)
}
//  if T1.TypeCode = typSubrange then T1 := TSubrangeType(T1).BaseType;
//  if T2.TypeCode = typSubrange then T2 := TSubrangeType(T2).BaseType;
  T1 := T1.NormalType;
  T2 := T2.NormalType;
  case T1.TypeCode of
    typShortint..typUInt64:
      Result := T2.TypeCode in [typShortint..typUInt64, typVariant, typOleVariant];

    typComp..typCurrency:
      Result := T2.TypeCode in [typShortint..typUInt64,
                          typComp..typCurrency, typVariant, typOleVariant];

    typBoolean..typLongBool:
      Result := T2.TypeCode in [typBoolean..typLongBool, typVariant, typOleVariant];

    typAnsiChar:
      Result := T2.TypeCode in [typAnsiChar, typVariant, typOleVariant];

    typWideChar:
      Result := T2.TypeCode in [typWideChar, typVariant, typOleVariant];

    typPointer:
      if TPointerType(T1).IsUntype then
      begin
        Result := T2.TypeCode in [typPAnsiChar, typPWideChar, typPointer,
          typClass, typClassRef];
      end
      else if T2.TypeCode = typPointer then
      begin
        if T2.IsUntypePointer then
          Result := True
        else
          Result := CheckPointee(TPointerType(T1), TPointerType(T2));
      end
      else
        Result := False;

    typPAnsiChar:
      case T2.TypeCode of
        typPointer: Result := TPointerType(T2).IsUntype;
        typPAnsiChar: Result := True;
      else
        Result := False;
      end;
    typPWideChar:
      case T2.TypeCode of
        typPointer: Result := TPointerType(T2).IsUntype;
        typPWideChar: Result := True;
      else
        Result := False;
      end;
    typProcedural:
      case T2.TypeCode of
        typPointer: Result := True;
        typProcedural: begin
          Result := TProceduralType(T1).IsMethodPointer = TProceduralType(T2).IsMethodPointer;
          if Result {and FTypedAddress }then // 在$T+状态下，需要检查参数列表
            Result := IsSameProcType(TProceduralType(T1), TProceduralType(T2));
        end;

      else
        Result := False;
      end;
    typVariant, typOleVariant:
      Result := T2.IsVariantCompatible;
    typAnsiString:
      Result := T2.IsStringCompatible or (t2.TypeCode in [typPAnsiChar, typPWideChar, typAnsiChar, typWideChar, typVariant, typOleVariant]);
    typWideString:
      Result := T2.IsStringCompatible or (t2.TypeCode in [typPAnsiChar, typPWideChar, typAnsiChar, typWideChar, typVariant, typOleVariant]);
    typUnicodeString:
      Result := T2.IsStringCompatible or (t2.TypeCode in [typPAnsiChar, typPWideChar, typAnsiChar, typWideChar, typVariant, typOleVariant]);
    typShortString:
      Result := T2.IsStringCompatible or (t2.TypeCode in [typPAnsiChar, typPWideChar, typAnsiChar, typWideChar, typVariant, typOleVariant]);
    typEnum:
      Result := T1 = T2;
    typSet:
      case T2.TypeCode of
        typSet: Result := CheckAssignmentCompatibility(TSetType(T1).RangeType, TSetType(T2).RangeType);
      else
        Result := False;
      end;
    typDynamicArray:
      Result := T1 = T2;
    typRecord:
      Result := T1 = T2;
    typClass: // T1左值 T2右值
      case T2.TypeCode of
        typPointer: Result := TPointerType(T2).IsUntype;
        typInterface: Result := TClassType(T1).IsImplemented(TInterfaceType(T2));
        typClass: Result := (T1 = T2)
            or TClassType(T2).IsInheritedFrom(TClassType(T1));
      else
        Result := False;
      end;
    typClassRef:
      case T2.TypeCode of
        typPointer: Result := TPointerType(T2).IsUntype;
        typClass: Result := TClassType(T2).IsInheritedFrom(TClassRefType(T1).RefType);
        typClassRef:
          Result := TClassRefType(T2).IsInheritedFrom(TClassRefType(T1));
      else
        Result := False;
      end;
    typObject:
      case T2.TypeCode of
        typPointer: Result := TPointerType(T2).IsUntype;
        typObject: Result := (T1 = T2)
            or TObjectType(T2).IsInheritedFrom(TObjectType(T1));
      else
        Result := False;
      end;
    typInterface:
      case T2.TypeCode of
        typInterface: Result := (T1 = T2)
            or TInterfaceType(T2).IsInheritedFrom(TInterfaceType(T1))
      else
        Result := False;
      end;
    typDispInterface:
      case T2.TypeCode of
        typInterface: Result := TInterfaceType(T2).Base = FContext.FIDispatchType;
        typDispInterface: Result := (T1 = T2);
      else
        Result := False;
      end;
  else
    Result := False;
  end;
end;

function TParser.CheckBoolExpr(var Expr: TExpr): Boolean;
begin
  Result := CheckExpr(Expr);
  if Result then
  begin
    Result := Expr.Typ.IsBoolean;
    if not Result then
      ParseError(Expr.Coord, SErr_ExpectBoolExpr);
  end;
end;

{$I expr.inc}
{function TParser.CheckExpr(var Expr: TExpr): Boolean;
begin
  Result := expr.CheckExpr(Self, E);
end;}

procedure TParser.CheckForward;

  procedure CheckForward_Func(Func: TFunction);
  begin
    if Func.StartStmt = nil then
      ParseError(Func.Coord, SErr_FuncNotImpl, [Func.Name]);
  end;

  procedure CheckForward_Method(typ: TType);
  var
    symbols: TSymbolTable;
    sym: TSymbol;
    i: Integer;
  begin
    case typ.TypeCode of
      typClass: symbols := TClassType(typ).Symbols;
      typObject: symbols := TObjectType(typ).Symbols;
      typRecord: symbols := TRecordType(typ).Symbols;
    else
      Exit;
    end;

    for i := 0 to symbols.Count - 1 do
    begin
      sym := symbols[i];
      case sym.NodeKind of
        nkType: CheckForward_Method(TType(sym)); // 嵌套class,object,record.
        nkMethod: 
          if TMethod(sym).StartStmt = nil then
            ParseError(sym.Coord, SErr_MethodNotImpl, [sym.Name]);
      end;
    end;
  end;
var
  i: Integer;
  sym: TSymbol;
begin
  for i := 0 to FModule.Symbols.Count - 1 do
  begin
    sym := FModule.Symbols[i];
    case sym.NodeKind of
      nkFunc: CheckForward_Func(TFunction(sym));
      nkType: CheckForward_Method(TType(sym));
    end;
  end;
end;

procedure TParser.CheckFunction(F: TFunction);
begin
  func.CheckFunction(Self, F);
end;

function TParser.CheckOverloads(ExistsFunc, Func: TFunctionDecl): Boolean;
  function CanOverload(f1, f2: TFunctionDecl): Boolean;
  var
    i: Integer;
  begin
    if f1.CountOfArgs <> f2.CountOfArgs then
      Result := True
    else
    begin
      for i := 0 to f1.CountOfArgs - 1 do
        if not TArgument(f1.Args[i]).ArgType.Equals(
          TArgument(f2.Args[i]).ArgType) then
        begin
          Result := True;
          Exit;
        end;
      Result := False;
    end;
  end;

begin
  while ExistsFunc <> nil do
  begin
    if not CanOverload(ExistsFunc, Func) then
    begin
      ParseError(Func.Coord, 'function declaration duplicated');
      Result := False;
      Exit;
    end;
    ExistsFunc := ExistsFunc.NextOverload;
  end;
  Result := True;
end;

procedure TParser.ClearExprList;
var
  I: Integer;
begin
  for I := 0 to FExprList.Count - 1 do
    TObject(FExprList[I]).Free;
  FExprList.Count := 0;
end;

procedure TParser.ClearNodes;
var
  I: Integer;
begin
  for I := 0 to FNodes.Count - 1 do
    TObject(FNodes[I]).Free;
  FNodes.Clear;
end;

procedure TParser.ClearScopes;
begin
  FScopeList.Clear;
//  while FScopeList.Count > 0 do
//    LeaveScope;
end;

procedure TParser.ClearTempExprList;
var
  I: Integer;
begin
  for I := 0 to FTempExprList.Count - 1 do
    TObject(FTempExprList[I]).Free;
  FTempExprList.Count := 0;
end;

procedure TParser.ClearWithList;
begin
  while FWithList.Count > 0 do
    LeaveWithStmt;
end;

constructor TParser.Create(AContext: TCompileContext);
begin
  FScanner := TScanner.Create;
  FScanner.OnError := OnScannerError;
  FScanner.OnDirective := OnScannerDirective;
  FScanner.OnIfDefined := OnScannerIfDefined;
  FScanner.OnIfOpt := OnScannerIfOpt;
  FScanner.OnIfEval := OnScannerIfEval;
//  FScanner.CodeSwitches := @FCodeSwitches;
//  Include(FCodeSwitches, cdOverflowChecks);

  FContext := AContext;

  FAlignSize := 8;
  FPointerSize := 4;
  FMinEnumSize := 1;

  FHeader := TFunctionHeader.Create;
  FQId := TQualifiedId.Create;
  
  FNodes := TList.Create;
  FNodes.Capacity := 1024 * 64;

  FTempExprList := TList.Create;
  FTempExprList.Capacity := 100;
  FExprList := TList.Create;
  FExprList.Capacity := 1024 * 64;
  FCurExprList := FExprList;

  FScopeList := TList.Create;
  FScopeList.Capacity := 16;

  FWithList := TList.Create;
  FWithList.Capacity := 16;

  FTempList := TList.Create;
  FTempList.Capacity := 16;
  
  FExternSymbols := TSymbolTable.Create(nil);
  FExternSymbols.Capacity := 1024 * 64;

  FDefinedSymbols := THashTable.Create(16);
  FMaxErrorCount := 10;
end;

function TParser.CreateBinaryExpr(op: TExprOpCode; L, R: TExpr): TBinaryExpr;
begin
  Result := TBinaryExpr.Create;
  if L <> nil then L.Parent := nil;
  if R <> nil then R.Parent := nil;
  Result.Left := L;
  Result.Right := R;
  Result.OpCode := op;
  InitExpr(Result);
end;

function TParser.CreateConstExpr(typ: TExprOpCode): TConstExpr;
begin
  Result := TConstExpr.Create;
  Result.OpCode := typ;
  InitExpr(Result);
end;

function TParser.CreateElement(SymClass: TSymbolClass): TSymbol;
begin
  Result := SymClass.Create;
  Result.Coord.Row := Scanner.CurRow;
  Result.Coord.Col := Scanner.CurColumn;
  Result.Coord.FileName := Scanner.CurFileName;
//  InitAstNode(Result);
  if FInternalSection then
  begin
    Result.Attr := [saInternal];
    FNodes.Add(Result);
  end
  else
    FContext.FNodes.Add(Result);
end;

function TParser.CreateNode(NodeClass: TAstNodeClass): TAstNode;
begin
  Result := NodeClass.Create;
  InitAstNode(Result);
end;

function TParser.CreateStmt(Stmt: TStatementClass): TStatement;
begin
  Result := Stmt.Create;
  InitAstNode(Result);
end;

function TParser.CreateSymbolExpr(const Name: string): TSymbolExpr;
begin
  Result := TSymbolExpr.Create;
  Result.Name := Name;
  Result.OpCode := opSYMBOL;
  InitExpr(Result);
end;

function TParser.CreateType(TypClass: TTypeClass): TType;
begin
  Result := TType(CreateElement(TypClass));
//  InitAstNode(Result);
end;

function TParser.CreateUnaryExpr(op: TExprOpCode; Operand: TExpr): TUnaryExpr;
begin
  Result := TUnaryExpr.Create;
  Result.Operand := Operand;
  Result.OpCode := op;
  InitExpr(Result);
end;

function TParser.CurSymbols: TSymbolTable;
begin
  Result := TSymbolTable(FScopeList.Last);
end;

destructor TParser.Destroy;
begin
  ClearNodes;
  ClearExprList;
  ClearTempExprList;
  ClearScopes;
  ClearWithList;

  FTempList.Free;
  FWithList.Free;
  FScopeList.Free;
  FNodes.Free;
  FTempExprList.Free;
  FExprList.Free;
  FExternSymbols.Free;
  FDefinedSymbols.Free;
  FScanner.Free;
  ValClear(FTempValue);
  FHeader.Free;
  FQId.Free;
  inherited;
end;

procedure TParser.DoHint(const Coord: TAstNodeCoord; const Msg: string);
var
  Err: TParserErrorInfo;
begin
  if Assigned(FOnError) then
  begin
    Err := TParserErrorInfo.Create;
    Err.Row := Coord.Row;
    Err.Column := Coord.Col;
    Err.FileName := Coord.FileName;
    Err.ErrorMessage := Msg;
    Err.ErrorLevel := elHint;
    FOnError(Err);
    Err.Free;
  end;
end;

procedure TParser.DoHint(const Coord: TAstNodeCoord; const Msg: string;
  Args: array of const);
begin
  DoHint(Coord, Format(Msg, Args));
end;

procedure TParser.DoWarning(const Coord: TAstNodeCoord; const Msg: string);
var
  Err: TParserErrorInfo;
begin
  if Assigned(FOnError) then
  begin
    Err := TParserErrorInfo.Create;
    Err.Row := Coord.Row;
    Err.Column := Coord.Col;
    Err.FileName := Coord.FileName;
    Err.ErrorMessage := Msg;
    Err.ErrorLevel := elWarning;
    FOnError(Err);
    Err.Free;
  end;
end;

procedure TParser.DoWarning(const Coord: TAstNodeCoord; const Msg: string;
  Args: array of const);
begin
  DoWarning(Coord, Format(Msg, Args));
end;
{
procedure TParser.EnterScope;
var
  Syms: TSymbolTable;
begin
  Syms := TSymbolTable.Create;
  Syms.Capacity := 100;
  FScopeList.Add(Syms);
end;}

procedure TParser.EnterScope(SymTable: TSymbolTable);
begin
  FScopeList.Add(SymTable)
end;

procedure TParser.EnterWithStmt(Sym: TSymbolExpr);
var
  Typ: TType;
begin
  Typ := Sym.Typ;
  if Typ.TypeCode = typPointer then
    Typ := TPointerType(Typ).RefType;
  Assert(Typ.TypeCode in [typClass, typInterface, typDispInterface, typRecord, typObject]);
  Assert(Sym.Reference <> nil);
  FWithList.Add(Sym);
end;

function TParser.EvalExpr(Expr: TExpr): TValueRec;
begin
// todo 1: 需要改成有可以判断是否能评估的
  ValInit(Result);
  EvalExpr(Expr, Result);
end;

function TParser.EvalExpr(Expr: TExpr; var Value: TValueRec): Boolean;

  procedure DoEval(Expr: TExpr; var V: TValueRec); forward;
  function DoEvalSet(un: TUnaryExpr): TValueRec; forward;

const
  OrdRangeHigh: array[typShortint..typWideChar] of Int64 =
    (
    // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
    $7f, $ff, $7fff, $ffff, $7fffffff, $ffffffff, $7fffffffffffffff, $ffffffffffffffff,
    // typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
    0, 0, 0, 0, 0, 0,
    // typBoolean, typByteBool, typWordBool, typLongBool,
    1, -1, -1, -1,
    // typAnsiChar, typWideChar,
    $ff, $ffff
    );
  OrdRangeLow: array[typShortint..typWideChar] of Int64 =
    (
    // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
    $80, 0, $8000, 0, $80000000, 0, $8000000000000000, 0,
    // typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
    0, 0, 0, 0, 0, 0,
    // typBoolean, typByteBool, typWordBool, typLongBool,
    0, 0, 0, 0,
    // typAnsiChar, typWideChar,
    0, 0
    );
  function CallHigh(E: TExpr): Integer;
  var
    Typ: TType;
  begin
    Typ := E.Typ;
   { case Typ.TypeCode of
      typShortint..typLongint:
        Result := ValFromInt(Integer(OrdRangeHigh[Typ.TypeCode]));
      typLongWord..typUInt64:
        Result := ValFromInt(Int64(OrdRangeHigh[Typ.TypeCode]));
      typBoolean..typLongBool:
        Result := ValFromBool(
    end;      }
    // todo 1: 需要考虑char,longBool..bytebool

    if Typ.TypeCode in [typShortint..typWideChar] then
      Result := OrdRangeHigh[Typ.TypeCode]
    else if Typ.TypeCode = typEnum then
      Result := TEnumType(Typ).HighValue
    else if Typ.TypeCode = typSubrange then
      Result := TSubrangeType(Typ).RangeEnd
    else if Typ.TypeCode = typArray then // typDynamicArray 常量估算不能对动态数组
      Result := TArrayType(Typ).Range.RangeEnd
    else if Typ.TypeCode = typShortString then
      Result := Typ.Size
    else begin
      ParseError(E.Coord, SErr_InvalidArgument, ['high']);
      Result := 0;
    end;
  end;

  function CallLow(E: TExpr): Integer;
  var
    Typ: TType;
  begin
    Typ := E.Typ;
    if Typ.TypeCode in [typShortint..typWideChar] then
      Result := OrdRangeLow[Typ.TypeCode]
    else if Typ.TypeCode = typEnum then
      Result := TEnumType(Typ).LowValue
    else if Typ.TypeCode = typSubrange then
      Result := TSubrangeType(Typ).RangeBegin
    else if Typ.TypeCode = typArray then // typDynamicArray 常量估算不能对动态数组
      Result := TArrayType(Typ).Range.RangeBegin
    else if Typ.TypeCode = typShortString then
      Result := 0
    else begin
      ParseError(E.Coord, SErr_InvalidArgument, ['low']);
      Result := 0;
    end;
  end;

  function CallLength(E: TExpr): Integer;
  var
    Typ: TType;
    V: TValueRec;
  begin
    Typ := E.Typ;
    case Typ.TypeCode of
      typArray: // typDynamicArray 不能用于常量估算
        Result := TArrayType(Typ).Range.RangeEnd - TArrayType(Typ).Range.RangeBegin + 1;
      typShortString:
        Result := Typ.Size;
      typAnsiString, typWideString: begin
        ValInit(V);
        try
          DoEval(E, V);
          Result := Length(ValToStr(V));
        finally
          ValClear(V);
        end;
      end;
      else begin
        ParseError(E.Coord, SErr_InvalidArgument, ['length']);
        Result := 0;
      end;
    end;
  end;

  // 比如: Sizeof(['a'..'g']);
  // 如果有不可取值的Expr,则当成32
  function SizeOfSetLiteral(E: TExpr): Integer;
  var
    V: TValueRec;
  begin
    try
      ValInit(V);
      V := DoEvalSet(TUnaryExpr(E));
      Result := ValToSet(V).MinSize;
      ValClear(V);
    except
      ValClear(V);
      Result := 32;
    end;
  end;
{
  sizeof可作用于类型,变量,过程/函数名,常量表达式
}
  function CallSizeOf(E: TExpr): Cardinal;
  var
    Elem: TSymbol;
    S: TSetValue;
  begin
    if E.Typ.TypeCode = typSet then
    begin
      if E.OpCode = opSET then
        Result := SizeOfSetLiteral(E)
      else if E.OpCode = opSYMBOL then begin
        Elem := TSymbolExpr(E).Reference;
        if Elem.NodeKind = nkConstant then
        begin
          S := ValToSet(TConstant(Elem).Value);
          if S = nil then
            Result := 32 else
          Result := S.MinSize
        end
        else
          Result := E.Typ.Size;
      end
      else if E.ClassType = TConstExpr then begin
        S := ValToSet(TConstExpr(E).Value);
        if S = nil then
          Result := 32 else
        Result := S.MinSize;
      end
      else
        Result := E.Typ.Size;
    end
    else
      Result := E.Typ.Size;
  end;

  function Call(bin: TBinaryExpr): TValueRec;

    function CheckInt(const V: TValueRec; const Func: string): Boolean;
    begin
      Result := V.VT in [vtInt, vtInt64];
      if not Result then
        ParseError(bin.Coord, SErr_InvalidArgument, [Func]);
    end;

    function CheckOrd(const V: TValueRec; const Func: string): Boolean;
    begin
      Result := V.VT in [vtInt, vtInt64, vtAChr, vtWChr];
      if not Result then
        ParseError(bin.Coord, SErr_InvalidArgument, [Func]);
    end;

    function CheckNum(const V: TValueRec; const Func: string): Boolean;
    begin
      Result := V.VT in [vtInt, vtInt64, vtReal, vtCurr];
      if not Result then
        ParseError(bin.Coord, SErr_InvalidArgument, [Func]);
    end;
  var
    Sym, A1: TExpr;
    F: TSymbol;
    V: TValueRec;
  begin
    Sym := bin.Left;
    if (Sym.OpCode = opSYMBOL) then
      F := TSymbolExpr(Sym).Reference
    else if Sym.OpCode = opMEMBER then
    begin
      Assert(TBinaryExpr(Sym).Right.OpCode = opSYMBOL);
      F := TSymbolExpr(TBinaryExpr(Sym).Right).Reference;
    end
    else begin
      F := nil;
      ParseError(bin.Coord, 'Invalid function in const expr', True);
    end;

    if F.NodeKind <> nkBuiltinFunc then
      ParseError(bin.Coord, 'Invalid function in const expr', True);

    A1 := TUnaryExpr(bin.Right).Operand; // first arg
    ValInit(V);
    try
      if not (TBuiltinFunction(F).Kind in [bfHigh, bfLow, bfSizeOf, bfLength]) then
        DoEval(A1, V);
      case TBuiltinFunction(F).Kind of
        bfAbs:     if CheckNum(V, 'abs') then Result := ValAbs(V);
        bfChr:     if CheckInt(V, 'chr') then Result := ValChr(V);
        bfHi:      if CheckInt(V, 'hi') then Result := ValHi(V);
        bfHigh:    Result := ValFromInt(CallHigh(A1));
        bfLength:  Result := ValFromInt(CallLength(A1));
        bfLo:      if CheckInt(V, 'lo') then Result := ValLo(V);
        bfLow:     Result := ValFromInt(CallLow(A1));
        bfOdd:     if CheckInt(V, 'odd') then Result := ValOdd(V);
        bfOrd:     if CheckOrd(V, 'ord') then Result := ValOrd(V);
        bfPred:    if CheckOrd(V, 'pred') then Result := ValPred(V);
        bfRound:   if CheckNum(V, 'round') then Result := ValRound(V);
        bfSizeOf:  Result := ValFromInt(Int64(CallSizeOf(A1)));// ValFromInt(A1.Typ.Size);
        bfSucc:    if CheckOrd(V, 'succ') then Result := ValSucc(V);
        bfSwap:    if CheckInt(V, 'swap') then Result := ValSwap(V);
        bfTrunc:   if CheckNum(V, 'trunc') then Result := ValTrunc(V);
      else
        ParseError(bin.Coord, SErr_InvalidBuiltinFunc);
      end;
    finally
      ValClear(V);
    end;
  end;

  function Cast(bin: TBinaryExpr): TValueRec;
  var
    Sym, A1: TExpr;
    F: TSymbol;
    V: TValueRec;
  begin
    Sym := bin.Left;
    if (Sym.OpCode = opSYMBOL) then
      F := TSymbolExpr(Sym).Reference
    else if Sym.OpCode = opMEMBER then
    begin
      Assert(TBinaryExpr(Sym).Right.OpCode = opSYMBOL);
      F := TSymbolExpr(TBinaryExpr(Sym).Right).Reference;
    end
    else begin
      F := nil;
      ParseError(bin.Coord, 'Invalid cast expression', True);
    end;

    if F.NodeKind <> nkType then
      ParseError(bin.Coord, 'Invalid cast expression', True);

    F := TType(F).NormalType;
    A1 := TUnaryExpr(bin.Right).Operand; // first arg
    ValInit(V);
    try
      DoEval(A1, V);
      case TType(F).TypeCode of
        typShortint..typLongint:
          Result := ValCast(V, vtInt);
        typLongWord..typUInt64:
          Result := ValCast(V, vtInt64);
        typComp..typExtended:
          Result := ValCast(V, vtReal);
        typCurrency: Result := ValCast(V, vtCurr);
        typAnsiChar: Result := ValChr(V);
        typWideChar: Result := ValFromInt(Word(ValToInt(V)));
        typBoolean..typLongBool:
          Result := ValCast(V, vtBool);
        typPointer:
          Result := ValCast(V, vtPtr);
        else begin
          ParseError(Expr.Coord, 'Invalid cast');
          Result := V;
        end;
      end;
    finally
      ValClear(V);
    end;
  end;

  function DoEvalSet(un: TUnaryExpr): TValueRec;
  var
    E: TExpr;
    V1, V2: TValueRec;
    S: TSetValue;
    I: Integer;
  begin
    Assert(un.OpCode = opSET);
    un := TUnaryExpr(un.Operand);
    if un = nil then begin
      ValFromSet(Result, nil);
      Exit;
    end;

    Assert(un.OpCode = opLIST);
    E := un.Operand;
    ValInit(V1);
    ValInit(V2);
    S := TSetValue.Create;
    try
      while E <> nil do
      begin
        if E.OpCode = opRANGE then begin
          ValClear(V1);
          ValClear(V2);
          DoEval(TBinaryExpr(E).Left, V1);
          DoEval(TBinaryExpr(E).Right, V2);
          for I := ValToInt(V1) to ValToInt(V2) do
            S.SetBits(I, True);
        end
        else begin
          ValClear(V1);
          DoEval(E, V1);
          S.SetBits(ValToInt(V1), True);
        end;
        E := TExpr(E.Next);
      end;
    finally
      ValClear(V1);
      ValClear(V2);
    end;
    S.Update;
    Result := ValFromSet(S);
  end;

  procedure DoEval(Expr: TExpr; var V: TValueRec);
  var
    LVal, RVal: TValueRec;
  begin
    ValInit(LVal);
    ValInit(RVal);
    try
      if Expr.ClassType = TBinaryExpr then
      begin
        if not (Expr.OpCode in [opCAST, opCALL]) then
        begin
          DoEval(TBinaryExpr(Expr).Left, LVal);
          DoEval(TBinaryExpr(Expr).Right, RVal);
        end;
      end
      else if Expr.ClassType = TUnaryExpr then
      begin
        if Expr.OpCode <> opSET then
          DoEval(TUnaryExpr(Expr).Operand, LVal);
      end
      else if Expr.ClassType = TConstExpr then
      begin
        ValCopy(V, TConstExpr(Expr).Value);
        Exit;
      end;

      case Expr.OpCode of
        opADD..opSHR: V := ValOp(LVal, RVal, Expr.OpCode);
        opNE..opGE: V := ValCmp(LVal, RVal, Expr.OpCode);
        opIN : V := ValIn(LVal, RVal);
        opNOT: V := ValNot(LVal);
        opNEG: V := ValNeg(LVal);
        opPOS: V := LVal;
        opMEMBER: V := RVal;
        opCAST: V := Cast(TBinaryExpr(Expr));
        opCALL: V := Call(TBinaryExpr(Expr));
        // todo 1: @操作符可以出现在
        opRANGE: // opRANGE 不会出现在常量估算之中,已经在opSET中计算
          ParseError(Expr.Coord, SErr_InvalidConstOp, True);
        opINDEX: // 在常量表达式中,不会取数组元素的值
          ParseError(Expr.Coord, SErr_InvalidConstOp, True);
        opSET: V := DoEvalSet(TUnaryExpr(Expr));
        opSYMBOL: begin
          case TSymbolExpr(Expr).Reference.NodeKind of
            nkEnumElement:
              ValFromInt(V, TEnumValue(TSymbolExpr(Expr).Reference).Value);
            nkModule, nkNameScope, nkType:
              V := ValFromElement(TSymbolExpr(Expr).Reference);
              // todo 1: 要考虑变量
            nkConstant:
              ValCopy(V, TConstant(TSymbolExpr(Expr).Reference).Value);
          else
            ParseError(Expr.Coord, SErr_InvalidOperand, True);
          end;
        end;
      else
        ParseError(Expr.Coord, SErr_InvalidConstOp, True);
      end;
    finally
      ValClear(LVal);
      ValClear(RVal);
    end;
  end;

begin
  ValClear(Value);

  Result := True;
  try
    DoEval(Expr, Value);
  except
    Result := False;
  end;
end;

procedure TParser.Expect(T: TToken; Stop: Boolean);

  function TokenText: string;
  begin
    case CurToken of
      tkIdentifier,
      tkStrConst,
      tkCharConst:
        Result := FCurTokenString;

      tkIntConst,
      tkHexConst,
      tkBinConst,
      tkOctalConst:
        Result := 'integer ' + IntToStr(fScanner.TokenValue.IntValue);

      tkFloatConst:
        Result := 'real ' + FloatToStr(FScanner.TokenValue.RealValue);
      else
        Result := TokenNames[CurToken];
    end;
  end;

  procedure Error;
  begin
    ParseError(Format('%s expected, but %s found', [
            TokenNames[T], TokenText]), Stop);
  end;
begin
  if CurToken <> T then
    Error;
end;

function TParser.FindSymbol(const S: string): TSymbol;
var
  I: Integer;
begin
  for I := FScopeList.Count - 1 downto 0 do
  begin
    Result := TSymbolTable(FScopeList[I]).Find(S);
    if Result <> nil then Exit;
  end;
  Result := FExternSymbols.Find(S);
end;

{function TParser.FindSymbol(M: TModule; const S: string): TSymbol;
begin
  if M = nil then
    Result := FindSymbol(S)
  else if M = FModule then
    Result := FModule.FindSymbol(S)
  else
    Result := FExternSymbols.Find(M, S);
end;}

function TParser.FindSymbol(Typ: TType; const S: string): TSymbol;
//var
//  I: Integer;
//  Rec: TRecordType;
begin
  if Typ is TRecordType then
  begin
    Result := TRecordType(Typ).FindSymbol(S);
  end
  else if Typ is TClassType then
  begin
    Result := TClassType(Typ).FindSymbol(S);
  end
  else
    Result := nil;
end;

function TParser.FindWith(const S: string; out Sym: TSymbolExpr; out Elem: TSymbol): Boolean;
var
  I: Integer;
  Typ: TType;
begin
  Result := True;
  for I := FWithList.Count - 1 downto 0 do
  begin
    Sym := TSymbolExpr(FWithList[I]);
    Typ := Sym.Typ;
    if Typ.TypeCode = typPointer then
      Typ := TPointerType(Typ).RefType;
    case Typ.TypeCode of
      typClass: Elem := TClassType(Typ).FindSymbol(S);
      typInterface, typDispInterface: Elem := TInterfaceType(Typ).FindSymbol(S);
      typObject: Elem := TObjectType(Typ).FindSymbol(S);
      typRecord: Elem := TRecordType(Typ).FindSymbol(S);
    else
      Elem := nil;
    end;
    if Elem <> nil then Exit;
  end;
  Elem := nil;
  Sym := nil;
  Result := False;
end;

procedure TParser.GetOverloadBegin(Func: TFunctionDecl);
begin
  FCurOverloadFunc := Func;
end;

procedure TParser.GetOverloadEnd;
begin
  FCurOverloadFunc := nil;
  FCurSymbolPos := nil;
end;

function TParser.GetOverloadNext: TFunctionDecl;

  function NextMeth_Class(T: TClassType): TFunctionDecl;
  var
    Sym: TSymbol;
  begin
    Result := nil;
    if T = nil then Exit;
// todo 1: 可能需要考虑有实现接口的类
    repeat
      Sym := T.FindCurSymbol(FCurOverloadFunc.Name);
      if Sym.NodeKind = nkMethod then
        Result := TMethod(sym);
      T := T.Base;
    until (T = nil) or (Result <> nil);
  end;

  function NextMeth_Object(T: TObjectType): TFunctionDecl;
  var
    Sym: TSymbol;
  begin
    Result := nil;
    if T = nil then Exit;

    repeat
      Sym := T.FindCurSymbol(FCurOverloadFunc.Name);
      if Sym.NodeKind = nkMethod then
        Result := TMethod(sym);
      T := T.Base;
    until (T = nil) or (Result <> nil);
  end;

  function NextMeth_Intf(T: TInterfaceType): TFunctionDecl;
  var
    Sym: TSymbol;
  begin
    Result := nil;
    if T = nil then Exit;

    repeat
      Sym := T.FindCurSymbol(FCurOverloadFunc.Name);
      if Sym.NodeKind = nkMethod then
        Result := TMethod(sym);
      T := T.Base;
    until (T = nil) or (Result <> nil);
  end;

  function NextOverloadMeth: TFunctionDecl;
  var
    T: TSymbol;
  begin
    Result := nil;
    T := FCurOverloadFunc.Parent;
    Assert(T.NodeKind = nkType, 'NextOverloadMeth: T is not type');
    case TType(T).TypeCode of
      typRecord: Exit;
      typClass: Result := NextMeth_Class(TClassType(T).Base);
      typObject: Result := NextMeth_Object(TObjectType(T).Base);
      typInterface, typDispInterface:
        Result := NextMeth_Intf(TInterfaceType(T).Base);
    else
      Assert(False, 'NextOverloadMeth: T is not class/object/interface');
    end;
  end;

  function NextOverloadExternal: TFunctionDecl;
  var
    Sym: TSymbol;
  begin
    // 取外部符号
    if FCurSymbolPos = nil then
    begin
      FCurSymbolPos := FExternSymbols.GetStart(FCurOverloadFunc.Name);
      // Skip first
      FExternSymbols.GetNext(FCurSymbolPos);
    end;

    repeat
      Sym := FExternSymbols.GetNext(FCurSymbolPos);
      if (Sym <> nil) and (Sym.NodeKind in [nkFunc, nkExternalFunc]) then
      begin
        Result := TFunctionDecl(Sym);
        Exit;
      end;
    until Sym = nil;

    Result := nil;
  end;

  function NextOverload: TFunctionDecl;
  var
    P, Sym: TSymbol;
  begin
//    if FCurOverloadFunc.Parent.NodeKind = nkModule then
    if (FCurOverloadFunc.Module <> Self.FModule) then
    begin
      Result := NextOverloadExternal;
      Exit;
    end;

    P := FCurOverloadFunc.Parent;
    if P.NodeKind <> nkModule then
    begin
      if P.NodeKind in [nkFunc, nkMethod] then
        P := P.Parent;
      // 允许嵌套函数重载
      while P.NodeKind in [nkFunc, nkMethod] do
      begin
        Sym := TFunction(P).LocalSymbols.Find(FCurOverloadFunc.Name);
        if Sym.NodeKind in [nkFunc, nkExternalFunc, nkMethod] then
        begin
          Result := TFunctionDecl(Sym);
          Exit;
        end;
        P := P.Parent;
      end;

      // 取模块
      while P.NodeKind <> nkModule do
      begin
        P := P.Parent;
        Assert(P <> nil, 'NextOverload: P <> nil');
      end;

      Sym := TModule(P).Symbols.Find(FCurOverloadFunc.Name);
      if (Sym <> nil) and (Sym.NodeKind in [nkFunc, nkExternalFunc, nkMethod]) then
      begin
        Result := TFunctionDecl(Sym);
        Exit;
      end;
    end;

    // 取外部符号
    if FCurSymbolPos = nil then
      FCurSymbolPos := FExternSymbols.GetStart(FCurOverloadFunc.Name);

    Result := NextOverloadExternal;
  end;

begin
  if FCurOverloadFunc.NodeKind = nkMethod then
    FCurOverloadFunc := NextOverloadMeth
  else
    FCurOverloadFunc := NextOverload;
  Result := FCurOverloadFunc;
end;

function TParser.GetSetType(typ: TSubrangeType): TSetType;
begin
  if typ.SetType = nil then
  begin
    typ.SetType := TSetType.Create;
    typ.SetType.Name := '$' + typ.Name + '_Set';
    typ.SetType.RangeType := typ;
    typ.SetType.Update;
    typ.SetType.Coord := typ.Coord;
    if saInternal in typ.Attr then Include(typ.SetType.Attr, saInternal);
    if Self.FInternalSection then
      Self.FNodes.Add(typ.SetType)
    else
      FContext.FNodes.Add(typ.SetType);
    typ.Parent.Add(typ.SetType);
  end;
  Result := typ.SetType;
end;

function TParser.GetSetType(typ: TEnumType): TSetType;
begin
  Result := GetSetType(GetSubrangeType(typ));
end;

function TParser.GetSubrangeType(typ: TEnumType): TSubrangeType;
begin
  if typ.SubrangeType = nil then
  begin
    typ.SubrangeType := TSubrangeType.Create;
    typ.SubrangeType.Name := '$' + typ.Name + '_Rng';
    typ.SubrangeType.BaseType := typ;
    typ.SubrangeType.Coord := typ.Coord;
    if saInternal in typ.Attr then Include(typ.SubrangeType.Attr, saInternal);
    if Self.FInternalSection then
      Self.FNodes.Add(typ.SubrangeType)
    else
      FContext.FNodes.Add(typ.SubrangeType);
    typ.Parent.Add(typ.SubrangeType);
  end;
  typ.SubrangeType.RangeBegin := typ.LowValue;
  typ.SubrangeType.RangeEnd := typ.HighValue;
  Result := typ.SubrangeType;
end;

procedure TParser.InitAstNode(ANode: TAstNode);
begin
  ANode.Coord.Row := Scanner.CurRow;
  ANode.Coord.Col := Scanner.CurColumn;
  ANode.Coord.FileName := Scanner.CurFileName;
  FNodes.Add(ANode);
end;

procedure TParser.InitExpr(Expr: TExpr);
begin
  Expr.Coord.Row := Scanner.CurRow;
  Expr.Coord.Col := Scanner.CurColumn;
  Expr.Coord.FileName := Scanner.CurFileName;
  Expr.Switches := FCodeSwitches;
  FCurExprList.Add(Expr);
end;

procedure TParser.InternalError(const Msg: string);
begin
  Self.ParseError(SErr_InternalError, [Msg], True);
end;

function TParser.IsSameArgs(L1, L2: TList): Boolean;
  function IsSameArg(A1, A2: TArgument): Boolean;
  begin
    Result := (A1.ArgType = A2.ArgType) and
              (A1.Modifier = A2.Modifier) and
              (A1.ArgKind = A2.ArgKind);
  end;

var
  I, C1, C2: Integer;
begin
  Result := False;
  if L1 <> nil then C1 := L1.Count else C1 := 0;
  if L2 <> nil then C2 := L2.Count else C2 := 0;

  if C1 <> C2 then Exit;

  for I := 0 to C1 - 1 do
    if not IsSameArg(TArgument(L1[I]), TArgument(L2[I])) then Exit;

  Result := True;
end;

function TParser.IsTempExpr: Boolean;
begin
  Result := FCurExprList = Self.FTempExprList;
end;

function TParser.IsVisible(Ref, Referred: TSymbol): Boolean;

  function PrivCanAccess(Ref: TSymbol; Referred: TType): Boolean;
  begin
  // 判断Ref是否可以访问Referred的private成员
  // 只要Ref是Referred的成员或嵌套类型的成员
  // 或者Ref和Referred是同一单元
    while Ref.Parent <> nil do
    begin
      if Ref = Referred then
      begin
        Result := True;
        Exit;
      end;
      Ref := Ref.Parent;
    end;

    Result := (Ref.NodeKind = nkModule) and (Ref = Referred.Module);
  end;

  function IsInhFrom(Ref: TSymbol; Referred: TType): Boolean;
  begin
    Result := False;
    if (Ref.NodeKind = nkType) and (TType(Ref).TypeCode = Referred.TypeCode) then
      case Referred.TypeCode of
        typClass: Result := TClassType(Ref).IsInheritedFrom(TClassType(Referred));
        typObject: Result := TObjectType(Ref).IsInheritedFrom(TObjectType(Referred));
      end;
  end;

  function ProtCanAccess(Ref: TSymbol; Referred: TType): Boolean;
  begin
    Result := PrivCanAccess(Ref, Referred) or IsInhFrom(Ref, Referred);
  end;

  function StrictPrivCanAccess(Ref: TSymbol; Referred: TType): Boolean;
  begin
    while Ref.Parent <> nil do
    begin
      if Ref = Referred then
      begin
        Result := True;
        Exit;
      end;
      Ref := Ref.Parent;
    end;
    Result := False;
  end;

  function StrictProtCanAccess(Ref: TSymbol; Referred: TType): Boolean;
  begin
    Result := StrictPrivCanAccess(Ref, Referred) or IsInhFrom(Ref, Referred);
  end;

  function PubCanAccess(Ref: TSymbol; Referred: TType): Boolean;
  var
    Vis: TMemberVisibility;
    S: TSymbol;
  const
    VisN: array[TMemberVisibility] of Byte = (
      4, 0, 1, 2, 4, 4, 4, 4
    );
  begin
    Vis := visPublic;
    while Referred <> nil do
    begin
      if VisN[Referred.Visibility] < VisN[Vis] then
        Vis := Referred.Visibility;
      S := Referred.Parent;
      if Assigned(S) and (s.NodeKind = nkType) then
        Referred := TType(S)
      else
        Break;
    end;
    case Vis of
      visPrivate: Result := PrivCanAccess(Ref, Referred);
      visProtected: Result := ProtCanAccess(Ref, Referred);
      visStrictPrivate: Result := StrictPrivCanAccess(Ref, Referred);
      visStrictProtected: Result := StrictProtCanAccess(Ref, Referred);
    else
      Result := True;
    end;
  end;
var
  S: TSymbol;
begin
  Result := True;
  S := Referred.Parent;
  if S = nil then Exit;
{
visDefault,
visPrivate,         [Owner, NestOfOwner, Module]
visProtected,       [Owner, NestOfOwner, Module, SubOfOwner]
visPublic,          [Owner, NestOfOwner, Module, SubOfOwner, Outter]
visPublished,       [Owner, NestOfOwner, Module, SubOfOwner, Outter]
visAutomated,       [Owner, NestOfOwner, Module, SubOfOwner, Outter]
visStrictPrivate,   [Owner, NestOfOwner]
visStrictProtected  [Owner, NestOfOwner, SubOfOwner]
}

  case S.NodeKind of
  nkType:
    if TType(S).TypeCode in [typClass, typObject, typRecord] then
    begin
      case Referred.Visibility of
        visPrivate: Result := PrivCanAccess(Ref, TType(S));
        visProtected: Result := ProtCanAccess(Ref, TType(S));
        visPublic, visPublished, visAutomated:
          Result := PubCanAccess(Ref, TType(S));
        visStrictPrivate:
          Result := StrictPrivCanAccess(Ref, TType(S));
        visStrictProtected:
          Result := StrictProtCanAccess(Ref, TType(S));
      else
        Result := True;
      end;
    end
    else if TType(S).TypeCode in [typInterface, typDispInterface] then
      Result := True
    else
      ParseError(SErr_InternalError, ['Referred.Parent invalid']);
  nkModule, nkFunc, nkMethod:
    Result := True;       // 处于Module这一级的符号,都可以使用
  else
    ParseError(SErr_InternalError, ['Referred.Parent invalid']);
  end;

end;

procedure TParser.LeaveScope;
begin
  if FScopeList.Count > 0 then
  begin
    FScopeList.Delete(FScopeList.Count - 1);
  end;
end;

procedure TParser.LeaveWithStmt;
begin
  if FWithList.Count > 0 then
  begin
    FWithList.Delete(FWithList.Count - 1);
  end;
end;

procedure TParser.NextToken;
const
  WhitespaceTokensToIgnore = [tkWhitespace, tkIllegalChar, tkComment,
                              tkLineEnding];
begin
  if FTokenBufferIndex < FTokenBufferSize then
  begin
    // Get token from buffer
    FCurToken := FTokenBuffer[FTokenBufferIndex];
    FCurTokenString := FTokenStringBuffer[FTokenBufferIndex];
    Inc(FTokenBufferIndex);
  end
  else
  begin
    { We have to fetch a new token. But first check, wether there is space left
      in the token buffer.}
    if FTokenBufferSize = 2 then
    begin
      FTokenBuffer[0] := FTokenBuffer[1];
      FTokenStringBuffer[0] := FTokenStringBuffer[1];
      Dec(FTokenBufferSize);
      Dec(FTokenBufferIndex);
    end;

    // Fetch new token
{    repeat
      FCurToken := Scanner.FetchToken;
    until not (FCurToken in WhitespaceTokensToIgnore);}
    FCurToken := FScanner.FetchToken;
    Assert(not (FCurToken in WhitespaceTokensToIgnore));

    FCurTokenString := Scanner.CurTokenString;
    FTokenBuffer[FTokenBufferSize] := FCurToken;
    FTokenStringBuffer[FTokenBufferSize] := FCurTokenString;
    Inc(FTokenBufferSize);
    Inc(FTokenBufferIndex);
  end;
end;

procedure TParser.OnScannerDirective(var dinfo: TDirectiveInfo);
begin
  case dinfo.Directive of
    cdBoolEval..cdSafeDivide:
      if dinfo.State then
        Include(FCodeSwitches, dinfo.Directive)
      else
        Include(FCodeSwitches, dinfo.Directive);
    cdTypeInfo:
      Self.FRttiInfo := dinfo.State;
    cdTypedAddress:
      Self.FTypedAddress := dinfo.State;
    cdWriteableConst:
      Self.FWriteableConst := dinfo.State;
    cdAlign:
      if dinfo.ArgInt1 > 0 then
        Self.FAlignSize := dinfo.ArgInt1
      else if dinfo.State then
        Self.FAlignSize := 8
      else
        Self.FAlignSize := 1;
    cdMinEnumSize:
      if dinfo.ArgInt1 > 0 then
        Self.FMinEnumSize := dinfo.ArgInt1
      else if dinfo.State then
        Self.FMinEnumSize := 4
      else
        Self.FMinEnumSize := 1;
    cdDefine:
      FDefinedSymbols.Add(dinfo.ArgStr1, nil);
    cdUndef:
      FDefinedSymbols.Remove(dinfo.ArgStr1);
  end;
end;

procedure TParser.OnScannerError(const Msg: string; Stop: Boolean);
begin
  ParseError(Msg, Stop);
end;

procedure TParser.OnScannerIfDefined(const S: string;
  out IsDefined: Boolean);
begin
  IsDefined := FDefinedSymbols.IsExists(S);
end;

procedure TParser.OnScannerIfEval(out IsDefined: Boolean);
var
  E: TExpr;
  V: TValueRec;
  Oldtemp: Boolean;
  FBakIndex, FBakSize: Integer;
  FBakTokens: array[0..1] of TToken;
  FBakStrBuf: array[0..1] of string;
begin
  FBakIndex := Self.FTokenBufferIndex;
  FBakSize := Self.FTokenBufferSize;
  FBakTokens[0] := Self.FTokenBuffer[0];
  FBakTokens[1] := Self.FTokenBuffer[1];
  FBakStrBuf[0] := Self.FTokenStringBuffer[0];
  FBakStrBuf[1] := Self.FTokenStringBuffer[1];

  OldTemp := IsTempExpr;
  SetTempExpr(True);
  NextToken;
  E := ParseExpr;
  try
    V := EvalExpr(E);
    case V.VT of
      vtInt: IsDefined := V.VInt <> 0;
      vtInt64: IsDefined := V.VInt64 <> 0;
      vtBool: IsDefined := V.VBool;
    else
      IsDefined := False;
      ParseError(E.Coord, 'Invalid const expr', True);
    end;
  except
    IsDefined := False;
  end;
  ValClear(V);
  if not OldTemp then
  begin
    ClearTempExprList;
    SetTempExpr(OldTemp);
  end;
  FTokenBufferIndex := FBakIndex;
  FTokenBufferSize := FBakSize;
  FTokenBuffer[0] := FBakTokens[0];
  FTokenBuffer[1] := FBakTokens[1];
  FTokenStringBuffer[0] := FBakStrBuf[0];
  FTokenStringBuffer[1] := FBakStrBuf[1];
end;

procedure TParser.OnScannerIfOpt(C: Char; out IsSet: Boolean);
begin
  case C of
    'A', 'a': IsSet := Self.FAlignSize > 1;
    'Q', 'q': IsSet := cdOverflowChecks in FCodeSwitches;
    'R', 'r': IsSet := cdRangeChecks in FCodeSwitches;
    'B', 'b': IsSet := cdBoolEval in FCodeSwitches;
    'I', 'i': IsSet := cdIOChecks in FCodeSwitches;
    'U', 'u': IsSet := cdSafeDivide in FCodeSwitches;
    'M', 'm': IsSet := Self.FRttiInfo;
    'T', 't': IsSet := Self.FTypedAddress;
    'J', 'j': IsSet := Self.FWriteableConst;

  else
    isSet := False;
  end;
end;

procedure TParser.OpenCode(const S: string);
begin
  Scanner.Open(S);
end;

procedure TParser.OpenFile(const FileName: string);
begin
  Scanner.OpenFile(FileName);
end;

function TParser.Parse: TModule;
begin
  NextToken;
  case CurToken of
    tkProgram: Result := ParseProgram;
    tkUnit: Result := ParseUnit;
  else
    ParseError('unit, program expected', True);
    Result := nil;
  end;
end;

function TParser.ParseAddExpr: TExpr;

  function AddOp(T: TToken): TExprOpCode;
  begin
    case T of
      tkPlus                  : Result := opAdd;
      tkMinus                 : Result := opSUB;
      tkOr                    : Result := opOR;
      tkXor                   : Result := opXOR;
    else
      Result := opNone;
//      tkPower                 : Result := opPower;
//      tkSymmetricalDifference : Result := opSymmetricalDifference;
    end;
  end;
var
  left, right: TExpr;
  op: TExprOpCode;
begin
{
<AddExpr>		::= <MulExpr>
			  | <AddExpr> <AddOp> <MulExpr>
}
  Result := ParseMulExpr;
  op := AddOp(CurToken);
  while op <> opNONE do
  begin
    NextToken;
    left := Result;
    right := ParseMulExpr;
    Result := CreateBinaryExpr(op, left, right);
    op := AddOp(CurToken);
  end; 
end;

procedure TParser.ParseArgumentList(Parent: TSymbol; Args: TList);

{
<ParmType>  ::= <TypeRef>
            | ARRAY OF <TypeRef>
            | ARRAY OF CONST
            | FILE SynError
}
  function ParseArgumentType(out ArgKind: TArgumentKind): TType;
  begin
    ArgKind := akNormal;
    case CurToken of
      tkIdentifier, tkString:
        Result := ParseTypeRef;
      tkArray: begin
        NextToken;
        Expect(tkOf);
        NextToken;
        if CurToken = tkConst then
        begin
          Result := FContext.FVarOpenArrayType;
          NextToken;
        end
        else
        begin
          Result := ParseTypeRef;
          if Result = FContext.FVarRecType then
            Result := FContext.FVarOpenArrayType
          else
            Result := FContext.GetOpenArrayType(Result);
        end;

        if TOpenArrayType(Result).ElementType.TypeCode = typUntype then
          ArgKind := akArrayOfConst
        else
          ArgKind := akArrayOfType;
      end;

      tkFile: begin
        NextToken;
        if CurToken = tkOf then
        begin
          NextToken;
          ParseTypeDecl;
        end;
        ParseError('File type cannot use in here', True);
        Result := FContext.FIntegerType;
      end;
    else
      Result := FContext.FIntegerType;
    end;
  end;

  function ParseArgument: TArgument;
  var
    Arg: TArgument;
    Typ: TType;
    E: TExpr;
    DefValue: TValueRec;
    HasVal: Boolean;
    M: TArgumentModifier;
    ArgKind: TArgumentKind;
  begin
    M := argDefault;
    case CurToken of
      tkVar: M := argVar;
      tkConst: M := argConst;
      tkOut: M := argOut;
    end;

    if M <> argDefault then
      NextToken;

    // parse list
    Arg := TArgument(ParseIdList(TArgument));
    Result := Arg;

    if CurToken = tkColon then
    begin
      Expect(tkColon);
      NextToken;

      Typ := ParseArgumentType(ArgKind);
    end
    else
    begin
      Typ := FContext.FUntype;
      ArgKind := akUntype;
    end;

    if Typ = nil then Typ := FContext.FUntype;

    HasVal := False;
    ValInit(DefValue);
    try
      // parse default value
      if CurToken = tkEqual then
      begin
        NextToken;

        E := ParseExpr;

        if (ArgKind in [akArrayOfType, akArrayOfConst])
            or (Typ.TypeCode = typUntype) then
          ParseError('Argument of this cannot have default values')
        else begin
          HasVal := CheckExpr(E);
          if HasVal then
          begin
            DefValue := EvalExpr(E);
            if not CheckAssignmentCompatibility(Typ, E.Typ) then
              ParseError('Assignment incompatibility');
          end;
        end;
        ClearTempExprList;
      end;

      // set arg info
      while Arg <> nil do
      begin
        if HasVal then
          Arg.DefaultValue := ValCopy(DefValue);
        Arg.Modifier := M;
        Arg.ArgType := Typ;
        Arg.ArgKind := ArgKind;
        Arg := TArgument(Arg.Next);
      end;
    finally
      ValClear(DefValue);
    end;
  end;

  procedure CheckDefaultArg(Args: TList);
  var
    i: Integer;
    Arg: TArgument;
    flag: Boolean;
  begin
    flag := False;
    for i := 0 to Args.Count - 1 do
    begin
      Arg := TArgument(Args[i]);
      if Arg.DefaultValue.VT <> vtEmpty then
        flag := True;
      if flag and (Arg.DefaultValue.VT = vtEmpty) then
      begin
        ParseError(Arg.Coord, 'Default value required for %s', [Arg.Name]);
        Break;
      end;
    end;
  end;
var
  Arg: TArgument;
  EndToken: TToken;
begin
  if CurToken = tkBraceOpen then
    EndToken := tkBraceClose
  else
    EndToken := tkSquaredBraceClose;
  NextToken; // skip '('
  if CurToken <> EndToken then
  begin
    repeat
      Arg := ParseArgument;
      while Arg <> nil do
      begin
        Arg.Parent := Parent;
        Args.Add(Arg);

        Arg := TArgument(Arg.Next);
      end;

      if CurToken = tkSemicolon then
        NextToken
      else
        Break;
    until False;
  end;
  Expect(EndToken);
  NextToken;
  CheckDefaultArg(Args);
end;

procedure TParser.ParseBlock(Parent: TSymbol);

  function ParseEntryCode(const AName: string): TFunction;
  var
    StateInfo: TParseStateInfo;
  begin
    Result := TFunction(CreateElement(TFunction));
    Exclude(Result.Attr, saInternal);
    Result.Name := AName;
    Result.Parent := FModule;
    FCurFunction := Result;

    StateSet(psInFunc, StateInfo);
    Result.StartStmt := ParseCompoundStmt;
    StateRestore(StateInfo);
    FCurFunction := nil;
  end;
begin
{
<Block>			::= <OptDeclSection> <OptExportBlock> <CompoundStmt> <OptExportBlock>
}
  while True do
  begin
    case CurToken of
      tkType: Self.ParseTypeSection(Parent);
      tkVar, tkThreadVar: Self.ParseVarSection(Parent);
      tkConst: Self.ParseConstSection(Parent);
      tkResourceString: Self.ParseResStringSection(Parent);
      tkProcedure, tkFunction, tkConstructor, tkDestructor: Self.ParseFunction(Parent);

      tkClass:
        begin
          NextToken;
          if CurToken in [tkProcedure, tkFunction] then
          begin
            Include(Self.FCurStates, psInClassPrefix);
            Self.ParseFunction(FModule);
          end
          else
            ParseError(SErr_ExpectProcOrFunc, True);
        end;
      tkLabel: Self.ParseLabelSection(Parent);
    else
      Expect(tkBegin);
      FModule.InitializeFunc := ParseEntryCode('$main');
      Expect(tkDot);
      Break;
    end;
  end;
end;

function TParser.ParseCaseStmt: TCaseStmt;

  procedure IncompatibleErr(const Coord: TAstNodeCoord; t1, t2: TTypeCode);
  begin
    ParseError(Coord, 'Incompatible types: ''%s'' and ''%s''',
                [TypeNames[t1], TypeNames[t2]]);
  end;

  function CheckCaseList(List: TExpr; Typ: TType): Boolean;
  var
    E: TExpr;
    T2: TType;
  begin
    Result := CheckExpr(List);
    if not Result then Exit;

    E := TUnaryExpr(List).Operand;
    while E <> nil do
    begin
      T2 := E.Typ.NormalType;
      if not Typ.Equals(T2) then
      begin
        Result := False;
        IncompatibleErr(E.Coord, Typ.TypeCode, T2.TypeCode);
        Break;
      end;
      E := TExpr(E.Next);
    end;
  end;

  function Contains(Stmt: TCaseStmt; Start, Stop: Int64): Boolean;
  var
    i: Integer;
  begin
    for i := 0 to Stmt.Count - 1 do
      if Stmt.Selectors[i].Contains(Start, Stop) then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;
  procedure AddRanges(Selector: TCaseSelector; Stmt: TCaseStmt; List: TExpr);
  var
    E: TExpr;
    Start, Stop: Int64;
  begin
    E := TUnaryExpr(List).Operand;
    Start := 0;
    Stop := 0;
    while E <> nil do
    begin
      ValClear(FTempValue);
      if E.OpCode = opRANGE then
      begin
        if not EvalExpr(TBinaryExpr(E).Left, FTempValue) then Break;
        Start := ValToInt64(FTempValue);

        if not EvalExpr(TBinaryExpr(E).Right, FTempValue) then Break;
        Stop := ValToInt64(FTempValue);
      end
      else
      begin
        if not EvalExpr(E, FTempValue) then Break;
        Start := ValToInt64(FTempValue);
        Stop := Start;
      end;
      if not Contains(Stmt, Start, Stop) then
        Selector.AddRange(Start, Stop)
      else
        ParseError(E.Coord, 'Case label duplicated');

      E := TExpr(E.Next);
    end;
  end;
var
  E: TExpr;
  Selector: TCaseSelector;
begin
  Result := TCaseStmt(CreateStmt(TCaseStmt));
  NextToken;
  Result.Expr := ParseExpr;
  if CheckExpr(Result.Expr) then
  begin
    if not Result.Expr.Typ.IsOrdinal then
      ParseError(Result.Expr.Coord, SErr_OrdinalRequired);
    if Result.Expr.IsTypeSymbol then
      ParseError(Result.Expr.Coord, SErr_VarRequired);
  end;
  Expect(tkOf);
  NextToken;

  while True do
  begin
    case CurToken of
      tkElse: begin
        NextToken;
        Result.Default := Self.ParseStmtList(Result, [tkEnd]);
      end;
      tkEnd:
        Break;
    else
      SetTempExpr(True);
      E := ParseSetElementList;
      CheckCaseList(E, Result.Expr.Typ);

      Expect(tkColon);
      NextToken;

      // parse stmt;
      Selector := TCaseSelector.Create;
      Result.AddSelector(Selector);
      AddRanges(Selector, Result, E);

      Selector.Stmt := ParseStatement(Result);
//      Expect(tkSemicolon);
      if CurToken = tkSemicolon then NextToken;
    end;
  end;
  Expect(tkEnd);
  NextToken;

  // todo 1: 检查case值是否重复
end;

function TParser.ParseClassRefType: TClassRefType;
var
  Typ: TType;
  Sym: TSymbol;
begin
  NextToken; // skip 'of'
  sym := ParseQualifiedSym;
  if Sym = nil then
  begin
    if FQId.CountOfNames = 1 then
    begin
      Typ := TUnresolvedType.Create; // 不使用CreateElement,因为这个TUnresolvedType要释放的
      Typ.Name := FQID.Name;
    end
    else
    begin
      ParseError(SErr_UndeclaredIdent, [FQId.Id]);
      Typ := FContext.FTObjectType;
    end;
  end
  else if Sym.NodeKind <> nkType then
  begin
    ParseError(SErr_SymbolNotType, [FQId.Id]);
    Typ := FContext.FTObjectType;
  end
  else
    Typ := TType(Sym);

  FQID.Reset;
  {if Typ.TypeCode = typClass then
    Result := TClassType(Typ).GetClassRef
  else begin
    parseError(SErr_ClassRequired);
    Result := FContext.FTObjectType.GetClassRef
  end;}

  Result := TClassRefType(CreateType(TClassRefType));
  if Typ.TypeCode = typClass then
    Result.RefType := TClassType(Typ)
  else begin
    Result.RefType := FContext.FTObjectType;
    parseError(SErr_ClassRequired);
  end;
end;

function TParser.ParseClassType(const TypName: string;
    Parent: TSymbol; out NotAddSym: Boolean): TClassType;

  procedure ParseBaseClass(ClassTyp: TClassType);
  var
    Typ: TSymbol;
  begin
    NextToken; // skip '('
    Typ := ParseQualifiedSym;
    if Typ.NodeKind = nkType then
      Typ := TType(Typ).OriginalType;
    if (Typ.NodeKind <> nkType) or (TType(Typ).TypeCode <> typClass) then
      ParseError(SErr_InvalidBaseClass)
    else
      Result.Base := TClassType(Typ);
    while CurToken = tkComma do
    begin
      // 分析接口列表
      ClassTyp.CreateInterfaces;

      NextToken;
    {  Typ := ParseTypeRef;
      if Typ.TypeCode <> typInterface then
        ParseError(SErr_ExpectIntfType)
      else
        ClassTyp.Interfaces.Add(Typ);}
      Typ := ParseQualifiedSym;
      if (Typ.NodeKind <> nkType) or (TType(Typ).TypeCode <> typInterface) then
        ParseError(SErr_ExpectIntfType)
      else
        ClassTyp.Interfaces.Add(Typ);
    end;

    Expect(tkBraceClose);
    NextToken;
  end;

  function FindForwardClass(const S: string): TClassType;
  var
    E: TSymbol;
  begin
    E := CurSymbols.Find(S);
    if Assigned(E) and (E.ClassType = TClassType) and (saForward in E.Attr) then
      Result := TClassType(E)
    else
      Result := nil;
  end;

  procedure CheckOverride(Result: TClassType; M: TMethod);
  var
    E: TSymbol;
    M2: TMethod;
    I: Integer;
    A1, A2: TArgument;
    Ok: Boolean;
  begin
    E := Result.FindBaseSymbol(M.Name);
    if (E = nil) or (E.NodeKind <> nkMethod) then
    begin
      ParseError('Virtual method not found in base');
      Exit;
    end;
    M2 := TMethod(E);
    Ok := M2.CountOfArgs = M.CountOfArgs;
    if Ok then
      for I := 0 to M2.CountOfArgs - 1 do
      begin
        A1 := TArgument(M.Args[I]);
        A2 := TArgument(M2.Args[I]);
        if (A1.Modifier <> A2.Modifier) and (A1.ArgType <> A2.ArgType) then
        begin
          Ok := False;
          Break;
        end;
      end;
    if not Ok then
      ParseError('Overrided function not matched');
    M.VTIndex := M2.VTIndex;
    Include(M.Modifiers, fmVirtual);
  end;

  function IsSameMethodDecl(F1, F2: TMethod): Boolean;
  begin
    Result := (F1.ReturnType = F2.ReturnType)
            and IsSameArgs(F1.Args, F2.Args)
            and (F1.CallConvention = F2.CallConvention)
            and (F1.MethodKind = F2.MethodKind)
            and not ((saClass in F2.Attr) or (saStatic in F2.Attr));
            // todo 1: 不需检查这个??
            {
            and ((saClass in F1.Attr) = (saClass in F2.Attr))
            and ((saStatic in F1.Attr) = (saStatic in F2.Attr));}
  end;

  procedure CheckImplMethod(IntfMeth, ImplMeth: TMethod);
    function DoCheck(IntfMeth, ImplMeth: TMethod): Boolean;
    begin
      Result := True;
      repeat
        if IsSameMethodDecl(IntfMeth, ImplMeth) then Exit;

        ImplMeth := TMethod(ImplMeth.NextOverload);
      until ImplMeth = nil;
      Result := False;
    end;
  begin
    repeat
      if DoCheck(IntfMeth, ImplMeth) then Exit; // 只要有一个声明符合即可
      IntfMeth := TMethod(IntfMeth.NextOverload);
    until IntfMeth = nil;
    ParseError(ImplMeth.Coord, SErr_ImplMethodDiffers, [ImplMeth.Name]);
  end;

  procedure CheckIntf(Result: TClassType; Intf: TInterfaceType);
  var
    I: Integer;
    IntfMeth, ImplMeth: TSymbol;
  begin
    for I := 0 to Intf.AllSymbols.Count - 1 do
    begin
      IntfMeth := Intf.AllSymbols[I];
      if IntfMeth.NodeKind <> nkMethod then Continue;

      ImplMeth := Result.FindSymbol(IntfMeth.Name);

      if ImplMeth = nil then // Not impl
        ParseError(Result.Coord, SErr_IntfMethodNotImpl, [IntfMeth.Name])
      else if not IsVisible(Result, ImplMeth) then
        // can not access by this class
        ParseError(ImplMeth.Coord, SErr_SymbolNotAccess, [ImplMeth.Name])
      else if ImplMeth.NodeKind <> nkMethod then // is not method
        ParseError(ImplMeth.Coord, SErr_ExpectMethod)
      else
        CheckImplMethod(TMethod(IntfMeth), TMethod(ImplMeth));
    end;
  end;

{  procedure CheckDup(Result: TClassType; M: TMethod);
  var
    DupSym: TSymbol;
  begin
    while M <> nil do
    begin
      DupSym := Result.FindBaseSymbol(M.Name);
      if DupSym.NodeKind = nkMethod then
        DoCheckDup
      M := TMethod(M.NextOverload);
    end;
  end; }

  procedure CheckClass(Result: TClassType);
  var
    I, Def: Integer;
    Sym: TSymbol;
  begin
    if Result.Base <> nil then
      if caSealed in Result.Base.ClassAttr then
        ParseError(Result.Coord, SErr_BaseClassSealed);

    Def := 0;
    for I := 0 to Result.Symbols.Count - 1 do
    begin
      Sym := Result.Symbols[I];
      // 检查虚函数
      case Sym.NodeKind of
        nkMethod:
      // todo 1: 考虑这种情况：既overload又override
          if fmOverride in TMethod(Sym).Modifiers then
            CheckOverride(Result, TMethod(Sym));
        nkProperty:
          // 检查默认属性只能有一个
          if paDefaultProp in TProperty(Sym).PropAttr then
            Inc(Def);
      end;
    end;

    if Def > 1 then
      ParseError(Result.Coord, 'Only one default property can inside class,object,record,interface declaraction');
    // todo 1: 检查属性重声明
    //todo 1: 检查接口实现
    if Result.Interfaces <> nil then
    begin
      for I := 0 to Result.Interfaces.Count - 1 do
      begin
        Sym := TSymbol(Result.Interfaces[I]);
        if (TType(Sym).TypeCode = typInterface) then
          CheckIntf(Result, TInterfaceType(Sym));
      end;
    end;

    // todo 1: 检查与基类相同的函数
   { for I := 0 to Result.Members.Count - 1 do
    begin
      Sym := TSymbol(Result.Members[I]);
      if Sym.NodeKind = nkMethod then
        CheckDup(Result, TMethod(Sym));
    end; }

    // todo 1: 检查是否全部实现了abstract类的所有virtual方法
  end;

  procedure CheckMethodResolution(typ: TClassType);

    function GetImplMethod(const S: string): TMethod;
    var
      sym: TSymbol;
    begin
      sym := typ.Symbols.Find(S);
      if sym <> nil then
      begin
        if sym.NodeKind <> nkMethod then
          ParseError(SErr_ExpectMethod);
      end
      else
        ParseError(SErr_UndeclaredIdent, [S]);
      Result := TMethod(sym);
    end;
  var
    MR: TMethodResolution;
  begin
    MR := typ.MR;
    while MR <> nil do
    begin
      MR.ImplementingMethod := GetImplMethod(MR.Name);
      MR := TMethodResolution(MR.Next);
    end;
  end;
var
  Field: TField;
  MethSym: TSymbol;
  Prop: TProperty;
  OldErr: Integer;
  StateInfo: TParseStateInfo;
  OldParent: TSymbol;
  OldVis: TMemberVisibility;
  ClassPrefix, ClassVar: Boolean;
begin
//  NextToken;
  // CurToken is a token after 'class'

  Result := FindForwardClass(TypName);
  if Result <> nil then
  begin
    Exclude(TSymbol(Result).Attr, saForward);
    NotAddSym := True;
  end
  else
  begin
    Result := TClassType(CreateElement(TClassType));
    Result.Name := TypName;
    NotAddSym := False;
  end;

  // forward decl
  if CurToken = tkSemicolon then
  begin
    Include(TSymbol(Result).Attr, saForward);
    Exit;
  end;

  Result.GlobalAlignSize := FAlignSize;
  if psInPacked in FCurStates then
    Result.GlobalAlignSize := 1;

  if FRttiInfo then
    Include(Result.ClassAttr, caRtti);

  StateSet(psInClass, StateInfo);

  if CurToken = tkIdentifier then
  begin
    if SameText(CurTokenString, 'abstract') then begin
      Include(Result.ClassAttr, caAbstract);
      NextToken;
    end
    else if SameText(CurTokenString, 'sealed') then begin
      Include(Result.ClassAttr, caSealed);
      NextToken;
    end;
  end;

  if CurToken = tkBraceOpen then
    ParseBaseClass(Result);

  if Result.Base = nil then
    Result.Base := FContext.FTObjectType;

  // TMyObj = class(TObject); 这种情况是一个完整的类定义
  if CurToken = tkSemicolon then begin
    StateRestore(StateInfo);
    Exit;
  end;

  OldErr := FErrorCount;
  OldVis := FCurVisibility;
  if Result.RttiEnabled then
    FCurVisibility := visPublished
  else
    FCurVisibility := visPublic;
  ClassPrefix := False;
  ClassVar := False;

  if not NotAddSym then
  begin
    AddSymbol(Result);
    NotAddSym := True;
  end;
  EnterScope(Result.Symbols);

  OldParent := FCurParent;
  FCurParent := Result;
  while True do
  begin
    case CurToken of
      tkPrivate: begin
        FCurVisibility := visPrivate;
        NextToken;
      end;
      tkProtected: begin
        FCurVisibility := visProtected;
        NextToken;
      end;
      tkPublic: begin
        FCurVisibility := visPublic;
        NextToken;
      end;
      tkPublished: begin
        FCurVisibility := visPublished;
        NextToken;
      end;
      tkStrict: begin
        NextToken;
        if CurToken = tkPrivate then
          FCurVisibility := visStrictPrivate
        else if CurToken = tkProtected then
          FCurVisibility := visStrictProtected
        else
          ParseError(SErr_ExpectProtectOrPrivate, True);
        NextToken;
      end;

      tkIdentifier:
        begin
          Field := ParseField(TField);
          while Field <> nil do
          begin
            Field.Visibility := FCurVisibility;
            if ClassVar then
              Include(Field.Attr, saStatic);
            AddSymbol(Field);
            Field := TField(Field.Next);
          end;
          Expect(tkSemicolon);
          NextToken;
        end;
      tkVar: begin
        NextToken;
        ClassVar := False;
      end;
      tkConst: begin
        Self.ParseConstSection(Result);
      end;
      tkType: begin
        Self.ParseTypeSection(Result);
      end;
      tkClass: begin
        ClassPrefix := True;
        NextToken;
        if CurToken = tkVar then
        begin
          ClassVar := True;
          NextToken;
        end
        else if not (CurToken in [tkFunction, tkProcedure, tkProperty]) then
          ParseError(SErr_ExpectMethodOrProperty, True);
      end;
      tkFunction, tkProcedure, tkConstructor, tkDestructor: begin
        // todo 1: 需要增加MethodResolution

        MethSym := ParseFunction(Result);
        case MethSym.NodeKind of
          nkMethod: begin
            MethSym.Visibility := FCurVisibility;

            if ClassPrefix then
              Include(MethSym.Attr, saClass);
            if fmStatic in TMethod(MethSym).Modifiers then
              Include(MethSym.Attr, saStatic);
          end;
          nkMethodResolution: begin
            if ClassPrefix then
              ParseError(MethSym.Coord, 'Method resolution clause may not need class prefix');
            MethSym.Next := Result.MR;
            Result.MR := TMethodResolution(MethSym);
          end;
        end;
        ClassPrefix := False;
      end;
      tkProperty: begin
        Prop := ParseProperty(Result, ClassPrefix);
        Prop.Visibility := FCurVisibility;
        AddSymbol(Prop);
        if ClassPrefix then
          Include(Prop.Attr, saStatic);
        if paDefaultProp in Prop.PropAttr then
          Result.DefaultProp := Prop;
      end;
      tkEnd: Break;
    else
      Expect(tkIdentifier);
    end;
  end;

  Expect(tkEnd);
  NextToken;
  Result.Hints := ParseHints;

  StateRestore(StateInfo);
  LeaveScope;

  FCurParent := OldParent;
  FCurVisibility := OldVis;

  // 检查Symbols
  if FErrorCount = OldErr then
  begin
    CheckMethodResolution(Result);
    CheckClass(Result);
  end;
  if FErrorCount = OldErr then
    Result.Update(Self.FPointerSize);
end;

function TParser.ParseCompoundStmt: TCompoundStmt;
var
  Stmt: TStatement;
begin
  NextToken; // skip 'begin'
  Result := TCompoundStmt(CreateStmt(TCompoundStmt));
  Result.IncludeBegin := True;
  while CurToken <> tkEnd do
  begin
    Stmt := ParseStatement(Result);
    if Stmt <> nil then
      Result.Statements.Add(Stmt);
    if CurToken = tkSemicolon then NextToken;
  end;
  NextToken; // skip 'end'
end;

function TParser.ParseConstExpr: TExpr;
begin
  Result := ParseExpr;
end;

procedure TParser.ParseConstSection(Parent: TSymbol);
  procedure ParseConstArray(Typ: TArrayType; var Value: TValueRec);
  begin
    
  end;
var
  C: TSymbol;
  Typ: TType;
  E: TExpr;
  StateInfo: TParseStateInfo;
begin
{
<ConstantDecl>		::= <RefId> '=' <ConstExpr> <OptPortDirectives> ';'
			  | <RefId> ':' <Type> '=' <TypedConstant> <OptPortDirectives> ';'
}
  NextToken; // skip 'const'

  StateSet(psInVar, StateInfo);
  Expect(tkIdentifier);
  repeat
    FTemp := CurTokenString;
    NextToken;
    if CurToken = tkColon then
    begin
      C := CreateElement(TVariable);
      C.Name := FTemp;
      NextToken;
      Typ := ParseTypeDecl;
      TVariable(C).VarType := Typ;
      Include(TVariable(C).VarAttr, vaReadOnly);
    end
    else
    begin
      C := CreateElement(TConstant);
      C.Name := FTemp;
      Typ := nil;
    end;
    FTemp := '';
    Expect(tkEqual);
    NextToken;

    C.Visibility := FCurVisibility;
    ValClear(FTempValue);
    // parse init value
    if (C.NodeKind = nkVariable) and
      (TVariable(c).VarType.TypeCode in [typArray, typRecord]) then
    begin
      ParseError(c.Coord, '目前不支持记录和数组的常量表达式', True);
    end;
    E := ParseExpr;
    if CheckExpr(E) then
    begin
      FTempValue := EvalExpr(E);
      if Assigned(Typ) then
      begin
        if not CheckAssignmentCompatibility(Typ, E.Typ) then
          ParseError(SErr_AssignIncomp);
      end
      else
        TConstant(C).ConstType := E.Typ;
      if C.NodeKind = nkConstant then
        ValCopy(TConstant(C).Value, FTempValue)
      else
        ValCopy(TVariable(C).Value, FTempValue);
      ValClear(FTempValue);
    end;

//    if CurToken = tkIdentifier then // parse hint
      C.Hints := ParseHints;

    Expect(tkSemicolon);
    AddSymbol(C);

    if (C.NodeKind = nkVariable) and (Parent.NodeKind in [nkFunc, nkMethod]) then
      TVariable(C).Level := TFunction(Parent).Level;
    NextToken;
  until CurToken <> tkIdentifier;
  StateRestore(StateInfo);
  ClearTempExprList;
end;

function TParser.ParseDesignator: TExpr;
var
  L, R: TExpr;
begin
{
<FieldDesignator>	::= <RefId>
			  | <FieldDesignator> '.' <RefId>

<Designator>		::= <FieldDesignator>
			  | <Designator> '.' <FieldDesignator>
			  | <Designator> '^'
			  | <Designator> '[' <ExprList> ']'
			  | <Designator> '(' <ExprList> ')' !FunctionCall or TypeCast
			  | <Designator> '('  ')'           !FunctionCall
		 	  | <Designator> AS <TypeRef>	!eg "with Source as TListItem do ..."
			  | '(' <Designator> ')'
			  | INHERITED <Designator>
}

  if CurToken = tkBraceOpen then
  begin
    NextToken;
    Expect(tkIdentifier);
    Result := ParseDesignator;
    Expect(tkBraceClose);
    NextToken;
    Exit;
  end;

  if CurToken = tkInherited then
  begin
    // inherited 优先度比较高:
    // inherited BaseMeth[1];  =>
    //   (inherited BaseMeth)[1];

    // todo: 这里需要修改
    NextToken;
    Expect(tkIdentifier);
    Result := TSymbolExpr(CreateSymbolExpr(CurTokenString));
    NextToken;
    if CurToken = tkBraceOpen then  // 参数
    begin
      L := Result;
      Result := CreateBinaryExpr(opCALL); 

      NextToken;
      if CurToken = tkBraceClose then
        R := nil
      else
        R := ParseExprList;

      TBinaryExpr(Result).Left := L;
      TBinaryExpr(Result).Right := R;
      Expect(tkBraceClose);
      NextToken;
    end;
  end
  else if CurToken = tkString then
  begin
    Result := CreateSymbolExpr(FContext.FStringType.Name);
    TSymbolExpr(Result).Reference := FContext.FStringType;
    NextToken;
  end
  else
  begin
  { //old code
    Expect(tkIdentifier);
    Result := CreateSymbolExpr(CurTokenString);
    NextToken;
  }
    ParseQualifiedId;
    Result := SimplifyQualId;
    FQId.Reset;
  end;

  while True do
  begin
    case CurToken of
      tkDot: begin
        L := Result;
        Result := CreateBinaryExpr(opMEMBER);

        // 允许保留字出现在tkDot(.)之后
        // 如 MyUnit.Type
        Scanner.NoReservedWord := True;
        NextToken;
        Scanner.NoReservedWord := False;
        Expect(tkIdentifier);
        R := CreateSymbolExpr(CurTokenString);
        TBinaryExpr(Result).Left := L;
        TBinaryExpr(Result).Right := R;

        NextToken;
      end;
      
      tkCaret: begin
        L := Result;
        Result := CreateUnaryExpr(opINST, L);
        NextToken;
      end;

      tkBraceOpen: begin // 函数调用或类型转换
        L := Result;
        Result := CreateBinaryExpr(opCALL); // 暂定为函数调用

        NextToken;
        if CurToken = tkBraceClose then
          R := nil
        else
          R := ParseExprList;

        if R <> nil then Include(R.Attr, eaArgList);
        TBinaryExpr(Result).Left := L;
        TBinaryExpr(Result).Right := R;
        Expect(tkBraceClose);
        NextToken;
      end;

      tkSquaredBraceOpen: begin
        NextToken;
        L := Result;
        Result := CreateBinaryExpr(opINDEX);

        if CurToken = tkSquaredBraceClose then
        begin
          R := nil;
          ParseError('Expression expected');
        end else
          R := ParseExprList;
        TBinaryExpr(Result).Left := L;
        TBinaryExpr(Result).Right := R;
        Expect(tkSquaredBraceClose);
        NextToken;
      end;
    else
      Break;
    end;
  end;
end;

procedure TParser.ParseError(const Msg: string; Stop: Boolean);
var
  Coord: TAstNodeCoord;
begin
  Coord.FileName := Scanner.CurFileName;
  Coord.Row := Scanner.CurRow;
  Coord.Col := Scanner.CurColumn;
  ParseError(Coord, Msg, Stop);
end;

procedure TParser.ParseError(const Msg: string; Args: array of const;
  Stop: Boolean);
begin
  ParseError(Format(Msg, Args), Stop);
end;

procedure TParser.ParseError(const Coord: TAstNodeCoord; const Msg: string;
  Stop: Boolean);
var
  Err: TParserErrorInfo;
begin
  Inc(FErrorCount);
  if FErrorCount > FMaxErrorCount then Stop := True;
  if Assigned(FOnError) then
  begin
    Err := TParserErrorInfo.Create;
    Err.Row := Coord.Row;
    Err.Column := Coord.Col;
    Err.FileName := Coord.FileName;
    Err.ErrorMessage := Msg;
    FOnError(Err);
    Err.Free;
    if Stop then
      raise EParserError.Create(Msg, Coord.FileName, Coord.Row, Coord.Col);
  end
  else begin
    raise EParserError.Create(Msg, Coord.FileName, Coord.Row, Coord.Col);
  end;
end;

procedure TParser.ParseError(const Coord: TAstNodeCoord; const Msg: string;
  Args: array of const; Stop: Boolean);
begin
  ParseError(Coord, Format(Msg, Args), Stop);
end;

function TParser.ParseExpr: TExpr;

  function RelOp(T: TToken): TExprOpCode;
  begin
    case T of
      tkLessThan              : Result := opLT;
      tkEqual                 : Result := opEQ;
      tkGreaterThan           : Result := opGT;
      tkNotEqual              : Result := opNE;
      tkLessEqualThan         : Result := opLE;
      tkGreaterEqualThan      : Result := opGT;
      tkIs                    : Result := opIS;
      tkAs                    : Result := opAS;
      tkIn                    : Result := opIN;
    else
      Result := opNone;
    end;
  end;

var
  left, right: TExpr;
  op: TExprOpCode;
begin
{
<Expr>			::= <AddExpr>
			  | <AddExpr> <RelOp> <AddExpr>
			  | SynError
}
  Result := ParseAddExpr;

  op := RelOp(CurToken);
  while op <> opNONE do
  begin
    NextToken;
    left := Result;
    right := ParseAddExpr;
    Result := CreateBinaryExpr(op, left, right);
    op := RelOp(CurToken);
  end;
end;

function TParser.ParseExpr2: TExpr;
begin
//  FError := False;
  FErrorCount := 0;
  ClearNodes;
  NextToken;
  Result := ParseExpr;
  CheckExpr(Result);
end;

function TParser.ParseExprList: TExpr;
var
  L, L2: TExpr;
begin
{<ExprList>		::= <Expr>
			  | <ExprList> ',' <Expr>
}
// 读取表达式列表, 数目必须>=1
  Result := CreateUnaryExpr(opLIST);
  L := ParseExpr;
  TUnaryExpr(Result).Operand := L;
  while CurToken = tkComma do
  begin
    NextToken;
    L2 := ParseExpr;
    L.Next := L2;
    L2.Parent := L.Parent;
    L := L2;
  end;
end;

function TParser.ParseFactor: TExpr;

  function DecodeConst: TExpr;
  var
    V: TValueRec;
    I: Int64;
    OP: TExprOpCode;
    S: string;
  begin
    ValInit(V);
    case CurToken of
      tkTrue, tkFalse: begin
        V := ValFromBool(CurToken = tkTrue);
        OP := opBOOLCONST;
      end;

      tkBinConst, tkOctalConst, tkIntConst, tkHexConst:
        begin
          I := FScanner.TokenValue.IntValue;
          if I <= $7fffffff then
            ValFromInt(V, Integer(I))
          else
            ValFromInt(V, I);
          OP := opINTCONST;
        end;

      tkStrConst, tkCharConst:
        begin
          S := CurTokenString;
          if Length(S) = 1 then
          begin
            ValFromChar(V, S[1]);
            OP := opCHARCONST;
          end else
          begin
            ValFromStr(V, S);
            OP := opSTRCONST;
          end;
        end;
    else // float
      ValFromReal(V, FScanner.TokenValue.RealValue);
      OP := opREALCONST;
    end;
    Result := CreateConstExpr(OP);
    TConstExpr(Result).Value := V;
    NextToken;
  end;

var
  L: TExpr;
begin
{
<Factor>		::= NIL
			  | <ICONST>
			  | <RCONST>
			  | <SCONST>
			  | <Designator>
			  | <SetConstructor>
			  | '@' <Designator>
			  | '@' '@' <Designator> !returns memory address of a procedural variable
			  | '(' <Expr> ')'
			  | '(' <Expr> ')' '^'
			  | '+' <Factor>
			  | '-' <Factor>
			  | NOT <Factor>
}
  case Self.CurToken of
    tkNil: begin
      Result := CreateConstExpr(opNIL);
      Result.Typ := FContext.FTypes[typPointer];
      TConstExpr(Result).Value := ValFromPtr(nil);
      NextToken;
    end;

    tkIntConst, tkHexConst, tkOctalConst, tkFloatConst, tkBinConst,
    tkStrConst, tkCharConst: begin
      Result := DecodeConst;
    end;

    tkPlus: begin
      NextToken;
    //  L := ParseFactor;
    //  Result := CreateUnaryExpr(opPOS, L);
      Result := ParseFactor;
    end;

    tkMinus: begin
      NextToken;
      L := ParseFactor;
      Result := CreateUnaryExpr(opNEG, L);
    end;

    tkNot: begin
      NextToken;
      L := ParseFactor;
      Result := CreateUnaryExpr(opNOT, L);
    end;

    tkBraceOpen: begin
      NextToken;
      Result := ParseExpr;
      Expect(tkBraceClose);
      NextToken;
      if CurToken = tkCaret then
      begin
        L := Result;
        Result := CreateUnaryExpr(opINST, L);
        NextToken;
      end;
    end;

    tkSquaredBraceOpen:
      Result := ParseSetConstructor;

    tkAt: begin
      Result := CreateUnaryExpr(opAddr, nil);
      NextToken;
      if CurToken = tkAt then
      begin
        NextToken;
        L := Result;
        Result := CreateUnaryExpr(opAddr, L);
        TUnaryExpr(L).Operand := ParseDesignator;
      end
      else
        TUnaryExpr(Result).Operand := ParseDesignator;
    end;

    tkIdentifier, tkInherited, tkString: begin
      Result := ParseDesignator;
    end;

    tkTrue, tkFalse: begin
      Result := CreateConstExpr(opBOOLCONST);
      TConstExpr(Result).Value := ValFromBool(CurToken = tkTrue);
      NextToken;
    end;

  else
//      ParseError('ident expected');
    Expect(tkIdentifier);
    Result := CreateUnaryExpr(opNONE, nil);

  end;
end;

function TParser.ParseField(FieldClass: TSymbolClass): TField;
var
  Field: TField;
  Typ: TType;
  Hints: TMemberHints;
  StateInfo: TParseStateInfo;
begin
  StateSet(psInField, StateInfo);

  Field := TField(ParseIdList(FieldClass));

  Expect(tkColon);
  NextToken;

  Typ := ParseTypeDecl;
  Hints := [];
//  if CurToken = tkIdentifier then // parse hint
    Hints := ParseHints;
  Result := Field;
  while Field <> nil do
  begin
    Field.FieldType := Typ;
    Field.Hints := Hints;
    Field := TField(Field.Next);
  end;
  StateRestore(StateInfo);
end;

function TParser.ParseForStmt: TForStmt;
var
  E: TSymbol;
  Typ: TType;
  StateInfo: TParseStateInfo;
begin
  NextToken; // skip 'for'
  Expect(tkIdentifier);

  Typ := nil;
  E := FindSymbol(CurTokenString);
  if E = nil then
    ParseError(SErr_UndeclaredIdent, [CurTokenString])
  else begin
    Include(E.Attr, saUsed);
    case E.NodeKind of
      nkVariable: Typ := TVariable(E).VarType;
      nkArgument: Typ := TArgument(E).ArgType;
    else
      ParseError('%s must be simple local variable', [CurTokenString]);
    end;
    if (Typ <> nil) and not Typ.IsOrdinal then
      ParseError('For loop control variable must be ordinal type', [CurTokenString]);
  end;

  // todo 1: 要检查循环控制变量不能被赋值
  Result := TForStmt(CreateStmt(TForStmt));
  Result.Value := E;

  NextToken;
  Expect(tkAssign);

  NextToken;
  Result.Start := ParseExpr;

  if CurToken = tkDownto then
    Result.Down := True
  else if CurToken = tkTo then
    Result.Down := False
  else
    Expect(tkTo);

  NextToken;
  Result.Stop := ParseExpr;

  // 检查表达式
  if (Typ <> nil) and CheckExpr(Result.Start) and
      CheckExpr(Result.Stop) then
  begin
    if not CheckAssignmentCompatibility(Typ, Result.Start.Typ) then
      ParseError(Result.Start.Coord, SErr_IncompatibleTypes);
    if not CheckAssignmentCompatibility(Typ, Result.Stop.Typ) then
      ParseError(Result.Stop.Coord, SErr_IncompatibleTypes);
  end;

  Expect(tkDo);
  NextToken;
  StateSet(psInForStmt, StateInfo);
  Result.Stmt := ParseStatement(Result);
  StateRestore(StateInfo);
end;

function TParser.ParseFunction(Parent: TSymbol): TSymbol;

  function FindExists(const S: string): TFunctionDecl;
  var
    E: TSymbol;
  begin
    E := CurSymbols.Find(S);
    if (E <> nil) and (E.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
      Result := TFunctionDecl(E)
    else
      Result := nil;
  end;

  function SameFuncDecl(F1: TFunctionDecl; F2: TFunctionHeader): Boolean;

    function SameArg(A1, A2: TArgument): Boolean;
    begin
      // todo 1: 要比较函数名
      Result := (A1.ArgType = A2.ArgType) and
                (A1.Modifier = A2.Modifier) and
                (A1.ArgKind = A2.ArgKind);
    end;

    function SameArgs: Boolean;
    var
      I: Integer;
    begin
      Result := False;
      if F1.CountOfArgs <> F2.Args.Count then Exit;

      for I := 0 to F1.CountOfArgs - 1 do
        if not SameArg(TArgument(F1.Args[I]),
                       TArgument(F2.Args[I])) then Exit;

      Result := True;
    end;

    function SameCallConv: Boolean;
    const
      CCIDs = [idRegister, idPascal, idCDecl, idStdCall, idSafeCall];
    begin
      Result := CCIDs * F2.Directives = [];
      if not Result then
        Result := F1.CallConvention = F2.CallConvention;
    end;
  begin
    Result := ((F2.MethodKind = mkConstructor) or (F1.ReturnType = F2.ReturnType))
            and SameArgs
            and SameCallConv
            and (F2.ClassPrefix = (saClass in F1.Attr));
  end;

{  function FindProperDecl(Func: TFunctionDecl; Header: TFunctionHeader): TFunctionDecl;
  begin
    Result := Func;
    while Result <> nil do
    begin
      if SameFuncDecl(Result, Header) then Exit;
      Result := Result.NextOverload;
    end;

    if Result = nil then
    begin
      // 找不到, 则使用第一个, 使分析可以继续进行
      ParseError('Declaration of %s differs from previous declaration', [Func.Name]);
      Result := Func;
    end;
  end; }

  // 查找符合Header的声明,找到则返回true, Func是那个函数
  // 未找到返回false,Func未变
  function FindProperDecl2(var Func: TFunctionDecl; Header: TFunctionHeader): Boolean; 
  var
    F: TFunctionDecl;
  begin
    Result := True;
    F := Func;
    while Func <> nil do
    begin
      if SameFuncDecl(Func, Header) then Exit;
      Func := Func.NextOverload;
    end;
    Result := False;
    Func := F;
  end;

  function FindFuncDecl(var Func: TFunctionDecl; Header: TFunctionHeader): Boolean;
  var
    E: TSymbol;
  begin
    E := CurSymbols.Find(Header.Name);
    if (E <> nil) and (E.NodeKind in [nkFunc, nkExternalFunc]) then
    begin
      // 查找一个合适的重载
      Func := TFunctionDecl(E);
      Result := FindProperDecl2(Func, Header);
    end
    else begin
      Result := False;
      Func := nil;
    end;
  end;

  function FindMethodDecl(var Func: TFunctionDecl; Header: TFunctionHeader): Boolean;
  var
    E: TSymbol;
    I: Integer;
  begin
    E := FModule.FindSymbol(Header.Names[0]);
//    E := FindSymbol(FModule, Header.Names[0]);
    if E = nil then
      ParseError(SErr_UndeclaredIdent, [Header.Names[0]], True);

    if (E.NodeKind <> nkType) or not (TType(E).TypeCode in
        [typClass, typRecord, typObject]) then
      ParseError('Identifier is not a class, record or object', True);

    for I := 1 to Header.CountOfNames - 1 do
    begin
      if Assigned(E) and (E.NodeKind = nkType) then
      begin
        case TType(E).TypeCode of
          typClass: E := TClassType(E).FindCurSymbol(Header.Names[I]);
          typRecord: E := TRecordType(E).FindSymbol(Header.Names[I]);
          typObject: E := TObjectType(E).FindCurSymbol(Header.Names[I]);
        else
          ParseError(SErr_UndeclaredIdent, [Header.Names[I]], True);
        end
      end
      else
        Break;
    end;

    if Assigned(E) and (E.NodeKind <> nkMethod) then
      ParseError('Identifier is not a method', True);

    Func := TFunctionDecl(E);
    Result := FindProperDecl2(Func, Header);
  end;

  procedure NotAllowDotted;
  begin
    if FHeader.CountOfNames > 0 then
      ParseError('Error in function declaraction', True);
  end;

  procedure AssignInfo(Func: TFunctionDecl; Header: TFunctionHeader);
  var
    I: Integer;
  begin
    if Header.Args.Count > 0 then
      Func.CreateArgs;
    for I := 0 to Header.Args.Count - 1 do
    begin
      TArgument(Header.Args[I]).Parent := Func;
      Func.Args.Add(Header.Args[I]);
    end;
    Func.ReturnType := Header.ReturnType;
    Func.Name := Header.Name;
    Func.Modifiers := Header.Modifiers;
    Func.CallConvention := Header.CallConvention;

    case Func.NodeKind of
      nkExternalFunc:
        begin
          TExternalFunction(Func).FileName := Header.FileName;
          TExternalFunction(Func).RoutineName := Header.RoutineName;
          TExternalFunction(Func).RoutineNo := Header.RoutineNo;
        end;

      nkMethod:
        begin
          TMethod(Func).MsgNo := Header.MsgNo;
          TMethod(Func).MethodKind := Header.MethodKind;
          TMethod(Func).ObjectKind := Header.ObjectKind;
          TMethod(Func).DispID := Header.MsgNo;
        end;
    end;
  end;

  function ToMethod(Header: TFunctionHeader): TFunctionDecl;
  begin
    Result := TMethod(CreateElement(TMethod));
    AssignInfo(Result, Header);
  end;

  function ToFunc(Header: TFunctionHeader): TFunctionDecl;
  begin
    if idExternal in Header.Directives then
      Result := TFunctionDecl(CreateElement(TExternalFunction))
    else
      Result := TFunctionDecl(CreateElement(TFunction));
    AssignInfo(Result, Header);
  end;

  function ToMethodResolution(Header: TFunctionHeader;
                              Parent: TSymbol): TMethodResolution;
  var
    typ: TClassType;

    function GetInterfaceMethod: TMethod;
    var
      i: Integer;
      intf: TInterfaceType;
      sym: TSymbol;
    begin
      Result := nil;
      if typ.Interfaces <> nil then
      begin
        for i := 0 to typ.Interfaces.Count - 1 do
        begin
          intf := TInterfaceType(typ.Interfaces[i]);
          if SameText(intf.Name, Header.Names[0]) then
          begin
            sym := intf.FindSymbol(Header.Names[1]);
            if sym.NodeKind = nkMethod then
            begin
              Result := TMethod(sym);
              Exit;
            end;
          end;
        end;
      end;
    end;
  begin
    if Parent.NodeKind <> nkType then
      typ := nil
    else if TType(Parent).TypeCode = typClass then
      typ := TClassType(Parent)
    else
      typ := nil;

    Result := TMethodResolution(CreateElement(TMethodResolution));
    if typ = nil then
      ParseError('Method resolution only inside class definition')
    else begin
      if FHeader.CountOfNames <> 2 then
      begin
        ParseError('Invalid method resolution');
      end;
      Result.Name := Header.ImplementingName;
      Result.InterfaceMethod := GetInterfaceMethod;
    end;
  end;

  procedure EnterClassScope(Owner: TSymbol);
  begin
    if Owner = nil then Exit;

    // 这里已经考虑了嵌套类的情况
    if (Owner.NodeKind = nkType) then
    begin
      EnterClassScope(Owner.Parent);
      case TType(Owner).TypeCode of
        typClass: EnterScope(TClassType(Owner).AllSymbols);
        typObject: EnterScope(TObjectType(Owner).AllSymbols);
        typRecord: EnterScope(TRecordType(Owner).Symbols);
      end;
    end;
  end;

  procedure LeaveClassScope(Owner: TSymbol);
  begin
    if Owner = nil then Exit;

    while Owner <> nil do
    begin
      if (Owner.NodeKind = nkType)
          and (TType(Owner).TypeCode in [typClass, typObject, typRecord]) then
      begin
        LeaveScope;
      end
      else
        Break;
      Owner := Owner.Parent;
    end;
  end;

  procedure CheckFunc(Func: TFunction);
  var
    i: Integer;
    sym: TSymbol;
  begin
    for i := 0 to Func.LocalSymbols.Count - 1 do
    begin
      sym := Func.LocalSymbols[i];
      if sym.NodeKind = nkFunc then
      begin
        if TFunction(sym).StartStmt = nil then
          ParseError(sym.Coord, SErr_FuncNotImpl, [sym.Name]);
        (*if (faNeedFPArg in TFunction(sym).FuncAttr) {and (Func.Level > 0)} then
          Include(Func.FuncAttr, faNeedFP);*)
      end;
    end;
  end;

var
  I: Integer;
  V: TVariable;
  Func: TFunctionDecl;
  OldFunc: TFunction;
  OldParent: TSymbol;
  ParentTyp: TType;
  StateInfo: TParseStateInfo;
  ClassPrefix: Boolean;
begin
  ClassPrefix := psInClassPrefix in FCurStates;
  if ClassPrefix then Exclude(FCurStates, psInClassPrefix);
  StateSet(psInFunc, StateInfo);
  ParseFunctionHeader(FHeader);
  StateRestore(StateInfo);

  FHeader.ClassPrefix := ClassPrefix;
  // in class / object / record
  // in interface section
  // in implement section

  if FHeader.ImplementingName <> '' then
  begin
    // 是一个方法解析语句
    Result := ToMethodResolution(FHeader, Parent);
    FHeader.Reset;
    Exit;
  end;

  // todo 2: 函数实现之处，参数列表可省略。如果有overload 则不行
  // todo 2: 这里的实现和fpc的$mode objpas相同,即实现部分不能省略参数.以后要兼容delphi

  if Parent.NodeKind = nkType then
    ParentTyp := TType(Parent)
  else
    ParentTyp := nil;

  if Assigned(ParentTyp) then
  begin
    case ParentTyp.TypeCode of
      typClass:  FHeader.ObjectKind := okClass;
      typObject: FHeader.ObjectKind := okObject;
      typRecord: FHeader.ObjectKind := okRecord;
    end;
    if FHeader.MethodKind = mkConstructor then
      FHeader.ReturnType := TType(ParentTyp);
  end;

  // 找到已经声明的
  if FCurStates * [psInClass, psInRecord, psInObject, psInIntf, psInDispIntf] <> [] then
  begin
    // 在class, record, object, interface, dispinterface 的声明部分
    NotAllowDotted;
    Func := FindExists(FHeader.Name);
    Result := ToMethod(FHeader);
    if (Func <> nil) then
    begin
      if CheckOverloads(Func, TFunctionDecl(Result)) then
      begin
        TFunctionDecl(Result).ID := Func.ID + 1;
        Func.AddOverload(TFunctionDecl(Result));
      end;
      Result.Parent := Parent;
    end
    else begin
      AddSymbol(Result);
    end;

    // 分析interface/dispinterface时不会对constructor/destructor进行分析
  {  if (FHeader.MethodKind in [mkConstructor, mkDestructor])
      and Assigned(ParentTyp) and not (ParentTyp.TypeCode in [typClass, typObject, typRecord]) then
    begin
      ParseError(Result.Coord, SErr_IntfNotAllowCtorDtor);
    end;}
    Include(TFunctionDecl(Result).Modifiers, fmForward);
  end
  else if psInIntfSect in FCurStates then
  begin
    // 在interface段中,只可能是函数和过程,不可能是方法
    NotAllowDotted;
    Func := FindExists(FHeader.Name);
    Result := ToFunc(FHeader);
    if Func <> nil then
    begin
      if CheckOverloads(Func, TFunctionDecl(Result)) then
      begin
        TFunctionDecl(Result).ID := Func.ID + 1;
        Func.AddOverload(TFunctionDecl(Result));
      end;
      Result.Parent := Parent;
    end
    else begin
      AddSymbol(Result);
    end;
    Include(TFunctionDecl(Result).Modifiers, fmForward);
  end
  else
  begin
  // todo 2: 要考虑方法已经实现，又给一次实现的错误代码
    Assert(psInImplSect in FCurStates);
    if FHeader.CountOfNames > 1 then
    begin
      // 方法
      if FindMethodDecl(Func, FHeader) then
        Result := Func   // todo 1: 要考虑这个Func已经实现过了
      else begin
        Result := ToMethod(FHeader);
        if Func <> nil then
        begin
          TFunctionDecl(Result).ID := Func.ID + 1;
          Func.AddOverload(TFunctionDecl(Result));
          Result.Parent := Func.Parent;
        end;
        ParseError('Method declaraction not found', Func = nil);
      end;
      // 假如存在多个声明,但都不符合,则报错,但创建一个新方法,以便继续分析
      Exclude(TFunctionDecl(Result).Modifiers, fmForward);
    end
    else
    begin
      // 假如找出多个声明,但不符合,则创建新的函数,
      // 这是implementation部分定义的新函数,不是错误
      if FindFuncDecl(Func, FHeader) then
      begin
        Result := Func;
        Exclude(TFunctionDecl(Result).Modifiers, fmForward);
      end
      else
      begin
        Result := ToFunc(FHeader);
      //  Result.IsInternal := True;
        if Func <> nil then
        begin
          if CheckOverloads(Func, TFunctionDecl(Result)) then
          begin
            TFunctionDecl(Result).ID := Func.ID + 1;
            Func.AddOverload(TFunctionDecl(Result));
          end;
          Result.Parent := Parent;
        end
        else begin
          AddSymbol(Result);
        end;
      end;
    end;
  end;

  FHeader.Reset;

  // todo 1: 这里需要检查overload.
  if fmForward in TFunctionDecl(Result).Modifiers then Exit;
  if fmExternal in TFunctionDecl(Result).Modifiers then Exit;

  if TFunction(Result).StartStmt <> nil then
  begin
    ParseError(Result.Coord, 'function definition duplicated');
    TFunction(Result).LocalSymbols.Clear;
    TFunction(Result).StartStmt := nil;
  end;

  StateSet(psInFunc, StateInfo);

  if (Result.NodeKind = nkMethod) then
    EnterClassScope(Result.Parent);

  // local symbols
  EnterScope(TFunction(Result).LocalSymbols);

  if (Result.NodeKind = nkMethod) and Assigned(Result.Parent) and
    not (fmStatic in TFunctionDecl(Result).Modifiers) then
  begin
    V := TVariable(CreateElement(TVariable));
    if saClass in Result.Attr then
      V.VarType := TClassType(Result.Parent).GetClassRef
    else
      V.VarType := TType(Result.Parent);
    V.Name := 'Self';
    V.VarAttr := [vaReadOnly, vaSelf, vaLocal];
    AddSymbol(V);
  end;

  if TFunctionDecl(Result).ReturnType <> nil then
  begin
    V := TVariable(CreateElement(TVariable));
    V.VarType := TFunctionDecl(Result).ReturnType;
    V.VarAttr := [vaResult, vaLocal];
    V.Attr := [saUsed];
    V.Name := 'Result';
    AddSymbol(V);
  end;

  OldFunc := fCurFunction;
  FCurFunction := TFunction(Result);
  OldParent := FCurParent;
  FCurParent := Result;
  if FTopFunction = nil then FTopFunction := FCurFunction;
  if OldFunc <> nil then
    FCurFunction.Level := OldFunc.Level + 1
  else
    FCurFunction.Level := 0;

  for I := 0 to TFunctionDecl(Result).CountOfArgs - 1 do
  begin
    TArgument(TFunctionDecl(Result).Args[I]).Level := FCurFunction.Level;
    AddSymbol(TArgument(TFunctionDecl(Result).Args[I]));
  end;

  ParseFunctionBlock(TFunction(Result));

  FCurFunction := OldFunc;
  if FCurFunction = nil then FTopFunction := nil;
  FCurParent := OldParent;

  LeaveScope;
  if Result.NodeKind = nkMethod then
    LeaveClassScope(Result.Parent);
  StateRestore(StateInfo);

  CheckFunc(TFunction(Result));
end;

procedure TParser.ParseFunctionBlock(Func: TFunction);
begin
  while True do
  begin
    case CurToken of
      tkType: Self.ParseTypeSection(Func);
      tkVar: Self.ParseVarSection(Func);
      tkConst: Self.ParseConstSection(Func);
      tkProcedure, tkFunction: Self.ParseFunction(Func);
      tkLabel: Self.ParseLabelSection(Func);
      tkResourceString: Self.ParseResStringSection(Func);
      tkThreadVar: ParseError('thread var can not in local', True);
    else
      Expect(tkBegin);
      Func.StartStmt := ParseCompoundStmt;
      Expect(tkSemicolon);
      NextToken;
      Break;
    end;
  end;
  // todo 1: 检查函数
  CheckFunction(Func);
end;

procedure TParser.ParseFunctionDirective(Result: TFunctionHeader);

  procedure ParseExternal;
  begin
    NextToken;
    if CurToken = tkSemicolon then Exit;

    Result.FileName := ParseStrExpr('1');

    NextToken;
    if CurToken = tkSemicolon then Exit;

    if SameText(CurTokenString, 'name') then
    begin
      Result.RoutineName := ParseStrExpr('1');
    end
    else if SameText(CurTokenString, 'index') then
    begin
      Result.RoutineNo := ParseIntExpr;
    end
    else
      ParseError('name or index expected', True);
    Self.ClearTempExprList;
  end;

  procedure SetFlag(Result: TFunctionHeader; D: TDirectiveIdent);
  begin
    Include(Result.Directives, D);
    if D in [idDeprecated..idUnimplemented] then
      Include(Result.Hints, TMemberHint(Ord(D) - Ord(idDeprecated)))
    else if D in [idRegister..idSafeCall] then
      Result.CallConvention := TCallingConvention(Ord(D) - Ord(idRegister) + 1)
    else
      Include(Result.Modifiers, TFunctionModifier(Ord(D) - Ord(idVirtual)));
  end;

  function IsEndId: Boolean;
  begin
    if psInClass in FCurStates then
    begin
      if SameText(CurTokenString, 'public') then
        Result := True
      else if SameText(CurTokenString, 'private') then
        Result := True
      else if SameText(CurTokenString, 'protected') then
        Result := True
      else if SameText(CurTokenString, 'published') then
        Result := True
      else if SameText(CurTokenString, 'strict') then
        Result := True
      else
        Result := False;
    end
    else
      Result := False;
  end;
var
  Directive: TDirectiveIdent;
begin
{ 此处语法不依object pascal
object pascal的指令有一定次序, 可以使用分号隔开,也可以不用
这里不管次序,分号是可选的

<CallDirectives>  ::= <CallDirective>
                    | <CallDirectives> <CallDirective>

<CallDirective>   ::= register | pascal | cdecl | stdcall | safecall |
                      inline | near | far | export | assmebler |
                      local | varargs | overload |
                      forward | external
}
  // 指令不需要用;与函数隔开
  while True do
  begin
    if CurToken = tkSemicolon then Break;

    Directive := FindDirective(CurTokenString);
    if (Directive in [idNone, idForward, idExternal]) then
      ParseError('Invalid function directive: %s', [CurTokenString], True)
    else begin
      SetFlag(Result, Directive);
    end;
    NextToken;
  end;

  Expect(tkSemicolon);

  // 分析 CallDirectives
  while True do
  begin
    if CurToken = tkSemicolon then
      NextToken;

    if CurToken in [tkVar, tkConst, tkResourceString, tkType,
                    tkLabel, tkClass, tkProperty, tkProcedure,
                    tkFunction, tkConstructor, tkDestructor,
                    tkBegin, tkEnd, tkImplementation,
                    tkPrivate, tkProtected, tkPublic, tkPublished,
                    tkStrict
                   ] then Break;

//    if (CurToken = tkIdentifier) and (IsEndId) then Break;

    Directive := FindDirective(CurTokenString);
    case Directive of
      idExternal: begin  // external必须是最后一个
        SetFlag(Result, Directive);
        ParseExternal;
        if CurToken = tkSemicolon then NextToken;
        Break;
      end;
      idMessage: begin
        NextToken;
        Result.MsgNo := ParseIntExpr;
        SetFlag(Result, Directive);
      end;
      idDispMId: begin
        NextToken;
        Result.DispID := ParseIntExpr;
        SetFlag(Result, Directive);
      end;
      idNone: begin
        NextToken;
        ParseError('Invalid function directive: %s', [CurTokenString]);
      end;
    else
      SetFlag(Result, Directive);
      NextToken;
    end;

//    NextToken;
  end;
end;

procedure TParser.ParseFunctionHeader(Result: TFunctionHeader);

  procedure GetDottedNames(Func: TFunctionHeader);
  var
    I: Integer;
  begin
    I := 1;
    NextToken;// skip '.'
    repeat
      Expect(tkIdentifier);
      with Func do
        if I >= Length(Names) then SetLength(Names, Length(Names) + 10);

      Func.Names[I] := CurTokenString;
      Inc(I);
      NextToken;
      if CurToken <> tkDot then Break;
      NextToken;
    until False;

    Func.CountOfNames := I;
    with Func do Names[0] := Name;
  end;
var
  IsProc: Boolean;
begin
{
<FunctionDecl>  ::= <FuncHeading>  <CallBody> <OptSemi>
<FuncHeading>   ::= FUNCTION  <RefId> <OptFormalParms> ':' <ResultType> <OptCallSpecifiers> ';'
                  | FUNCTION  <RefId> ';'
                  | <FuncHeading> <CallDirectives> <OptSemi>

<CallBody>    ::= <OptDeclSection> <CompoundStmt>
                | <OptDeclSection> <AssemblerStmt>
                | <ExternalDeclaration>
                | FORWARD
}
  IsProc := CurToken <> tkFunction;
  if CurToken = tkConstructor then
    Result.MethodKind := mkConstructor
  else if CurToken = tkDestructor then
    Result.MethodKind := mkDestructor
  else
    Result.MethodKind := mkNormal;

  NextToken; // skip 'function' / 'procedure' / 'constructor' / 'destructor'

  Expect(tkIdentifier);
  Result.Name := CurTokenString;
  NextToken;
  if CurToken = tkDot then
    GetDottedNames(Result);

  if CurToken = tkBraceOpen then
  begin
    ParseArgumentList(nil, Result.Args);
  end
  else if CurToken = tkEqual then
  begin
    NextToken;
    Expect(tkIdentifier);
    Result.ImplementingName := CurTokenString;
    NextToken;
    Expect(tkSemicolon);
    NextToken;
    Exit;
  end;

  if not IsProc then
  begin
    // parse return type
    Expect(tkColon);
    if CurToken = tkColon then NextToken;
    Result.ReturnType := ParseTypeRef;
  end;

  ParseFunctionDirective(Result);
end;

function TParser.ParseGotoStmt: TGotoStmt;
var
  E: TSymbol;
begin
  Result := TGotoStmt(CreateStmt(TGotoStmt));
  NextToken; // skip 'goto';
  if CurToken in [tkIdentifier, tkIntConst] then
  begin
    E := FindSymbol(CurTokenString);
    if E = nil then
      ParseError(SErr_UndeclaredIdent)
    else if E.NodeKind <> nkLabel then
      ParseError(SErr_LabelExpected)
    else
      Result.StmtLabel := TStmtLabel(E);

    NextToken;
  //  Expect(tkSemicolon);
  end
  else
    Expect(tkIdentifier);
end;

function TParser.ParseHints: TMemberHints;
var
  S: string;
begin
  // Cur is tkIdentifier
  Result := [];

  repeat
    if CurToken = tkIdentifier then
    begin
      S := CurTokenString;
      if SameText(S, 'deprecated') then
        Include(Result, hDeprecated)
      else if SameText(S, 'platform') then
        Include(Result, hPlatform)
      else if SameText(S, 'experimental') then
        Include (Result, hExperimental)
      else if SameText(S, 'unimplemented') then
        Include(Result, hUnimplemented)
      else
        Break;
    end
    else if CurToken = tkLibrary then
      Include(Result, hLibrary)
    else
      Break;
    NextToken;
  until False;

end;

function TParser.ParseIdList(SymClass: TSymbolClass): TSymbol;
var
  E1, E2: TSymbol;
begin
  // cur token must be tkIdentifier
  Result := nil;
  E2 := nil;
  repeat
    Expect(tkIdentifier);
    E1 := TSymbol(CreateElement(SymClass));
    E1.Name := CurTokenString;

    if Result = nil then
      Result := E1;
    if E2 <> nil then
      E2.Next := E1;
    E2 := E1;

    NextToken;
    if CurToken <> tkComma then Break;
    NextToken;
  until False;
end;

function TParser.ParseIfStmt: TIfStmt;
begin
  NextToken; // skip if
  Result := TIfStmt(CreateStmt(TIfStmt));
  Result.Value := ParseExpr;
  CheckBoolExpr(Result.Value);

  Expect(tkThen);
  NextToken;

  Result.TrueStmt := ParseStatement(Result);
  if CurToken = tkElse then
  begin
    NextToken;
    Result.FalseStmt := ParseStatement(Result);
  end;
end;

procedure TParser.ParseImplementSection;

  procedure ParseStmtList(Stmts: TCompoundStmt);
  var
    Stmt: TStatement;
  begin
    while not (CurToken in [tkEnd, tkFinalization]) do
    begin
      Stmt := ParseStatement(Stmts);
      if Stmt <> nil then
        Stmts.Statements.Add(Stmt);
      if CurToken = tkSemicolon then NextToken;
    end;
  end;

  function ParseCodeList: TCompoundStmt;
  begin
    Result := TCompoundStmt(CreateStmt(TCompoundStmt));
    ParseStmtList(Result);
  end;

  function ParseEntryCode(const AName: string): TFunction;
  var
    StateInfo: TParseStateInfo;
  begin
    Result := TFunction(CreateElement(TFunction));
    Exclude(Result.Attr, saInternal);
    Result.Name := AName;
    Result.Parent := FModule;
    FCurFunction := Result;

    StateSet(psInFunc, StateInfo);
    Result.StartStmt := ParseCodeList;
    StateRestore(StateInfo);
    FCurFunction := nil;
  end;
begin
{
<ImplementationSection>	::= IMPLEMENTATION <OptUsesSection> <OptDeclSection> <OptExportBlock>
}
  FCurStates := [psInImplSect];
  FInternalSection := True;

  NextToken; // skip 'implementation'
  if CurToken = tkUses then
    ParseUsesClause;

  while True do
  begin
    case CurToken of
      tkType: Self.ParseTypeSection(FModule);
      tkVar, tkThreadVar: Self.ParseVarSection(FModule);
      tkConst: Self.ParseConstSection(FModule);
      tkResourceString: Self.ParseResStringSection(FModule);
      tkProcedure, tkFunction, tkConstructor, tkDestructor: Self.ParseFunction(FModule);

      tkClass:
        begin
          NextToken;
          if CurToken in [tkProcedure, tkFunction] then
          begin
            Include(Self.FCurStates, psInClassPrefix);
            Self.ParseFunction(FModule);
          end
          else
            ParseError(SErr_ExpectProcOrFunc, True);
        end;
      tkLabel: Self.ParseLabelSection(FModule);
    else
      Break;
    end;
  end;

  if CurToken = tkInitialization then
  begin
    NextToken;
    FModule.InitializeFunc := ParseEntryCode('$init');

    if CurToken = tkFinalization then
    begin
      NextToken;
      FModule.FinalizeFunc := ParseEntryCode('$final');
    end;
    Expect(tkEnd);
    NextToken;
  end
  else if CurToken = tkBegin then
  begin
    NextToken;
    FModule.InitializeFunc := ParseEntryCode('$init');
    Expect(tkEnd);
    NextToken;
  end
  else begin
    Expect(tkEnd);
    NextToken;
  end;
  Expect(tkDot);
end;

procedure TParser.ParseInterfaceSection;
begin
{
<InterfaceSection>	::= INTERFACE <OptUsesSection> <OptExportDeclList> 
}
  FCurStates := [psInIntfSect];

  NextToken; // skip 'interface'
  if CurToken = tkUses then
    ParseUsesClause;

  while True do
  begin
    case CurToken of
      tkType: Self.ParseTypeSection(FModule);
      tkVar: Self.ParseVarSection(FModule);
      tkConst: Self.ParseConstSection(FModule);
      tkResourceString: Self.ParseResStringSection(FModule);
      tkProcedure, tkFunction: Self.ParseFunction(FModule);
      tkLabel: begin
        ParseError('Label declaration not allowed in interface part');//Self.ParseLabelSection(FModule);
        Self.ParseLabelSection(FModule);
      end;
    else
      Break;
    end;
  end;
end;

function TParser.ParseInterfaceType(const IntfName: string;
    Parent: TSymbol; out NotAddSym: Boolean): TInterfaceType;

  procedure ParseGuidConst(var guid: TGuid);
  begin
    try
      guid := StringToGuid(ParseStrExpr('{00000000-0000-0000-C000-000000000046}'));
    except
      ParseError('Invalid guid string');
    end;
  end;

  function FindForwardIntf(const S: string): TInterfaceType;
  var
    E: TSymbol;
  begin
    E := CurSymbols.Find(S);
    if Assigned(E) and (E.ClassType = TInterfaceType) and (saForward in E.Attr) then
      Result := TInterfaceType(E)
    else
      Result := nil;
  end;

  procedure CheckIntf(Typ: TInterfaceType);
  var
    i, Def: Integer;
    Sym: TSymbol;
  begin
    Def := 0;
    for i := 0 to Typ.Symbols.Count - 1 do
    begin
      Sym := Typ.Symbols[i];
      if Sym.NodeKind = nkIntfProperty then
        if ipaDefaultProp in TIntfProperty(Sym).PropAttr then
          Inc(Def);
    end;
    if Def > 1 then
      ParseError(Typ.Coord, 'Only one default property can inside class,object,record,interface declaraction');
  end;
var
  Typ: TType;
  OldErr: Integer;
  MethSym: TSymbol;
  Prop: TIntfProperty;
  OldParent: TSymbol;
  State: TParseStateInfo;
begin
  Result := FindForwardIntf(IntfName);
  if Result <> nil then
  begin
    if Result.IsDisp <> (CurToken = tkDispInterface) then
      ParseError(SErr_RedeclaredIdent, [IntfName]);
    Exclude(Result.Attr, saForward);
    NotAddSym := True;
  end
  else
  begin
    Result := TInterfaceType(CreateElement(TInterfaceType));
    Result.Name := IntfName;
    Result.IsDisp := CurToken = tkDispInterface;
    NotAddSym := False;
  end;

  NextToken; // skip 'interface'

  OldErr := FErrorCount;

  if CurToken = tkSemicolon then
  begin   // forward
    Include(Result.Attr, saForward);
    Exit;
  end;

  if Result.IsDisp then
    StateSet(psInDispIntf, State)
  else
    StateSet(psInIntf, State);
    
  if CurToken = tkBraceOpen then
  begin
    // parse base
    NextToken;
    Typ := ParseTypeRef;
    Typ := Typ.OriginalType;
    if Typ.TypeCode <> typInterface then
      ParseError(SErr_InvalidBaseIntf)
    else
      Result.Base := TInterfaceType(Typ);
    Expect(tkBraceClose);
    NextToken;
  end;

  // todo 1: dispinterface 必须有guid,但interface不一定要guid
  // dispinterface基类是idispinterface
  if Result.Base = nil then
  begin
    if Result.IsDisp then
      Result.Base := FContext.FIDispatchType
    else
      Result.Base := FContext.FIUnknownType;
  end;

  if CurToken = tkSquaredBraceOpen then
  begin
    NextToken;
    ParseGuidConst(Result.Guid);
    Expect(tkSquaredBraceClose);
    NextToken;
  end;

  if not NotAddSym then
  begin
    AddSymbol(Result);
    NotAddSym := True;
  end;
  OldParent := FCurParent;
  FCurParent := Result;

  Self.EnterScope(Result.Symbols);
  while True do
  begin
    case CurToken of
      tkProcedure, tkFunction: begin
        MethSym := Self.ParseFunction(Result);
        if MethSym.NodeKind <> nkMethod then
          ParseError(MethSym.Coord, 'Method required');
        Exclude(TFunctionDecl(MethSym).Modifiers, fmForward);
        if Result.IsDisp then
          if not (fmDispID in TMethod(MethSym).Modifiers) then
            ParseError('DispID is required for method or property of dispinterface');
      end;
      tkProperty: begin
        Prop := ParseIntfProperty(Result);
        AddSymbol(Prop);

        if ipaDefaultProp in Prop.PropAttr then
          Result.DefaultProp := Prop;

        if Result.IsDisp then
          if not (ipaHasDispID in Prop.PropAttr) then
            ParseError('DispID is required for method or property of dispinterface');
      end;
      tkVar, tkConst, tkType: ParseError('var, const and type not allow in interface');
      tkEnd: Break;
    else
      Expect(tkEnd);
    end;
  end;
  Expect(tkEnd);
  NextToken;
  Result.Hints := ParseHints;

  FCurParent := OldParent;
  StateRestore(State);
  Self.LeaveScope;
  if OldErr = FErrorCount then
    CheckIntf(Result);
end;

function TParser.ParseIntExpr(const DefValue: Integer): Integer;
var
  E: TExpr;
begin
  E := ParseExpr;
  if CheckExpr(E) and (E.Typ.IsInteger or (E.Typ.TypeCode = typSubrange)) then
  begin
    ValClear(FTempValue);
    FTempValue := EvalExpr(E);
    Result := ValToInt(FTempValue);
    ValClear(FTempValue);
  end
  else begin
    ParseError('Integer constant expression expected');
    Result := DefValue;
  end;
end;

function TParser.ParseIntfProperty(Parent: TType): TIntfProperty;

  function ParseAccessor: TMethod;
  var
    E: TSymbol;
  begin
    Expect(tkIdentifier);
    E := CurSymbols.Find(CurTokenString);
    if (E.NodeKind = nkMethod) and (E.Parent = Parent) then
      Result := TMethod(E)
    else  begin
      Result := nil;
      ParseError('Invalid property accessor');
    end;
    NextToken;
  end;

  function ExpectParentType(typ: TTypeCode): Boolean;
  begin
    Result := typ = Parent.TypeCode;
    if not Result then
      ParseError('property directive not allow in here');
  end;

  function IsSameType(t1, t2: TType): Boolean;
  begin
    t1 := t1.OriginalType;
    t2 := t2.OriginalType;
    if t1.TypeCode = typSubrange then t1 := TSubrangeType(t1).BaseType;
    if t2.TypeCode = typSubrange then t2 := TSubrangeType(t2).BaseType;

    Result := t1 = t2;
  end;

  procedure CheckSetterArgs(Prop: TIntfProperty; MethodArgs: TList);
  var
    I, ExpectArgCount: Integer;
    A1, A2: TArgument;
  begin
    ExpectArgCount := Prop.CountOfArgs + 1;

    if (MethodArgs = nil) or (MethodArgs.Count <> ExpectArgCount) then
      ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched)
    else begin
      { Setter:
        procedure SetValue(i: Integer; value: Double);
      }
      for I := 0 to Prop.CountOfArgs - 1 do
      begin
        A1 := TArgument(Prop.Args[I]);
        A2 := TArgument(MethodArgs[I]);
        if not IsSameType(A1.ArgType, A2.ArgType)
            or (A1.Modifier <> A2.Modifier) then
        begin
          ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
          Exit;
        end;
      end;
      A1 := TArgument(MethodArgs[MethodArgs.Count - 1]);
      if not IsSameType(Prop.PropType, A1.ArgType) or (A1.Modifier in [argVar, argOut]) then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
    end;
  end;

  procedure CheckSetter(Prop: TIntfProperty);
  begin
    if Prop.Setter = nil then Exit;

    if Prop.Setter.ReturnType <> nil then
      ParseError(Prop.Coord, 'Setter method not allow return value')
    else
      CheckSetterArgs(Prop, TMethod(Prop.Setter).Args);
  end;

  procedure CheckGetterArgs(Prop: TIntfProperty; MethodArgs: TList);
  var
    I, ExpectArgCount: Integer;
    A1, A2: TArgument;
  begin
    ExpectArgCount := Prop.CountOfArgs;

    if MethodArgs = nil then begin
      if ExpectArgCount <> 0 then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched)
    end
    else
      for I := 0 to Prop.CountOfArgs - 1 do
      begin
        A1 := TArgument(Prop.Args[I]);
        A2 := TArgument(MethodArgs[I]);
        if not IsSameType(A1.ArgType, A2.ArgType)
            or (A1.Modifier <> A2.Modifier) then
        begin
          ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
          Exit;
        end;
      end;
  end;

  procedure CheckGetter(Prop: TIntfProperty);
  begin
    if Prop.Getter = nil then Exit;

    if not IsSameType(Prop.PropType, Prop.Getter.ReturnType) then
      ParseError(Prop.Coord, 'Accessor type not matched')
    else
      CheckGetterArgs(Prop, Prop.Getter.Args);
  end;

  procedure CheckProp(Prop: TIntfProperty);
  var
    OldErr: Integer;
  begin
    if (ipaDefaultProp in Prop.PropAttr) and (Prop.CountOfArgs = 0) then
      ParseError(Prop.Coord, 'Non-array property not allow default directive')
    else begin
      OldErr := FErrorCount;
      CheckGetter(Prop);
      if OldErr = FErrorCount then
        CheckSetter(Prop);
    end;
  end;
var
  PropD: TPropDirective;
  OldErr: Integer;
begin
{
<PropertySpec>		::= PROPERTY <PropertyDecl> <OptPropSpecifiers> ';'

<PropertyDecl>		::= <RefId>                     ':' <TypeRef>
			  | <RefId> '[' <IndexList> ']' ':' <TypeRef>
			  | <RefId>
<PropertySpecifiers>	::= <PropertySpecifier>
			  | <PropertySpecifiers>  <PropertySpecifier>

<PropertySpecifier>	::= READ       <FieldDesignator>
			  | WRITE      <FieldDesignator>
			  | WRITEONLY
			  | READONLY
			  | DISPID <ConstIntExpr>        !Only within InterfaceTypes
}
  NextToken;
  Expect(tkIdentifier);
  Result := TIntfProperty(CreateElement(TIntfProperty));
  Result.Name := CurTokenString;
  NextToken;

  OldErr := FErrorCount;
  if CurToken = tkSquaredBraceOpen then
  begin
    NextToken;
    // parse array property args
    Result.CreateArgs;
    Self.ParseArgumentList(Result, Result.Args);
    Expect(tkColon);
  end;

  if CurToken = tkColon then
  begin
    NextToken;
    Result.PropType := ParseTypeRef;
  end;

  while CurToken = tkIdentifier do
  begin
    PropD := ParsePropDirective(CurTokenString);
    NextToken;
    case PropD of
      idRead:
        if ExpectParentType(typInterface) then
          Result.Getter := ParseAccessor;
      idWrite:
        if ExpectParentType(typInterface) then
          Result.Setter := ParseAccessor;
      idDispID:
        if ExpectParentType(typDispInterface) then
        begin
          Result.DispID := ParseIntExpr;
          Include(Result.PropAttr, ipaHasDispID);
        end;
      idReadOnly:
        if ExpectParentType(typDispInterface) then
          Include(Result.PropAttr, ipaReadOnly);
      idWriteOnly:
        if ExpectParentType(typDispInterface) then
          Include(Result.PropAttr, ipaWriteOnly);
      else
        ParseError('Invalid property directive: %s', [CurTokenString]);
    end;
  end;
  Expect(tkSemicolon);
  NextToken;
  if CurToken = tkIdentifier{Result.ArgCount > 0} then
  begin
    // 只有Array property才需要default指令
    if SameText(CurTokenString, 'default') then
      Include(Result.PropAttr, ipaDefaultProp);
    NextToken;
    Expect(tkSemicolon);
    NextToken;
  end;
  if OldErr = FErrorCount then
    CheckProp(Result);
end;

procedure TParser.ParseLabelSection(Parent: TSymbol);
var
  Lab: TStmtLabel;
begin
  NextToken; // skip 'label'
  Lab := TStmtLabel(ParseIdList(TStmtLabel));
  while Lab <> nil do
  begin
    AddSymbol(Lab);
    Lab := TStmtLabel(Lab.Next);
  end;
  Expect(tkSemicolon);
  NextToken;
end;

function TParser.ParseMulExpr: TExpr;

  function MulOp(T: TToken): TExprOpCode;
  begin
    case T of
      tkMul        : Result := opMUL;
      tkFDiv       : Result := opFDIV;
      tkMod        : Result := opMOD;
      tkDiv        : Result := opIDIV;
      tkAnd        : Result := opAND;
      tkSHR        : Result := opSHR;
      tkSHL        : Result := opSHL;
    else
      Result := opNONE;
    end;
  end;
var
  left, right: TExpr;
  op: TExprOpCode;
begin
{
<MulExpr>		::= <Factor>
			  | <MulExpr> <MulOp> <Factor>
}
  Result := ParseFactor;
  op := MulOp(CurToken);
  while op <> opNONE do
  begin
    NextToken;
    left := Result;
    right := ParseFactor;
    Result := CreateBinaryExpr(op, left, right);
    op := MulOp(CurToken);
  end;
end;

function TParser.ParseObjectType(const ObjName: string): TObjectType;
// todo 1: 要完善
  procedure CheckObject(typ: TObjectType);
  var
    i: Integer;
    sym: TSymbol;
  begin
    for i := 0 to typ.Symbols.Count - 1 do
    begin
      sym := typ.Symbols[i];
      case sym.NodeKind of
        nkMethod:
          // 检查是否override;
          if fmOverride in TMethod(Sym).Modifiers then
            Include(TMethod(Sym).Modifiers, fmVirtual);
      end;
    end;
  end;
var
  Base: TSymbol;
  Field: TField;
  MethSym: TSymbol;
  Prop: TProperty;
  OldErr: Integer;
  StateInfo: TParseStateInfo;
  OldParent: TSymbol;
  OldVis: TMemberVisibility;
  ClassPrefix, ClassVar: Boolean;
begin
  Result := TObjectType(CreateElement(TObjectType));
  StateSet(psInObject, StateInfo);

  NextToken; // skip 'object'

  if CurToken = tkBraceOpen then
  begin
    // parse base
    NextToken;
    Base := ParseQualifiedSym;
    if (Base.NodeKind <> nkType) or (TType(Base).TypeCode <> typObject) then
      ParseError(SErr_InvalidBaseObject)
    else
      Result.Base := TObjectType(Base);
    Expect(tkBraceClose);
    NextToken;
  end;

  Result.Name := ObjName;
  AddSymbol(Result);

  Result.GlobalAlignSize := FAlignSize;
  if psInPacked in FCurStates then
    Result.GlobalAlignSize := 1;

  OldErr := FErrorCount;
  OldVis := FCurVisibility;
  FCurVisibility := visPublic;
  ClassPrefix := False;
  ClassVar := False;

  EnterScope(Result.Symbols);
  OldParent := FCurParent;
  FCurParent := Result;
  while True do
    case CurToken of
      tkPrivate: begin
        FCurVisibility := visPrivate;
        NextToken;
      end;
      tkProtected: begin
        FCurVisibility := visProtected;
        NextToken;
      end;
      tkPublic: begin
        FCurVisibility := visPublic;
        NextToken;
      end;
      tkPublished: begin
        FCurVisibility := visPublished;
        NextToken;
      end;
      tkStrict: begin
        NextToken;
        if CurToken = tkPrivate then
          FCurVisibility := visStrictPrivate
        else if CurToken = tkProtected then
          FCurVisibility := visStrictProtected
        else
          ParseError(SErr_ExpectProtectOrPrivate, True);
        NextToken;
      end;

      tkIdentifier: begin
        Field := ParseField(TField);
        while Field <> nil do
        begin
          Field.Visibility := FCurVisibility;
          if ClassVar then
            Include(Field.Attr, saStatic);
          AddSymbol(Field);
          Field := TField(Field.Next);
        end;
        Expect(tkSemicolon);
        NextToken;
      end;

      tkVar: begin
        NextToken;
        ClassVar := False;
      end;

      tkConst: Self.ParseConstSection(Result);

      tkType: Self.ParseTypeSection(Result);

      tkClass: begin
        ClassPrefix := True;
        NextToken;
        if CurToken = tkVar then
        begin
          ClassVar := True;
          NextToken;
        end
        else if not (CurToken in [tkFunction, tkProcedure, tkProperty]) then
          ParseError(SErr_ExpectMethodOrProperty, True);
      end;

      tkFunction, tkProcedure: begin
        MethSym := ParseFunction(Result);
        MethSym.Visibility := FCurVisibility;
        if MethSym.NodeKind <> nkMethod then
          ParseError(MethSym.Coord, 'Method required');

        if ClassPrefix then
          Include(MethSym.Attr, saClass);
        if fmStatic in TMethod(MethSym).Modifiers then
          Include(MethSym.Attr, saStatic);
        ClassPrefix := False;
      end;

      tkProperty: begin
        Prop := ParseProperty(Result, ClassPrefix);
        Prop.Visibility := FCurVisibility;
        AddSymbol(Prop);
        if ClassPrefix then
          Include(Prop.Attr, saStatic);
      end;

      tkEnd: Break;
    else
      Expect(tkIdentifier);
    end;

  Expect(tkEnd);
  NextToken;
  Result.Hints := ParseHints;

  StateRestore(StateInfo);
  LeaveScope;

  FCurParent := OldParent;
  FCurVisibility := OldVis;
  // 检查Symbols
  if FErrorCount = OldErr then
    CheckObject(Result);
  if FErrorCount = OldErr then
    Result.Update(Self.FPointerSize);
end;

function TParser.ParseProgram: TModule;

  procedure SkipArgs;
  begin
    NextToken; // skip '('
    while True do
    begin
      Expect(tkIdentifier, True);
      NextToken;
      if CurToken <> tkComma then Break;

      NextToken;
    end;
    NextToken; // skip ')'
    Expect(tkSemicolon);
  end;

  procedure CleanupSym;
  var
    st: TSymbolTable;
  begin
    st := FModule.InternalSymbols;
    FModule.InternalSymbols := FModule.Symbols;
    FModule.Symbols := st;
  end;
begin
{
<ProgHeader>		::= PROGRAM <RefId> <OptProgParamList> ';'
<OptProgParamList>	::= '(' <IdList> ')' 
}
  NextToken; // skip 'program'
  Expect(tkIdentifier, True);

  FModule := TModule(CreateElement(TModule));
  FModule.Name := CurTokenString;
  FModule.Kind := mkProgram;
  FModule.TimeStamp := FScanner.TimeStamp;

  NextToken;
  if CurToken = tkBraceOpen then
    SkipArgs
  else
    Expect(tkSemicolon, True);

  NextToken; // skip ';'

  EnterScope(FModule.Symbols);
  FContext.LoadSystemUnit;
  FModule.Symbols.AutoAddToOwner := False;
  AddSymbols(FContext.FSystemUnit);
  FModule.Symbols.AutoAddToOwner := True;
  FModule.LoadedUnits.Add(FContext.FSystemUnit);

  FCurParent := FModule;
  SetTempExpr(True);

  FInternalSection := True;
  FCurStates := [psInImplSect];
  if CurToken = tkUses then
    ParseUsesClause;

  ParseBlock(FModule);

  FCurParent := nil;
  LeaveScope;
  FCurStates := [];

  // 检查全部的声明是否都有实现
  CheckForward;

  CleanupSym;
  Result := FModule;
end;

function TParser.ParseProperty(Parent: TType; IsStatic: Boolean): TProperty;

  // 检查Expr的有效性, 如果非法,返回false,错误信息通过ParseError发出
  // 返回返回true
  function IsMemberExpr(E: TExpr): Boolean;
  var
    Elem: TSymbol;
  begin
    Result := False;

    if E.OpCode = opSYMBOL then
    begin
      Elem := TSymbolExpr(E).Reference;
      if (Elem.NodeKind in [nkField, nkMethod]) then
      begin
        if Elem.Parent = Parent then
          Result := True
        else
          ParseError('%s must be a member of %s', [Elem.Name, Parent.Name]);
      end
      else
        ParseError('%s must be a field or method of %s', [Elem.Name, Parent.Name]);
      Exit;
    end;

    // 可以是record 中的字段, 但不可通过指针来访问
    while E <> nil do
    begin
      if E.OpCode = opMEMBER then
      begin
        if not (TBinaryExpr(E).Left.Typ.TypeCode in [typRecord, typObject]) then begin
          ParseError('Only record or object type allow in accessor expression');
          Exit;
        end else if TSymbolExpr(TBinaryExpr(E).Right).Reference.NodeKind <> nkField then begin
          ParseError('Only field allow in accessor expression');
          Exit;
        end else
          E := TBinaryExpr(E).Left;
      end
      else if E.OpCode = opSYMBOL then
        Break
      else
        Exit; // invalid operator
    end;

    Elem := TSymbolExpr(E).Reference;
    if Elem.NodeKind <> nkField then
      ParseError('%s must be a field of %s', [Elem.Name, Parent.Name])
    else if Elem.Parent <> Parent then
      ParseError('%s must be a member of %s', [Elem.Name, Parent.Name])
    else
      Result := True;
  end;

  function GetAccessor(E: TExpr): TMultiAccessor;
  var
    R: TSymbolExpr;
  begin
    while E <> nil do
    begin
      if E.OpCode = opMEMBER then
        E := TBinaryExpr(E).Left
      else
        Break;
    end;

    Result := TMultiAccessor(CreateElement(TMultiAccessor));
    Result.Add(TField(TSymbolExpr(E).Reference));

    E := E.Parent;
    while E <> nil do
    begin
      R := TSymbolExpr(TBinaryExpr(E).Right);
      Result.Add(R.Reference);
      E := E.Parent;
    end;
  end;

  function ParseAccessor: TSymbol;
  var
    E: TExpr;
  begin
    // CurToken is tkIdentifier
    E := CreateSymbolExpr(CurTokenString);
    NextToken;
    while CurToken = tkDot do
    begin
      NextToken;
      Expect(tkIdentifier);
      E := CreateBinaryExpr(opMEMBER, E);
      TBinaryExpr(E).Right := CreateSymbolExpr(CurTokenString);
      NextToken;
    end;

    Include(FCurStates, psInAccessor);
    Result := nil;
    if CheckExpr(E) then
      if IsMemberExpr(E) then
      begin
        if E.OpCode = opSYMBOL then
          Result := TSymbolExpr(E).Reference
        else
          Result := GetAccessor(E);
      end;

    Exclude(FCurStates, psInAccessor);
    ClearTempExprList;
  end;

  function ParseStoredProc: TSymbol;
  var
    E: TExpr;
  begin
    Result := nil;
    E := ParseExpr;
    if CheckExpr(E) then
    begin
      if not E.Typ.IsBoolean then
        ParseError('bool expression expected')
      else if IsMemberExpr(E) then
        Result := GetAccessor(E)
      else begin
        ValClear(FTempValue);
        FTempValue := EvalExpr(E);
        if ValToBool(FTempValue) then
          Result := FContext.FTrueConst
        else
          Result := FContext.FFalseConst;
        ValClear(FTempValue);
      end;
    end;
    ClearTempExprList;
  end;

  function IsSameType(t1, t2: TType): Boolean;
  begin
    t1 := t1.OriginalType;
    t2 := t2.OriginalType;
    if t1.TypeCode = typSubrange then t1 := TSubrangeType(t1).BaseType;
    if t2.TypeCode = typSubrange then t2 := TSubrangeType(t2).BaseType;

    Result := t1 = t2;
  end;

  function CheckPropStatic(Prop: TProperty; Elem: TSymbol): Boolean;
  begin
    if saStatic in Prop.Attr then
      Result := saStatic in Elem.Attr
    else
      Result := not (saStatic in Elem.Attr);

    if Result then Exit;

    if saStatic in Prop.Attr then
      ParseError(Prop.Coord, 'Static property not allow non-static accessor')
    else
      ParseError(Prop.Coord, 'Non-static property not allow static accessor');
  end;

  procedure CheckGetterArgs(Prop: TProperty; MethodArgs: TList);
  var
    I, ExpectArgCount: Integer;
    A1, A2: TArgument;
  begin
    if Prop.HasIndexSpec then
      ExpectArgCount := 1
    else
      ExpectArgCount := Prop.CountOfArgs;

    if MethodArgs = nil then begin
      if ExpectArgCount <> 0 then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched)
    end
    else if Prop.HasIndexSpec then
    begin
      A1 := TArgument(MethodArgs[0]);
      if (A1.ArgType.TypeCode = typLongint) and (A1.Modifier in [argVar, argOut]) then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
    end
    else
      for I := 0 to Prop.CountOfArgs - 1 do
      begin
        A1 := TArgument(Prop.Args[I]);
        A2 := TArgument(MethodArgs[I]);
        if not IsSameType(A1.ArgType, A2.ArgType)
            or (A1.Modifier <> A2.Modifier) then
        begin
          ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
          Exit;
        end;
      end;
  end;

  procedure CheckGetter(Prop: TProperty);
  var
    OldErr: Integer;
  begin
    if Prop.Getter = nil then Exit;

    OldErr := FErrorCount;
    case Prop.Getter.NodeKind of
      nkField: begin
        if not IsSameType(Prop.PropType, TField(Prop.Getter).FieldType) then
          ParseError(Prop.Coord, 'Accessor type not matched')
        else
          CheckPropStatic(Prop, TField(Prop.Getter));
      end;
      nkMethod: begin
        if not IsSameType(Prop.PropType, TMethod(Prop.Getter).ReturnType) then
          ParseError(Prop.Coord, 'Accessor type not matched')
        else
          CheckPropStatic(Prop, TMethod(Prop.Getter));
        if OldErr = FErrorCount then
          CheckGetterArgs(Prop, TMethod(Prop.Getter).Args);
      end;
      else begin
        if not IsSameType(Prop.PropType, TMultiAccessor(Prop.Getter).Last.FieldType) then
          ParseError(Prop.Coord, 'Accessor type not matched')
        else
          CheckPropStatic(Prop, TMultiAccessor(Prop.Getter).First);
      end;
    end;
  end;

  procedure CheckSetterArgs(Prop: TProperty; MethodArgs: TList);
  var
    I, ExpectArgCount: Integer;
    A1, A2: TArgument;
  begin
    if Prop.HasIndexSpec then
      ExpectArgCount := 2
    else
      ExpectArgCount := Prop.CountOfArgs + 1;

    if MethodArgs = nil then begin
      if ExpectArgCount <> 0 then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched)
    end
    else if Prop.HasIndexSpec then
    begin
      A1 := TArgument(MethodArgs[0]);
      A2 := TArgument(MethodArgs[1]);
      if (A1.ArgType.TypeCode <> typLongint) or (A1.Modifier in [argVar, argOut]) then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched)
      else if not IsSameType(A2.ArgType, Prop.PropType) or (A2.Modifier in [argVar, argOut]) then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
    end
    else begin
      { Setter:
        procedure SetValue(i: Integer; value: Double);
      }
      for I := 0 to Prop.CountOfArgs - 1 do
      begin
        A1 := TArgument(Prop.Args[I]);
        A2 := TArgument(MethodArgs[I]);
        if not IsSameType(A1.ArgType, A2.ArgType)
            or (A1.Modifier <> A2.Modifier) then
        begin
          ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
          Exit;
        end;
      end;
      A1 := TArgument(MethodArgs[MethodArgs.Count - 1]);
      if not IsSameType(Prop.PropType, A1.ArgType) or (A1.Modifier in [argVar, argOut]) then
        ParseError(Prop.Coord, SErr_AccessorMethodArgsNotMatched);
    end;
  end;

  procedure CheckSetter(Prop: TProperty);
  var
    OldErr: Integer;
  begin
    if Prop.Setter = nil then Exit;

    OldErr := FErrorCount;
    case Prop.Setter.NodeKind of
      nkField: begin
        if not IsSameType(Prop.PropType, TField(Prop.Setter).FieldType) then
          ParseError(Prop.Coord, 'Accessor type not matched')
        else
          CheckPropStatic(Prop, TField(Prop.Setter));
      end;
      nkMethod: begin
        if TMethod(Prop.Setter).ReturnType <> nil then
          ParseError(Prop.Coord, 'Setter method not allow return value')
        else
          CheckPropStatic(Prop, TMethod(Prop.Setter));
        if OldErr = FErrorCount then
          CheckSetterArgs(Prop, TMethod(Prop.Setter).Args);
      end;
      else begin
        if not IsSameType(Prop.PropType, TMultiAccessor(Prop.Setter).Last.FieldType) then
          ParseError(Prop.Coord, 'Accessor type not matched')
        else
          CheckPropStatic(Prop, TMultiAccessor(Prop.Setter).First);
      end;
    end;
  end;

  procedure CheckStored(Prop: TProperty);
  var
    Meth: TMethod;
  begin
    if Prop.Stored = nil then Exit;

    case Prop.Stored.NodeKind of
      nkMethod: begin
        Meth := TMethod(Prop.Stored);
        if not (Meth.CountOfArgs = 1)
            and (TArgument(Meth.Args[0]).ArgType.TypeCode = typLongint) then
          ParseError(Prop.Coord, 'Accessor arguments not matched');
      end;
    end;
  end;

  procedure CheckProp(Prop: TProperty);
  var
    OldErr: Integer;
  begin
    // 1.检查类型
    // 2.检查参数
    // 3.有index,Accessor不能是字段,属性也不能是Array
    // 4.class property的Accessor必须是class var或class method
    // 5.Getter 和 setter必须有一个不为nil
    // 6.Array property才能有default

    if Prop.HasIndexSpec and (Prop.CountOfArgs > 0) then
      ParseError(Prop.Coord, 'Array property not allow index directive')
    else if (saStatic in Prop.Attr) and (Prop.Stored <> nil) then
      ParseError(Prop.Coord, 'Class property not allow stored directive')
    else if (Prop.Getter = nil) and (Prop.Setter = nil) then
      ParseError(Prop.Coord, 'Getter and Setter not allow be also nil')
    else if (paDefaultProp in Prop.PropAttr) and (Prop.CountOfArgs = 0) then
      ParseError(Prop.Coord, 'Non-array property not allow default directive') 
    else begin
      OldErr := FErrorCount;
      CheckGetter(Prop);
      if OldErr = FErrorCount then
        CheckSetter(Prop);
      if OldErr = FErrorCount then
        CheckStored(Prop);
    end;
  end;

  procedure FindBaseDecl(Prop: TProperty);
  var
    Elem: TSymbol;
    BaseProp: TProperty;
  begin
    case Parent.TypeCode of
      typClass: Elem := TClassType(Parent).FindBaseSymbol(Prop.Name);
      typObject: Elem := TObjectType(Parent).FindBaseSymbol(Prop.Name);
    else
      Elem := nil;
    end;

    if (Elem = nil) or (Elem.NodeKind <> nkProperty) then
      ParseError(Prop.Coord, 'Property %s not exists in base', [Prop.Name])
    else begin
      BaseProp := TProperty(Elem);
      if (saStatic in BaseProp.Attr) xor (saStatic in Prop.Attr) then
      begin
        ParseError(Prop.Coord, 'Property %s different from base', [Prop.Name]);
        Exit;
      end;
      if Prop.PropType = nil then Prop.PropType := BaseProp.PropType;
      if Prop.Getter = nil then Prop.Getter := BaseProp.Getter;
      if Prop.Setter = nil then Prop.Setter := BaseProp.Setter;
      if Prop.Stored = nil then Prop.Stored := BaseProp.Stored;
      if not Prop.HasIndexSpec then Prop.Index := BaseProp.Index;
      if ValIsClear(Prop.DefaultValue) and not ValIsClear(BaseProp.DefaultValue) then
        ValCopy(Prop.DefaultValue, BaseProp.DefaultValue);
    end;
  end;
var
  FullDecl: Boolean;
  PropD: TPropDirective;
  OldErr: Integer;
begin
{
<PropertySpec>		::= PROPERTY <PropertyDecl> <OptPropSpecifiers> ';'

<PropertyDecl>		::= <RefId>                     ':' <TypeRef>
			  | <RefId> '[' <IndexList> ']' ':' <TypeRef>
			  | <RefId>
}
  NextToken;
  Expect(tkIdentifier);
  Result := TProperty(CreateElement(TProperty));
  Result.Name := CurTokenString;
  NextToken;

  FullDecl := False;
  if CurToken = tkSquaredBraceOpen then
  begin
//    NextToken;
    // parse array property args
    Result.CreateArgs;
    Self.ParseArgumentList(Result, Result.Args);
    Expect(tkColon);
    FullDecl := True;
  end;

  if CurToken = tkColon then
  begin
    NextToken;
    Result.PropType := ParseTypeRef;
    FullDecl := True;
  end;

  OldErr := Self.ErrorCount;
  while CurToken = tkIdentifier do
  begin
  // todo 5: 此处可以优化。在Scanner中增加一个把Read等视为关键字的选项
    PropD := ParsePropDirective(CurTokenString);
    NextToken;
    case PropD of
      idRead: Result.Getter := ParseAccessor;
      idWrite: Result.Setter := ParseAccessor;
      idIndex: Result.Index := ParseIntExpr;
      idStored: Result.Stored := ParseStoredProc;
      idNoDefault: Include(Result.PropAttr, paNoDefault);

    else
      ParseError('Invalid property directive: %s', [CurTokenString]);
    end;
  end;
  Expect(tkSemicolon);
  NextToken;
  if CurToken = tkIdentifier{Result.ArgCount > 0} then  // 只有Array property才需要default指令
    if SameText(CurTokenString, 'default') then
    begin
      Include(Result.PropAttr, paDefaultProp);
      NextToken;
      Expect(tkSemicolon);
      NextToken;
    end;

  if IsStatic then
  begin
    Include(Result.Attr, saStatic);
    Include(Result.Attr, saClass);
  end;

  if not FullDecl then   // 不是完整声明,需要查找基类声明来完善
    FindBaseDecl(Result);
  // 如果上面已经有错误发生,则不需检查
  if OldErr = Self.ErrorCount then
    CheckProp(Result);
end;

procedure TParser.ParseQualifiedId(const First: string);
begin
  if First = '' then
  begin
    Expect(tkIdentifier);
    FQId.Name := CurTokenString;
    NextToken;
  end
  else
    FQId.Name := First;
  FQId.CountOfNames := 1;
  while CurToken = tkDot do  // System.SysUtils.Id 这种情况
  begin
    // 允许保留字出现在tkDot(.)之后
    // 如 MyUnit.Type
    Scanner.NoReservedWord := True;
    NextToken;
    Scanner.NoReservedWord := False;
    Expect(tkIdentifier);
    if FQId.CountOfNames > Length(FQId.Names) then
      SetLength(FQId.Names, Length(FQId.Names) + 10);
    FQId.Names[FQId.CountOfNames] := CurTokenString;
    NextToken;
    Inc(FQId.CountOfNames);
  end;
  FQId.Names[0] := FQId.Name;
end;

function TParser.ParseQualifiedSym(const First: string): TSymbol;
var
  Sym: TSymbol;
  I: Integer;
begin
  ParseQualifiedID(First);
  if FQId.CountOfNames = 1 then
    Result := FindSymbol(FQId.Name)
  else
  begin
    Sym := FindSymbol(FQId.Name);
    for I := 1 to FQID.CountOfNames - 1 do
    begin
      if not Assigned(Sym) then Break;

      case Sym.NodeKind of
        nkModule: Sym := TModule(Sym).FindSymbol(FQId.Names[I]);
        nkNameScope: Sym := TNameScope(Sym).FindSymbol(FQId.Names[I]);
        nkType:
          case TType(Sym).TypeCode of
            typClass: Sym := TClassType(Sym).FindSymbol(FQId.Names[I]);
            typRecord: Sym := TRecordType(Sym).FindSymbol(FQId.Names[I]);
            typObject: Sym := TObjectType(Sym).FindSymbol(FQId.Names[I]);
          else
            Sym := nil;
          end;
      else
        Sym := nil;
      end;
      if Sym = nil then Break;

      if not IsVisible(FCurParent, Sym) then
        ParseError(SErr_SymbolNotAccess, [Sym.Name]);
    end;

    Result := Sym;
  end;
end;

function TParser.ParseRaiseStmt: TRaiseStmt;
begin
  Result := TRaiseStmt(CreateStmt(TRaiseStmt));
  NextToken; // skip 'raise'
  Result.Expr := Self.ParseDesignator;
  if CheckExpr(Result.Expr) then
    if Result.Expr.Typ.TypeCode <> typClass then
      ParseError(Result.Expr.Coord, SErr_ClassRequired);
end;

function TParser.ParseRecordType(const TypName: string; Parent: TSymbol): TRecordType;

  procedure CheckSelector(E: TExpr; T: TType);
  begin
  end;

  function ParseRecordBody(BodyClass: TRecordBodyClass): TRecordBody;
  var
    Field: TField;
    B1, B2: TRecordVariant;
    E: TExpr;
  begin
    Result := BodyClass.Create;
    while (CurToken <> tkEnd) and (CurToken <> tkBraceClose) do
    begin
      if CurToken = tkCase then
      begin
        NextToken; // skip 'case'
        Expect(tkIdentifier);
        FTemp := CurTokenString;
        Result.Selector := TField(CreateElement(TField));
        NextToken;
        if CurToken = tkColon then
        begin
          // todo: 2 这部分还未测试
          // 上一个是Selector Field Name
          Result.Selector.Name := FTemp;
          FTemp := '';
          NextToken;
        end
        else
          UngetToken;
        Result.Selector.FieldType := ParseTypeRef;
        Expect(tkOf);
        NextToken;
        if Result.Selector.Name <> '' then AddSymbol(Result.Selector);

        B1 := nil;
        repeat
          E := ParseExprList;
          // todo 5: 需要检查E类型兼容
          CheckSelector(E, Result.Selector.FieldType);
          Expect(tkColon);
          NextToken;
          Expect(tkBraceOpen);
          NextToken;

          B2 := TRecordVariant(ParseRecordBody(TRecordVariant));
          if Result.Variants = nil then
            Result.Variants := B2;
          if B1 <> nil then B1.Next := B2;
          B1 := B2;
          Expect(tkBraceClose);
          NextToken;

          if CurToken = tkSemiColon then
            NextToken
          else
            Break;
          if (CurToken = tkEnd) or (CurToken = tkBraceClose) then Break;
        until False;

        ClearTempExprList;
      end
      else begin
        Field := TField(ParseField(TField));
        while Field <> nil do
        begin
          Field.Visibility := visPublic;
          AddSymbol(Field);
          if Result.ClassType = TRecordVariant then
            Include(Field.FieldAttr, faRecVar);
          Result.Members.Add(Field);
          Field := TField(Field.Next);
        end;
        if CurToken = tkSemicolon then NextToken;
      end;
    end;
  end;
var
  OldParent: TSymbol;
begin
  NextToken; // skip 'record'

  Result := TRecordType(CreateElement(TRecordType));
  Result.Name := TypName;
  if Result.Name <> '' then
  begin
    AddSymbol(Result);
  end;

  EnterScope(Result.Symbols);
  OldParent := FCurParent;
  FCurParent := Parent;

  Result.Body := ParseRecordBody(TRecordBody);
  Expect(tkEnd);
  NextToken;
  if not (psInVar in Self.FCurStates) then
    Result.Hints := ParseHints;
  FCurParent := OldParent;
  LeaveScope;

  Result.GlobalAlignSize := FAlignSize;
  if psInPacked in FCurStates then
    Result.GlobalAlignSize := 1;
  Result.Update;
{  if Result.Size > $7fffffff then
    ParseError(Result.Coord, 'Type size may be exceed than 2GB');}
end;

function TParser.ParseRepeatStmt: TRepeatStmt;
var
  StateInfo: TParseStateInfo;
begin
  NextToken; // skip 'repeat'
  StateSet(psInWhileStmt, StateInfo);
  Result := TRepeatStmt(CreateStmt(TRepeatStmt));
  Result.Stmt := ParseStmtList(Result, [tkUntil]);
  StateRestore(StateInfo);

  Expect(tkUntil);
  NextToken;

  Result.Condition := ParseExpr;
  CheckBoolExpr(Result.Condition);
//  Expect(tkDo);
//  NextToken;
//  Result.Stmt := ParseStatement(Result);
end;

procedure TParser.ParseResStringSection(Parent: TSymbol);
var
  V: TConstant;
begin
  NextToken; // skip 'resourcestring'

  Expect(tkIdentifier);
  repeat
    V := TConstant(CreateElement(TConstant));
    V.Name := CurTokenString;
    NextToken;

    Expect(tkEqual);
    NextToken;

    V.Value := ValFromStr(ParseStrExpr('1'));
    V.Visibility := FCurVisibility;
    V.IsResStr := True;
    Expect(tkSemicolon);
    AddSymbol(V);
    NextToken;
  until CurToken <> tkIdentifier;
  ClearTempExprList;
end;

function TParser.ParseSetConstructor: TExpr;
begin
  Expect(tkSquaredBraceOpen);
  Result := CreateUnaryExpr(opSET);
  NextToken;
  if CurToken <> tkSquaredBraceClose then
    TUnaryExpr(Result).Operand := ParseSetElementList;
  Expect(tkSquaredBraceClose);
  NextToken;
end;

function TParser.ParseSetElementList: TExpr;

  function ParseSetElement: TExpr;
  var
    L: TExpr;
  begin
    Result := ParseExpr;
    if CurToken = tkDotDot then
    begin
      L := Result;
      Result := CreateBinaryExpr(opRANGE);
      TBinaryExpr(Result).Left := L;
      NextToken;
      TBinaryExpr(Result).Right := ParseExpr;
    end;
  end;

var
  L, L2: TExpr;
begin
// 读取SET表达式列表, 数目必须>=1
  Result := CreateUnaryExpr(opLIST);
  L := ParseSetElement;
  TUnaryExpr(Result).Operand := L;
  while CurToken = tkComma do
  begin
    NextToken;
    L2 := ParseSetElement;
    L.Next := L2;
    L2.Parent := L.Parent;
    L := L2;
  end;
end;

function TParser.ParseSimpleStmt: TStatement;

  function SymbolCanAssignTo(Ref: TSymbol): Boolean;
  begin
    case Ref.NodeKind of
      nkVariable: Result := not (vaReadOnly in TVariable(Ref).VarAttr);
      nkField: Result := True;
      nkProperty: Result := TProperty(Ref).Setter <> nil;
      nkIntfProperty: Result := TIntfProperty(Ref).Setter <> nil;
      nkArgument: Result := TArgument(Ref).Modifier <> argConst;
    else
      Result := False;
    end;
  end;

  // 检查是否可以作为左值
  function CheckLValue(L: TExpr): Boolean;

    function IsSizeEqual(E: TExpr): Boolean;
    var
      Left, R: TExpr;
    begin
      // E is TBinaryExpr
      // E.Right is opLIST
      R := TBinaryExpr(E).Right;
      R := TUnaryExpr(R).Operand;
      Left := TBinaryExpr(E).Left;

      if R.Typ.TypeCode = typUntype then
        Result := True
      else
        Result := Left.Typ.Size = R.Typ.Size;
    end;
  var
    Ref: TSymbol;
  begin
    // 必须指向内存地址
    {
      var p: pchar;
      p := nil;
      p^ := a;
      (p + 1)^ := #0;
      @procvar := nil;
      arr[5] := 1;
      Integer(p^) := 5;
    }
    case L.OpCode of
      opMEMBER: begin
        Ref := TSymbolExpr(TBinaryExpr(L).Right).Reference;
        Result := SymbolCanAssignTo(Ref);
      end;
      opSYMBOL: begin
        Ref := TSymbolExpr(L).Reference;
        Result := SymbolCanAssignTo(Ref);
      end;
      opINDEX:
        Result := True;

      opINST: Result := L.Typ.TypeCode <> typUntype;

      opCAST: Result := IsSizeEqual(L);  // todo 2: 可能不需要检查

      opADDR:
        Result := TUnaryExpr(L).Operand.Typ.TypeCode = typProcedural;
    else
      Result := False;
    end;

  end;

  function CheckLExpr(var L: TExpr): Boolean;
  begin
    Include(FCurStates, psInLeftVal);
    Result := CheckExpr(L);
    Exclude(FCurStates, psInLeftVal);
  end;

  function CheckRExpr(L: TExpr; var R: TExpr): Boolean;
  begin
   // 已经处理：把函数赋值给函数变量 
    FExpectedProcType := L.Typ.TypeCode = typProcedural;
    Result := CheckExpr(R);
    FExpectedProcType := False;
  end;

  procedure CheckAssign(L, R: TExpr);
  begin
    if (L.Typ.TypeCode = typClassRef) and (R.Typ.TypeCode = typClass) then
      if not R.IsTypeSymbol then
      begin
        ParseError(L.Coord, SErr_IncompatibleTypes);
        Exit;
      end;

    if R.IsNilConst and (L.Typ.TypeCode in [typPointer, typClass,
        typClassRef, typPAnsiChar, typPWideChar, typDynamicArray,
        typInterface, typDispInterface]) then
    begin
      Exit;
    end;

    if not CheckAssignmentCompatibility(L.Typ, R.Typ) then
      ParseError(L.Coord, SErr_IncompatibleTypes);
  end;

  procedure ProcessDelayed(L, R: TExpr);
  begin
    if R.OpCode <> opADDR then
      InternalError('Expect opADDR in ProcessDelayed');
    if R.Typ.TypeCode <> typProcedural then
      InternalError('Expect procedural type in ProcessDelayed');

    // 重载的函数可以赋值给Pointer类型,取第一个函数
    if L.Typ.IsUntypePointer then Exit;

    if L.Typ.TypeCode <> typProcedural then
      ParseError(L.Coord, SErr_IncompatibleTypes);
    FindProper(TUnaryExpr(R), TProceduralType(L.Typ));
    Exclude(R.Attr, eaDelayed);
  end;
var
  L: TExpr;
  Lab: TSymbol;
  AsgStmt: TAssignmentStmt;
begin
  if CurToken in [tkIdentifier, tkIntConst, tkAt] then
  begin
    FTemp := CurTokenString;
    NextToken;
    if CurToken = tkColon then
    begin // 标号
      Lab := FindSymbol(FTemp);
      if Lab = nil then
        ParseError(SErr_UndeclaredIdent)
      else if Lab.NodeKind <> nkLabel then
        ParseError(SErr_LabelExpected)
      else if TStmtLabel(Lab).Stmt <> nil then
        ParseError(SErr_RedeclaredIdent);

      FTemp := '';
      NextToken;
      Result := TEmptyStmt(CreateStmt(TEmptyStmt));
      if (Lab <> nil) and (Lab.NodeKind = nkLabel) then
        TStmtLabel(Lab).Stmt := Result;
    end
    else
    begin
      UngetToken;
      if CurToken = tkAt then
        L := ParseFactor
      else
        L := ParseDesignator;

      if L.OpCode = opADDR then
        Expect(tkAssign);

      if CurToken = tkAssign then
      begin
        NextToken;
        AsgStmt := TAssignmentStmt(CreateStmt(TAssignmentStmt));
        Result := AsgStmt;
        AsgStmt.Left := L;
        AsgStmt.Right := ParseExpr;
        if CheckLExpr(L) and CheckRExpr(L, AsgStmt.Right) then
        begin
          if not CheckLValue(L) then
            ParseError(L.Coord, 'Left side can not assign to');

          // 检查右值
          if AsgStmt.Right.Typ.TypeCode = typUntype then
            ParseError('expression not return a value')
          else begin
            if eaDelayed in AsgStmt.Right.Attr then
              ProcessDelayed(L, AsgStmt.Right);
            CheckAssign(L, AsgStmt.Right);
          end;
        end;
      end
      else
      begin
        // call stmt
        Result := CreateStmt(TCallStmt);
        if CheckExpr(L) then
          if L.OpCode <> opCALL then  // todo 1: 对于obj.show之类的有问题
            ParseError('Call statement expected');
        TCallStmt(Result).CallExpr := L;
      end;
    end;
  end
  else
  begin
    ParseError('Identifier expected');
    Result := CreateStmt(TCallStmt);
    NextToken;
  end;
end;

function TParser.ParseStatement(Parent: TStatement): TStatement;
label Start;
begin
Start:
  SetTempExpr(False);
  case CurToken of
    tkAsm:
      begin
        ParseError('Asm statement unsupported', True);
        Result := nil;
      end;
    tkIf: Result := ParseIfStmt;
    tkCase: Result := ParseCaseStmt;
    tkWhile: Result := ParseWhileStmt;
    tkFor: Result := ParseForStmt;
    tkRepeat: Result := ParseRepeatStmt;
    tkTry: Result := ParseTryStmt;
    tkWith: Result := ParseWithStmt(Parent);
    tkRaise: Result := ParseRaiseStmt;
    tkGoto: Result := ParseGotoStmt;
    tkBegin: Result := ParseCompoundStmt;
    tkElse: Result := nil;
    tkEnd: Result := nil;
//    tkOn: Expect(tkIdentifier);
    tkSemicolon:
      begin
        while CurToken = tkSemicolon do
          NextToken;
        Result := nil;
        //Goto Start;
      end;
  else
    Result := ParseSimpleStmt;
    if Result.StmtKind = skCallStmt then
      Result := StmtAdjust(Result);
  end;
  if Result <> nil then Result.Parent := Parent;
  SetTempExpr(True);
end;

function TParser.ParseStmtList(Parent: TStatement; EndTokens: TTokens): TCompoundStmt;
var
  Stmt: TStatement;
begin
// 和ParseCompoundStmt类似，但无begin和end.
  Result := TCompoundStmt(CreateStmt(TCompoundStmt));
  Result.Parent := Parent;
  while not (CurToken in EndTokens) do
  begin
    Stmt := ParseStatement(Result);
    if Stmt <> nil then
      Result.Statements.Add(Stmt);
    if CurToken = tkSemicolon then NextToken;
  end;
end;

function TParser.ParseStrExpr(const DefValue: string): string;
var
  E: TExpr;
begin
  E := ParseExpr;
  if CheckExpr(E) and (E.Typ.TypeCode in [typAnsiString, typWideString, typUnicodeString]) then
  begin
    ValClear(FTempValue);
    FTempValue := EvalExpr(E);
    Result := ValToStr(FTempValue);
    ValClear(FTempValue);
  end
  else begin
    ParseError('String constant expression expected');
    Result := DefValue;
  end;
end;

function TParser.ParseTryStmt: TTryStmt;

  function IsOn: Boolean;
  begin
    Result := (CurToken = tkOn) or ((CurToken = tkIdentifier) and SameText(CurTokenString, 'on'));
  end;

  function ParseExceptBlock(TryStmt: TTryStmt): TExceptBlock;
  var
    Handler: TExceptHandler;
    Sym: TSymbol;
  begin
    NextToken; // skip 'except'

    Result := TExceptBlock.Create;
    TryStmt.ExceptBlock := Result;
    if IsOn then
    begin
      while True do
      begin
        if IsOn then
        begin
          NextToken; // skip 'on'
          Handler := TExceptHandler.Create;
          Handler.ExceptVar := TVariable(CreateElement(TVariable));
          Result.AddExceptHandler(Handler);
          // parse except var
          Expect(tkIdentifier);
          FTemp := CurTokenString;
          NextToken;
          if CurToken = tkColon then
          begin
            // case 1, on E: Exception do
            NextToken; // skip ':'
            Handler.ExceptVar.Name := FTemp;
            FTemp := '';
          end;
            // case 2, on Exception do
          Sym := ParseQualifiedSym(FTemp);
          if Sym = nil then
            ParseError(SErr_UndeclaredIdent, [FQID.Id]);
          if (Sym.NodeKind <> nkType) and (TType(Sym).TypeCode <> typClass) then
          begin
            ParseError(SErr_ClassRequired);
            Sym := FContext.FTObjectType;
          end;
          FQID.Reset;
          Handler.ExceptVar.VarType := TType(Sym);
          Expect(tkDo);
          NextToken;
          if Handler.ExceptVar.Name <> '' then
            Self.CurSymbols.Add(Handler.ExceptVar);
          Handler.Stmt := Self.ParseStatement(TryStmt);
          if Handler.ExceptVar.Name <> '' then
            Self.CurSymbols.Remove(Handler.ExceptVar.Name);

//          while CurToken = tkSemicolon do
//            NextToken;
        end
        else if CurToken = tkElse then
        begin
          NextToken;
          Result.Default := Self.ParseStmtList(TryStmt, [tkEnd]);
          Break;
        end
        else
          Break;
      end
    end
    else // case 3, try dosome; except end; 
      Result.Default := ParseStmtList(TryStmt, [tkEnd]);

  end;
begin
  NextToken; // skip 'try'
  Result := TTryStmt(CreateStmt(TTryStmt));
  Result.Stmt := ParseStmtList(Result, [tkFinally, tkExcept]);
  if CurToken = tkFinally then
  begin
    NextToken;
    Result.FinallyStmt := ParseStmtList(Result, [tkEnd])
  end
  else
    ParseExceptBlock(Result);
  Expect(tkEnd);
  NextToken;
end;

function TParser.ParseTypeDecl(const TypName: string; Parent: TSymbol): TType;

  procedure ParseEnumType(T: TEnumType);
  var
    E: TEnumValue;
    Expr: TExpr;
    Value: Integer;
  begin
    Value := 0;
    E := nil;
    SetTempExpr(True);
    NextToken; // skip '('
    repeat
      Expect(tkIdentifier);
      if CurToken = tkIdentifier then
      begin
        E := TEnumValue(CreateElement(TEnumValue));
        E.Name := CurTokenString;
        E.Value := Value;
        E.EnumType := T;
        T.Values.Add(E);
        AddSymbol(E);
      end
      else
        Break;

      NextToken;
      if CurToken = tkEqual then
      begin
        NextToken;
        ValClear(FTempValue);
        Expr := ParseExpr;
        if EvalExpr(Expr, FTempValue) then
        begin
          Value := ValToInt(FTempValue);
          E.Value := Value;
        end;
        ValClear(FTempValue);
      end
      else if CurToken <> tkComma then Break;

      Inc(Value);
      Expect(tkComma);
      NextToken;
    until False;
    ClearTempExprList;
    Expect(tkBraceClose);
    NextToken;
  end;

  function ParseTypeName(Alias: Boolean): TType;
  var
    T: TType;
  begin
    Result := ParseTypeRef;
    if Result = FContext.FAnytype then
      ParseError('Type expected');
    if Alias then
    begin
      T := CreateType(TClonedAliasType);
      TClonedAliasType(T).RefType := Result;
      Result := T;
    end;
  end;

  function CanDelayDecl(Qd: TQualifiedId): Boolean;
  begin
  // 不带任何限定, 可以延迟声明
    Result := Qd.CountOfNames = 1;
  end;

  function ParsePointerType: TType;
  var
    Typ: TType;
    Sym: TSymbol;
  begin
    sym := ParseQualifiedSym;
    if Sym = nil then
    begin
      if CanDelayDecl(FQId) then
      begin
        Typ := TUnresolvedType.Create;
        Typ.Name := FQID.Name;
      end
      else begin
        ParseError(SErr_UndeclaredIdent, [FQId.Id]);
        Typ := FContext.FIntegerType;
      end;
    end
    else if Sym.NodeKind <> nkType then
    begin
      ParseError(SErr_SymbolNotType, [FQId.Id]);
      Typ := FContext.FIntegerType;
    end
    else
      Typ := TType(Sym);

    FQId.Reset;
    Result := CreateType(TPointerType);
    TPointerType(Result).RefType := Typ;
    Result.Size := FPointerSize;
  end;

  function CheckSubRng(L, R: TExpr): Boolean;
  var
    Sym: TSymbol;
  begin
    Result := False;
    if not L.Typ.IsOrdinal then
    begin
      ParseError(SErr_ExpectOrdinal);
      Exit;
    end;

    if not R.Typ.IsOrdinal then
    begin
      ParseError(SErr_ExpectOrdinal);
      Exit;
    end;

    Sym := L.GetReference;
    if (Sym <> nil) and not (Sym.NodeKind in [nkConstant, nkEnumElement]) then
    begin
      ParseError(SErr_ExpectOrdinal);
      Exit;
    end;

    Sym := R.GetReference;
    if (Sym <> nil) and not (Sym.NodeKind in [nkConstant, nkEnumElement]) then
    begin
      ParseError(SErr_ExpectOrdinal);
      Exit;
    end;

    Result := True;
  end;

  // 分析两种情况:
  // MyT = Vcl.TRect;
  // MyT = 1+2..5*6;
  function ParseType(Alias: Boolean): TType;
  var
    L, R: TExpr;
    LVal, RVal: TValueRec;
  begin
  // todo 1: 注意是否支持 ttt = aa..bb
    L := ParseTypeExpr;
    if CurToken = tkDotDot then
    begin
      NextToken;
      R := ParseTypeExpr;

      if CheckExpr(L) and CheckExpr(R) and CheckSubRng(L, R) then
      begin
        LVal := EvalExpr(L);
        RVal := EvalExpr(R);
        Result := CreateType(TSubrangeType);
        if ValToInt64(LVal) > ValToInt64(RVal) then
        begin
          TSubrangeType(Result).RangeBegin := ValToInt64(RVal);
          TSubrangeType(Result).RangeEnd := ValToInt64(LVal);
          ParseError(L.Coord, SErr_SubrangeOutOfBound);
        end else
        begin
          TSubrangeType(Result).RangeBegin := ValToInt64(LVal);
          TSubrangeType(Result).RangeEnd := ValToInt64(RVal);
        end;

        case L.Typ.TypeCode of
          typBoolean..typLongBool:
            TSubrangeType(Result).BaseType := FContext.FTypes[typBoolean];
          typAnsiChar:
            TSubrangeType(Result).BaseType := FContext.FTypes[typAnsiChar];
          typWideChar:
            TSubrangeType(Result).BaseType := FContext.FTypes[typWideChar];
          typEnum:
            TSubrangeType(Result).BaseType := L.Typ;
        else
          TSubrangeType(Result).BaseType := FContext.TypeOfRange(
              TSubrangeType(Result).RangeBegin,
              TSubrangeType(Result).RangeEnd
            );
        end;
      end
      else
        Result := nil;
    end
    else if L.OpCode in [opSYMBOL, opMEMBER] then
    begin
      if CheckExpr(L) then
        Result := L.Typ
      else
        Result := nil;
    end
    else begin
      Result := nil;
      ParseError('Error in type declaraction');
    end;
    ClearTempExprList;
  end;

{
<CallType> ::= PROCEDURE <OptFormalParms> <OptCallConventions>
      | PROCEDURE   <OptFormalParms>                  OF OBJECT <OptCallConventions>
      | FUNCTION    <OptFormalParms> ':' <ResultType>           <OptCallConventions>
      | FUNCTION    <OptFormalParms> ':' <ResultType> OF OBJECT <OptCallConventions>
}
  function ParseProceduralType: TProceduralType;
    function EndOfDecl: Boolean;
    begin
      if psInVar in fCurStates then
        Result := (CurToken = tkEqual) or (CurToken = tkSemicolon)
      else
        Result := CurToken = tkSemicolon;
    end;
  var
    IsFunc: Boolean;
    CC: TCallingConvention;
    M: TFunctionModifier;
  begin
    IsFunc := CurToken = tkFunction;
    NextToken; // skip 'function'/'procedure'

    Result := TProceduralType(CreateType(TProceduralType));
    if CurToken = tkBraceOpen then
    begin
      Result.CreateArgs;
      ParseArgumentList(Result, Result.Args);
    end;

    if IsFunc then
    begin
      // parse return type
      Expect(tkColon);
      if CurToken = tkColon then NextToken;
      Result.ReturnType := ParseTypeRef;
    end;

    if CurToken = tkOf then
    begin
      NextToken;
      Expect(tkObject);
      if CurToken = tkObject then
        Result.IsMethodPointer := True;
      NextToken;
    end;

    if CurToken = tkSemicolon then
      NextToken;

    if (CurToken = tkIdentifier) and IsCallConv(CurTokenString, cc) then
    begin
      // 分析指令直至 ';'
      Result.CallConvention := cc;
      NextToken;
      while not EndOfDecl do
      begin
        Expect(tkIdentifier);
        if IsCallConv(CurTokenString, cc) then
          Result.CallConvention := cc
        else if IsModifier(CurTokenString, M) then
          // 忽略
        else
          ParseError('Invalid procedural directive %s', [CurTokenString]);
        NextToken;
      end;
    end
    else if CurToken <> tkEqual then
      UngetToken;
  end;

  function ParseArrayType: TType;
  var
    IsDyn: Boolean;
    A1, LastArr: TArrayType;
    Typ: TType;
  begin
    NextToken; // skip 'array';
    LastArr := nil;
    if CurToken = tkSquaredBraceOpen then
    begin
      IsDyn := False;
      Result := nil;
      NextToken;
      repeat
        A1 := TArrayType(CreateType(TArrayType));
        Typ := ParseType(False);
        if (Typ <> nil) and Typ.IsOrdinal then
        begin
          case Typ.TypeCode of
            typEnum: TArrayType(A1).Range := GetSubrangeType(TEnumType(Typ));
            typSubrange: TArrayType(A1).Range := TSubrangeType(Typ);
          else
            TArrayType(A1).Range := FContext.GetSubrangeType(Typ.TypeCode);
          end;
        end;

        if A1.Range = nil then
          A1.Range := FContext.FBoolRangeType;

        if Result = nil then
          Result := A1;
        if LastArr <> nil then
          LastArr.ElementType := A1;
        LastArr := A1;

        if CurToken <> tkComma then Break;
        NextToken;
      until False;

      Expect(tkSquaredBraceClose);
      NextToken;
    end
    else begin
      IsDyn := True;
      Result := CreateType(TDynamicArrayType);
    end;

    Expect(tkOf);
    NextToken;
    Typ := ParseTypeDecl;
    if IsDyn then
      TDynamicArrayType(Result).ElementType := Typ
    else
      LastArr.ElementType := Typ;
    //  TArrayType(Result).ElementType := Typ;
    if psInPacked in FCurStates then
      TArrayType(Result).IsPacked := True;
    if IsDyn then
      Result.Size := FPointerSize
    else
      TArrayType(Result).Update;
  end;

  function ParseSetType: TSetType;
  var
    T: TType;
    RangeType: TSubrangeType;
  begin
    NextToken; // skip 'set'
    Expect(tkOf);
    NextToken;
    T := ParseType(False);

    if T.IsOrdinal then
    begin
      if not Assigned(T.Parent) then T.Parent := Parent;

      case T.TypeCode of
        typSubrange: RangeType := TSubrangeType(T);
        typEnum: RangeType := GetSubrangeType(TEnumType(T));
      else
        RangeType := FContext.GetSubrangeType(T.TypeCode);
      end;

      if RangeType.RangeEnd - RangeType.RangeBegin + 1 > 256 then
        ParseError('Sets may have at most 256 elements');
      Result := GetSetType(RangeType);
    end
    else begin
      ParseError(SErr_ExpectOrdinal);
      Result := FContext.FByteSetType;
    end;
  end;

  // 返回true,类型可以接受
  function CheckFileType(Typ: TType): Boolean;
  var
    I: Integer;
  begin
    if Typ = nil then begin
      Result := True;
      Exit;
    end;

    Result := False;
    case Typ.TypeCode of
      typFile..typText:
        ParseError('File type not allowed in here');
      typAnsiString..typUnicodeString,
      typVariant..typOleVariant,
      typDynamicArray, typInterface, typDispInterface:
        ParseError('Type need finalization, not allowed in file type');
      typArray:
        Result := CheckFileType(TArrayType(Typ).ElementType);
      typRecord:
        for I := 0 to TRecordType(Typ).Symbols.Count - 1 do
        begin
          Result := CheckFileType(TField(TRecordType(Typ).Symbols[I]).FieldType);
          if not Result then Exit;
        end;
      else
        Result := True;
    end;
  end;

  function ParseFileType: TFileType;
  var
    Typ: TType;
  begin
    NextToken; // skip 'file'
    if CurToken = tkOf then
    begin
      NextToken;
      Typ := ParseTypeDecl;
      Result := TFileType(CreateType(TFileType));
      Result.ElementType := Typ;
      CheckFileType(Result.ElementType);
    end
    else
      Result := TFileType(FContext.FTypes[typFile]);
  end;

  function ParsePackedType: TType;
  var
    StateInfo: TParseStateInfo;
  begin
    NextToken;
    if CurToken in [tkFile, tkSet, tkArray, tkRecord, tkClass] then
    begin
      StateSet(psInPacked, StateInfo);
      Result := ParseTypeDecl;
      StateRestore(StateInfo);
    end
    else
    begin
      ParseError('Packed not allow here');
      Result := nil;
    end;
  end;

  function ParseShortString: TShortStringType;
  begin
    Result := TShortStringType(CreateElement(TShortStringType));
    NextToken;
    Result.CharCount := Self.ParseIntExpr(1);
    if Result.Size > 255 then
    begin
      ParseError(SErr_ShortStrSize);
      Result.CharCount := 255;
    end;
    Result.Update;
    Expect(tkSquaredBraceClose);
    NextToken;
  end;

begin
{
<TypeDecl>		::= <TypeId> '=' <TypeSpec>
			  | SynError ';'
}
  // 分析类型,直到';'或'='出现

  case CurToken of
    tkBraceOpen:
      begin // enum
        Result := CreateType(TEnumType);
        ParseEnumType(TEnumType(Result));
        TEnumType(Result).MinEnumSize := FMinEnumSize;
        TEnumType(Result).Update;
      end;

    tkCaret:
      begin  // pointer
        NextToken;
        Expect(tkIdentifier);
        Result := ParsePointerType;
      end;

    tkIdentifier, tkIntConst, tkHexConst, tkCharConst, tkStrConst:
      Result := ParseType(not (psInVar in FCurStates));

    tkType:
      begin // My = type Byte;
        if (psInVar in FCurStates) or (psInField in FCurStates) then
          Expect(tkIdentifier);
        NextToken;
        if CurToken <> tkString then
          Expect(tkIdentifier);
        Result := ParseTypeName(True);  // 可以用parseQSym
      end;

    tkProcedure: Result := ParseProceduralType;
    tkFunction: Result := ParseProceduralType;
    tkRecord: Result := ParseRecordType(TypName, Parent);

    tkString:
      begin
        NextToken;
        if CurToken = tkSquaredBraceOpen then
          Result := ParseShortString
        else
          Result := FContext.FStringType;
      end;
    tkPacked: Result := ParsePackedType;
    tkArray: Result := ParseArrayType;
    tkSet: Result := ParseSetType;
    tkFile: Result := ParseFileType;
  else
    Result := nil;
  end;

  if Result = nil then
    Result := FContext.FIntegerType;

  if Result.TypeCode in [typPointer, typClass, typClassRef,
      typInterface, typDispInterface, typDynamicArray, typFile, typText] then
    Result.Size := FPointerSize
  else if Result.TypeCode = typProcedural then
  begin
    if TProceduralType(Result).IsMethodPointer then
      Result.Size := FPointerSize * 2
    else
      Result.Size := FPointerSize;
  end
  else if Result.TypeCode = typAlias then
    Result.Size := TAliasType(Result).RefType.Size
  else if Result.TypeCode = typClonedType then
    Result.Size := TClonedAliasType(Result).RefType.Size;
end;

function TParser.ParseTypeExpr: TExpr;

  function ParseTypeExprList: TExpr;
  var
    L, L2: TExpr;
  begin
  {<ExprList>		::= <TypeExpr>
          | <ExprList> ',' <TypeExpr>
  }
  // 读取表达式列表, 数目必须>=1
    Result := CreateUnaryExpr(opLIST);
    L := ParseTypeExpr;
    TUnaryExpr(Result).Operand := L;
    while CurToken = tkComma do
    begin
      NextToken;
      L2 := ParseTypeExpr;
      L.Next := L2;
      L2.Parent := L.Parent;
      L := L2;
    end;
  end;

	function ParseType_Designator: TExpr;
	var
		L, R: TExpr;
	begin
	{
	<FieldDesignator>	::= <RefId>
					| <FieldDesignator> '.' <RefId>

	<TypeDesignator>		::= <FieldDesignator>
					| <Designator> '.' <FieldDesignator>
					| <Designator> '(' <ExprList> ')' !FunctionCall or TypeCast
					| <Designator> '('  ')'           !FunctionCall
					| '(' <Designator> ')'
	}

		if CurToken = tkBraceOpen then
		begin
			NextToken;
			Expect(tkIdentifier);
			Result := ParseType_Designator;
			Expect(tkBraceClose);
			NextToken;
			Exit;
		end;

		ParseQualifiedId;
		Result := SimplifyQualId;
		FQId.Reset;

		if CurToken = tkBraceOpen then
		begin 
			// 函数调用或类型转换
			L := Result;
			Result := CreateBinaryExpr(opCALL); // 暂定为函数调用

			NextToken;
			if CurToken = tkBraceClose then
				R := nil
			else
				R := ParseTypeExprList;  // todo 1: ??这里要改

			TBinaryExpr(Result).Left := L;
			TBinaryExpr(Result).Right := R;
			Expect(tkBraceClose);
			NextToken;
		end;
	end;

	function ParseType_Factor: TExpr;
	var
		L: TExpr;
	begin
	{
	<TypeFactor>		::= NIL
					| <ICONST>
					| <RCONST>
					| <SCONST>
					| <Designator>
					| '(' <Expr> ')'
          | <Set Constructor>
					| '+' <Factor>
					| '-' <Factor>
					| NOT <Factor>
	}
		case Self.CurToken of
			tkNil, tkIntConst, tkHexConst, tkFalse, tkTrue,
			tkOctalConst, tkStrConst, tkCharConst, tkBinConst:
				Result := ParseFactor;

			tkPlus: begin
				NextToken;
				L := ParseType_Factor;
				Result := CreateUnaryExpr(opPOS, L);
			end;

			tkMinus: begin
				NextToken;
				L := ParseType_Factor;
				Result := CreateUnaryExpr(opNEG, L);
			end;

			tkNot: begin
				NextToken;
				L := ParseType_Factor;
				Result := CreateUnaryExpr(opNOT, L);
			end;

			tkBraceOpen: begin
				NextToken;
				Result := ParseTypeExpr;
				Expect(tkBraceClose);
				NextToken;
			end;

			tkIdentifier: begin
				Result := ParseType_Designator;
			end;

    else
      Expect(tkIdentifier);
      Result := CreateUnaryExpr(opNONE, nil);
		end;
	end;

	function ParseType_MulExpr: TExpr;

		function MulOp(T: TToken): TExprOpCode;
		begin
			case T of
				tkMul        : Result := opMUL;
				tkFDiv       : Result := opFDIV;
				tkMod        : Result := opMOD;
				tkDiv        : Result := opIDIV;
				tkAnd        : Result := opAND;
				tkSHR        : Result := opSHR;
				tkSHL        : Result := opSHL;
			else
				Result := opNONE;
			end;
		end;
	var
		left, right: TExpr;
		op: TExprOpCode;
	begin
	{
	<MulExpr>		::= <Factor>
					| <MulExpr> <MulOp> <Factor>
	}
		Result := ParseType_Factor;
		op := MulOp(CurToken);
		while op <> opNONE do
		begin
			NextToken;
			left := Result;
			right := ParseType_Factor;
			Result := CreateBinaryExpr(op, left, right);
			op := MulOp(CurToken);
		end;
	end;

	function ParseType_AddExpr: TExpr;

		function AddOp(T: TToken): TExprOpCode;
		begin
			case T of
				tkPlus                  : Result := opAdd;
				tkMinus                 : Result := opSUB;
				tkOr                    : Result := opOR;
				tkXor                   : Result := opXOR;
			else
				Result := opNone;
			end;
		end;
	var
		left, right: TExpr;
		op: TExprOpCode;
	begin
	{
	<AddExpr>		::= <MulExpr>
					| <AddExpr> <AddOp> <MulExpr>
	}
		Result := ParseType_MulExpr;
		op := AddOp(CurToken);
		while op <> opNONE do
		begin
			NextToken;
			left := Result;
			right := ParseType_MulExpr;
			Result := CreateBinaryExpr(op, left, right);
			op := AddOp(CurToken);
		end; 
	end;

  function RelOp(T: TToken): TExprOpCode;
  begin
    case T of
      tkLessThan              : Result := opLT;
      tkEqual                 : Result := opEQ;
      tkGreaterThan           : Result := opGT;
      tkNotEqual              : Result := opNE;
      tkLessEqualThan         : Result := opLE;
      tkGreaterEqualThan      : Result := opGT;
//      tkIs                    : Result := opIS;
//      tkAs                    : Result := opAS;
      tkIn                    : Result := opIN;
    else
      Result := opNone;
    end;
  end;

var
  L, R: TExpr;
  op: TExprOpCode;
begin
// 分析类型表达式
{
MySubrange = a.b * 2 div 1 .. 100;
My2 = 1>2..True;
My3 = sizeof(Integer)..Sizeof(Double)
}

  Result := ParseType_AddExpr;

  if Result.OpCode in [opSYMBOL, opMEMBER] then Exit;

  op := RelOp(CurToken);
  while op <> opNONE do
  begin
    NextToken;
    L := Result;
    R := ParseType_AddExpr;
    Result := CreateBinaryExpr(op, L, R);
    op := RelOp(CurToken);
  end;
end;

function TParser.ParseTypeRef: TType;
var
  E: TExpr;
begin
  // CurToken is tkIdentifier / tkString
  if CurToken = tkString then
  begin
    Result := FContext.FStringType;
    NextToken;
    Exit;
  end;

  ParseQualifiedId();
  E := SimplifyQualId;
  FQId.Reset;
  Result := nil;
  // todo 1: 这里有问题,如果Expr不是类型符号,会有错误
  // 如 a: ptr
  if CheckExpr(E) then
  begin
    if E.OpCode = opMEMBER then
      E := TBinaryExpr(E).Right;
    with TSymbolExpr(E) do
      if Reference.NodeKind = nkType then
        Result := TType(Reference);
  end;

  if Result = nil then
    Result := FContext.FAnytype;
end;

procedure TParser.ParseTypeSection(Parent: TSymbol);

  procedure CheckUnresolved(Typ: TType);
  var
    Ref, Resolved: TType;
    Sym: TSymbol;
  begin
    while Typ <> nil do
    begin
      if typ.TypeCode = typClassRef then
        Ref := TClassRefType(typ).RefType
      else if typ.TypeCode = typPointer then
        Ref := TPointerType(typ).RefType
      else
        Ref := nil;

      if Ref = nil then Continue;

      Resolved := nil;
      Sym := FindSymbol(TUnresolvedType(Ref).Name);
      if Sym = nil then
        ParseError(typ.Coord, SErr_UnresolvedIdent, [TUnresolvedType(Ref).Name])
      else if Sym.NodeKind <> nkType then
        ParseError(typ.Coord, SErr_SymbolNotType, [TUnresolvedType(Ref).Name])
      else
        Resolved := TType(Sym);

      if typ.TypeCode = typClassRef then
      begin
        if Resolved = nil then Resolved := FContext.FTObjectType;
        TClassRefType(typ).RefType := TClassType(Resolved);
      end
      else if typ.TypeCode = typPointer then
      begin
        if Resolved = nil then Resolved := FContext.FIntegerType;
        TPointerType(typ).RefType := Resolved;
      end;
      Ref.Free; // this type is not added to FNodes

      Ref := Typ;
      Typ := TType(Typ.Next);
      Ref.Next := nil;
    end;
  end;

 { procedure NextT;
  var
    StateInfo: TParseStateInfo;
  begin
    StateSet(psInClass, StateInfo);
    NextToken;
    StateRestore(StateInfo);
  end; }
var
  Typ, T2: TType;
  StateInfo: TParseStateInfo;
  TypName: string;
  Unresolved: TType;
  Noname, NotAddSym: Boolean;

  procedure AddUnresolved(Sym: TType);
  begin
    Sym.Next := Unresolved;
    Unresolved := Sym;
  end;
begin
{
<TypeSection>		::= TYPE <TypeDeclList>

<TypeDeclList>		::= <TypeDecl>
			  | <TypeDeclList>  <TypeDecl>
}
  NextToken; // skip 'type'
  Expect(tkIdentifier);

  StateSet(psInType, StateInfo);
  Unresolved := nil;
  while CurToken = tkIdentifier do
  begin
    TypName := CurTokenString;
    NextToken;
    Expect(tkEqual);

    NextToken;

    Noname := False;
    NotAddSym := False;
    case CurToken of
      tkClass:
        begin
          // 在class中,private等成为关键字,防止Scanner把这些词扫描成一般标识符
          Scanner.EnableScopeKeyWords(True);
          NextToken;
          if CurToken = tkOf then
            Typ := ParseClassRefType
          else
            Typ := ParseClassType(TypName, Parent, NotAddSym);
          Scanner.EnableScopeKeyWords(False);
        end;

      tkRecord:
        begin
          Scanner.EnableScopeKeyWords(True);
          Typ := ParseRecordType(TypName, Parent);
          Scanner.EnableScopeKeyWords(False);
          NotAddSym := True;
        end;

      tkInterface, tkDispInterface: 
        Typ := ParseInterfaceType(TypName, Parent, NotAddSym);

      tkObject:
        begin
          Scanner.EnableScopeKeyWords(True);
          Typ := ParseObjectType(TypName);
          Scanner.EnableScopeKeyWords(False);
          NotAddSym := True;
        end;
    else
      Typ := ParseTypeDecl(TypName, Parent);
      Noname := True;
    end;

    if (Typ.Name <> '') and Noname then
    begin
      T2 := Typ;
      Typ := CreateType(TAliasType);
      TAliasType(Typ).RefType := T2;
    end;

    if Typ.Name = '' then Typ.Name := TypName;

    if not NotAddSym then
      AddSymbol(Typ);

    Typ.Visibility := FCurVisibility;
    case Typ.TypeCode of
      typClassRef:
        if TClassRefType(Typ).RefType.TypeCode = typUntype then AddUnresolved(Typ);
      typPointer:
        if TPointerType(Typ).RefType.TypeCode = typUntype then AddUnresolved(Typ);
    end;
    Expect(tkSemicolon);
    NextToken;
  end;
  StateRestore(StateInfo);

  CheckUnresolved(Unresolved);
end;

function TParser.ParseUnit: TModule;
begin
  FModule := TModule(CreateElement(TModule));
  ParseUnitInterface(FModule);
  ParseUnitImplementation;
  Result := FModule;
end;

procedure TParser.ParseUnitImplementation;

  // 把Symbols中标志为saInternal的添加到InternalSymbols;
  procedure CleanupSym;
  var
    globalst, localst: TSymbolTable;
    i, count: Integer;
    sym: TSymbol;

    procedure CleanupFunc(sym: TFunctionDecl);
    var
      gf, lf, f: TFunctionDecl;
    begin
      gf := nil;
      lf := nil;
      while sym <> nil do
      begin
        f := sym.NextOverload;
        if saInternal in sym.Attr then
        begin
          if lf = nil then
            lf := sym
          else
            lf.NextOverload := sym;
        end
        else
        begin
          if gf = nil then
            gf := sym
          else
            gf.NextOverload := sym;
        end;
        sym.NextOverload := nil;
        sym := f;
      end;
      if gf <> nil then globalst.Add(gf);
      if lf <> nil then localst.Add(lf);
    end;
  begin
    count := 0;
    for i := 0 to FModule.Symbols.Count - 1 do
    begin
      if saInternal in FModule.Symbols[i].Attr then
        Inc(count);
    end;

    if count = 0 then Exit;

    if count = FModule.Symbols.Count then
    begin
      localst := FModule.InternalSymbols;
      FModule.Symbols := FModule.InternalSymbols;
      FModule.Symbols := localst;
      Exit;
    end;

    globalst := nil;
    localst := nil;
    try
      globalst := TSymbolTable.Create(FModule);
      localst := TSymbolTable.Create(FModule);
      localst.Capacity := Trunc(count * 1.39);
      globalst.Capacity := Trunc((FModule.Symbols.Count - count) * 1.39);
      globalst.AutoAddToOwner := False;
      localst.AutoAddToOwner := False;

      for i := 0 to FModule.Symbols.Count - 1 do
      begin
        sym := FModule.Symbols[i];
        if sym.NodeKind in [nkFunc, nkMethod, nkExternalFunc] then
          CleanupFunc(TFunctionDecl(sym))
        else if saInternal in sym.Attr then
          localst.Add(sym)
        else
          globalst.Add(sym);
      end;
      globalst.AutoAddToOwner := True;
      localst.AutoAddToOwner := True;
    except
      globalst.Free;
      localst.Free;
      raise;
    end;
    FModule.Symbols.Free;
    FModule.Symbols := globalst;
    FModule.InternalSymbols.Free;
    FModule.InternalSymbols := localst;
  end;

  procedure CleanupUnit;
  var
    i: Integer;
    newSt: TSymbolTable;
  begin
    if FModule.LoadedUnits.Count < 2 then Exit;

    newSt := TSymbolTable.Create(nil);
    newSt.AutoAddToOwner := False;
    newSt.Capacity := FModule.LoadedUnits.Count;
    for i := 0 to FModule.LoadedUnits.Count - 1 do
    begin
      if not (saInternal in FModule.LoadedUnits[i].Attr) then
        newSt.Add(FModule.LoadedUnits[i]);
    end;
    FModule.LoadedUnits.Free;
    FModule.LoadedUnits := newSt;
  end;
begin
  Expect(tkImplementation);
  ParseImplementSection;
  Expect(tkDot);

  FCurParent := nil;
  FCurStates := [];
  LeaveScope; // pop global symbols

  // 检查全部的声明是否都有实现
  CheckForward;

  if FErrorCount = 0 then
  begin
    CleanupSym;
    CleanupUnit;
  end;
end;

procedure TParser.ParseUnitInterface(M: TModule);

  procedure AddNameScopes(M: TModule);
  var
    I: Integer;
    Ns, Prev: TNameScope;
  begin
    if Length(M.Names) = 0 then
    begin
      CurSymbols.AutoAddToOwner := False;
      AddSymbol(M);
      CurSymbols.AutoAddToOwner := True;
    end
    else begin
      Prev := nil;
      for I := 0 to High(M.Names) do
      begin
        Ns := TNameScope(CreateElement(TNameScope));
        Ns.Name := M.Names[I];
        if I = 0 then
          AddSymbol(Ns) else
          Prev.Add(Ns.Name, Ns);
        Prev := Ns;
      end;
    end;
  end;

begin
{
<Unit>			::= <UnitHeader>  <InterfaceSection> <ImplementationSection> <InitSection> '.'
<UnitHeader>		::= UNIT <QualId> <OptPortDirectives> ';'
}
  if CurToken <> tkUnit then NextToken;
  Expect(tkUnit);
  NextToken; // skip 'unit'
  FModule := M;
  FModule.TimeStamp := FScanner.TimeStamp;

  // 分析单元名
  ParseQualifiedId;
  if FQId.CountOfNames = 1 then
    FModule.Name := FQID.Name
  else
    FModule.SetNameScope(FQID.Names, FQID.CountOfNames);
  FQID.Reset;

  if CurToken <> tkSemicolon then
  begin
    FModule.Hints := ParseHints;
    Expect(tkSemicolon);
  end;

  FContext.FModules.Add(FModule);

  if FIsSystemUnit then
    if not SameText('System', FModule.Name) then
      ParseError(SErr_SystemUnitNameMismatch, True);

  Expect(tkSemicolon);
  NextToken;
  Expect(tkInterface);

  if FIsSystemUnit then
  begin
    FModule := FContext.FSystemUnit;
  end;

  EnterScope(FModule.Symbols);
  AddNameScopes(FModule);
  if not FIsSystemUnit then
  begin
    FContext.LoadSystemUnit;
    FModule.Symbols.AutoAddToOwner := False;
    AddSymbols(FContext.FSystemUnit);
    FModule.Symbols.AutoAddToOwner := True;
    FModule.LoadedUnits.Add(FContext.FSystemUnit);
  end;

  FCurParent := FModule;
  SetTempExpr(True);
  ParseInterfaceSection;

  Expect(tkImplementation);
end;

procedure TParser.ParseUsesClause;
var
  M: TModule;
  Coord: TAstNodeCoord;
begin
  if FIsSystemUnit then
    ParseError(SErr_SystemUnitUsesOthers, True);
{
<UsesClause>		::= USES <IdList> ';'
			  | SynError
}
  NextToken; // skip 'uses'

  // TODO 5:  暂不考虑 uses语句中的 in
  while True do
  begin
    Expect(tkIdentifier);
    M := FContext.LoadUnit(CurTokenString);
    if (M.State in [msIntfCompiling, msLoading]) or SameText(FModule.Name, CurTokenString) then
    begin
      Coord.Row := FScanner.CurRow;
      Coord.Col := FScanner.CurColumn;
      Coord.FileName := FScanner.CurFileName;
      ParseError(Coord, SErr_CircularUnitReference, [FModule.Name, CurTokenString], True);
    end;

    FModule.LoadedUnits.Add(M);
    if Self.FInternalSection then
      Include(M.Attr, saInternal)
    else
      Exclude(M.Attr, saInternal);
    Self.AddSymbols(M);
    NextToken;
    if CurToken <> tkComma then Break;
    NextToken; // skip tkComma
  end;
  Expect(tkSemicolon);
  NextToken;
end;

procedure TParser.ParseVarSection(Parent: TSymbol);

// todo 1: 有问题,有时候需要返回TMultiAccessor
  function ParseAbsVar: TSymbol;

    function FindField(Typ: TType; const S: string): TSymbol;
    begin
      case Typ.TypeCode of
        typRecord: Result := TRecordType(Typ).FindSymbol(S);
        typObject: Result := TObjectType(Typ).FindSymbol(S);
        typClass: Result := TClassType(Typ).FindSymbol(S);
      else
        Result := nil;
      end;
    end;
  var
    I: Integer;
    Sym: TSymbol;
  begin
  {
    type
      TMyD = record
        a: string[20];
      end;
      PMyd = ^TMyd;
    var
      D: TmyD;
      P: PMyd = @D;
      StrLen1: Byte absolute D.a;   // ok
      StrLen2: Byte absolute D; // ok
      StrLen3: Byte absolute P.a;   // error
  }
    ParseQualifiedId;
    if FQId.CountOfNames = 1 then
      Result := FindSymbol(FQId.Name)
    else begin
      Sym := FindSymbol(FQId.Name);
      for I := 1 to FQID.CountOfNames - 1 do
      begin
        if not Assigned(Sym) then Break;

        case Sym.NodeKind of
          nkModule: Sym := TModule(Sym).FindSymbol(FQId.Names[I]);
          nkNameScope: Sym := TNameScope(Sym).FindSymbol(FQId.Names[I]);
          nkVariable: begin
            Sym := FindField(TVariable(Sym).VarType, FQId.Names[I]);
            if Assigned(Sym) then
              if Sym.NodeKind <> nkField then
              begin
                ParseError(SErr_FieldRequired);
                Sym := nil;
              end;
          end;
          nkType: begin
            Sym := FindField(TType(Sym), FQId.Names[I]);
            if Assigned(Sym) then
              if Sym.NodeKind <> nkField then
              begin
                ParseError(SErr_FieldRequired);
                Sym := nil;
              end
              else if not (saStatic in TField(Sym).Attr) then
              begin
                ParseError(SErr_StaticRequired);
                Sym := nil;
              end;
          end;
        else
          Sym := nil;
        end;
        if Sym = nil then Break;
        if not IsVisible(FCurParent, Sym) then
          ParseError(SErr_SymbolNotAccess, [Sym.Name]);
      end;
      if Assigned(Sym) then
        if Sym.NodeKind in [nkVariable, nkField] then
        begin
          ParseError(SErr_VarRequired);
          Sym := nil;
        end;
      Result := Sym;
    end;
  end;
var
  Typ: TType;
  Variable, V1: TVariable;
  E: TExpr;
  HasVal: Boolean;
  IsTls: Boolean;
  Hints: TMemberHints;
  StateInfo: TParseStateInfo;
begin
{
<VarSection>		::= VAR <VarDeclList>

<VarDeclList>		::= <VarDecl>
			  | <VarDeclList>  <VarDecl>	

<VarDecl>		::= <IdList> ':' <Type> <OptAbsoluteClause> <OptPortDirectives> ';'
			  | <IdList> ':' <Type> '=' <TypedConstant> <OptPortDirectives> ';'
			  | <IdList> ':' <TypeSpec> 
			  | SynError ';'
}
  // current token is tkType
  IsTls := CurToken = tkThreadVar;
  NextToken; // skip 'var' / 'threadvar'
  Expect(tkIdentifier);

  StateSet(psInVar, StateInfo);
  while CurToken = tkIdentifier do
  begin
    // parse variable list
    Variable := TVariable(ParseIdList(TVariable));

    Expect(tkColon);
    NextToken; // skip ':'

    Typ := ParseTypeDecl;
    Hints := [];
    if CurToken = tkAbsolute then
    begin
      if IsTls then
        ParseError('Thread local variables cannot be ABSOLUTE', True);
      if Variable.Next <> nil then
        ParseError(SErr_AbsoluteVarList, True);
      NextToken;
      Variable.AbsVar := ParseAbsVar;
      if CurToken = tkEqual then
        ParseError(SErr_InitAbsoluteVar);
    end
    else
//    if CurToken = tkIdentifier then // parse hint
      Hints := ParseHints;

    HasVal := False;
    if CurToken = tkEqual then
    begin
      if IsTls then
        ParseError('Cannot initialize thread local variables', True);
    // todo 2: 要考虑数组和记录常量
      ValClear(FTempValue);
      // parse init value
      NextToken;
      E := ParseExpr;
      HasVal := CheckExpr(E);
      if HasVal then
      begin
        FTempValue := EvalExpr(E);
        if not CheckAssignmentCompatibility(Typ, E.Typ) then
          ParseError(SErr_AssignIncomp);
        // todo 1:对于string要转换
        {if Typ.TypeCode = typWideString then
          if FTempValue.VT = vtStr then
            FTempValue}
      end;
    end;

    V1 := Variable;
    repeat
      if HasVal then
        V1.Value := ValCopy(FTempValue);

      V1.VarType := Typ;
      V1.Hints := Hints;
      V1.Visibility := Self.FCurVisibility;

      if IsTls then Include(V1.VarAttr, vaTls);

      if Parent.NodeKind in [nkMethod, nkFunc] then
        Include(V1.VarAttr, vaLocal);

      AddSymbol(V1);
      if Parent.NodeKind in [nkFunc, nkMethod] then
        V1.Level := TFunction(Parent).Level;
      V1 := TVariable(V1.Next);
    until V1 = nil;

    ValClear(FTempValue);
    Expect(tkSemicolon);
    NextToken;
  end;
  StateRestore(StateInfo);
end;

function TParser.ParseWhileStmt: TWhileStmt;
var
  StateInfo: TParseStateInfo;
begin
  NextToken; // skip 'while'
  Result := TWhileStmt(CreateStmt(TWhileStmt));
  Result.Condition := ParseExpr;
  CheckBoolExpr(Result.Condition);
  Expect(tkDo);
  NextToken;
  StateSet(psInWhileStmt, StateInfo);
  Result.Stmt := ParseStatement(Result);
  StateRestore(StateInfo);
end;

function TParser.ParseWithStmt(Parent: TStatement): TCompoundStmt;

  function GetUniqueLocalName: string;
  var
    I: Integer;
  begin
    I := 1;
    repeat
      Result := Format('$with%d', [I]);
      if Self.CurSymbols.Find(Result) = nil then Break;
      Inc(I);
    until False;
  end;

var
  E: TExpr;
  Deepth: Integer;
  V: TVariable;
  AssStmt: TAssignmentStmt;
  Stmt: TStatement;
begin
  NextToken; // skip 'with'
  Deepth := FWithList.Count;
  Result := TCompoundStmt(CreateStmt(TCompoundStmt));
  repeat
    E := ParseExpr;
    if CurToken <> tkDo then
    begin
      Expect(tkComma);
      NextToken;
    end;

    {
      with getobj(a,b) do
        dosome;
      ==> 扩展成:
      $with1 = getobj(a,b);
      $with1.dosome;
    }
    // 要考虑如果E是无效的,应该如何继续分析
    // 处理: E.Typ = TObject
    if not CheckExpr(E) then
      E.Typ := FContext.FTObjectType
    else if not (E.Typ.TypeCode in [typClass, typInterface,
        typDispInterface, typRecord, typObject]) then
    begin
      E.Typ := FContext.FTObjectType;
      ParseError(SErr_ExpectStructType);
    end;

    if E.OpCode = opSYMBOL then // 这时E指向一个Var/const
      EnterWithStmt(TSymbolExpr(E))
    else begin
      // 新建一个局部变量
      // 如果是指向record,object类型的,只操作指针
      V := TVariable(CreateElement(TVariable));
      V.Name := GetUniqueLocalName;
      if E.OpCode = opINST then
        V.VarType := TUnaryExpr(E).Operand.Typ
      else if E.Typ.TypeCode in [typRecord, typObject] then
      begin
        E.Typ.CreatePointerType(FPointerSize);
        V.VarType := E.Typ.PointerType;
      end
      else
        V.VarType := E.Typ;

      Include(V.VarAttr, vaHidden);
      AddSymbol(V);

      // 然后新建一个赋值语句,(如果E已经是无效,那赋值也是无效的)
      AssStmt := TAssignmentStmt(CreateStmt(TAssignmentStmt));
      AssStmt.Left := CreateSymbolExpr(V.Name);
      AssStmt.Left.Typ := V.VarType;
      TSymbolExpr(AssStmt.Left).Reference := V;

      EnterWithStmt(TSymbolExpr(AssStmt.Left));

      if E.OpCode = opINST then
      begin
        E := TUnaryExpr(E).Operand;
        E.Parent := nil;
        AssStmt.Right := E;
      end
      else if E.Typ.TypeCode in [typRecord, typObject] then
      begin
        AssStmt.Right := CreateUnaryExpr(opADDR, E);
        AssStmt.Right.Typ := V.VarType;
      end
      else
        AssStmt.Right := E;
      AssStmt.Parent := Result;
      Result.Statements.Add(AssStmt);
    end;

  until CurToken = tkDo;

  NextToken; // skip 'do'
  Stmt := ParseStatement(Result);
  if Stmt <> nil then
    Result.Statements.Add(Stmt);

  // leave scope
  while Deepth < FWithList.Count do
    LeaveWithStmt;
end;

procedure TParser.SetTempExpr(Value: Boolean);
begin
  if Value then
    FCurExprList := FTempExprList
  else
    FCurExprList := FExprList;
end;

function TParser.SimplifyQualId: TExpr;

  function CloneSymbolExpr(Source: TSymbolExpr): TSymbolExpr;
  begin
    Result := CreateSymbolExpr(Source.Name);
    TSymbolExpr(Result).Reference := Source.Reference;
    TSymbolExpr(Result).Typ := Source.Typ;
    Result.Coord := Source.Coord;
  end;
var
  Sym: TSymbol;
  Prefix: TSymbolExpr;
  I: Integer;
  InValid: Boolean;
begin
  // 简化如System.SysUtils.TStrings,这种完全限定标识
  // 直接取TStrings
  // 如果 System.SysUtils.TStrings 标识符无效,则不简化

  if FindWith(FQId.Names[0], Prefix, Sym) then // 在with语句中
  begin
    Prefix := CloneSymbolExpr(Prefix);
    if Prefix.Typ.TypeCode = typPointer then
    begin
    // dereference
      Result := CreateUnaryExpr(opINST, Prefix);
      Result.Typ := TPointerType(Prefix.Typ).RefType;
    //  Include(Result.Attr, eaVerified);
    end
    else
      Result := Prefix;
    // concat with Sym
    for I := 0 to FQId.CountOfNames-1 do
      Result := CreateBinaryExpr(opMEMBER, Result, CreateSymbolExpr(FQId.Names[I]));
  end
  else
  begin
    Sym := FindSymbol(FQId.Names[0]);
    I := 1;
    while Assigned(Sym) and (I < FQId.CountOfNames) do
    begin
      case Sym.NodeKind of
        nkModule: Sym := TModule(Sym).FindSymbol(FQId.Names[I]);
        nkNameScope: Sym := TNameScope(Sym).FindSymbol(FQId.Names[I]);
      else
        Break;
      end;
      Inc(I);
    end;

    Result := nil;
    InValid := False;
    // 如果标识符未指向有效符号, 则不精简表达式, 但标记为无效
    if (Sym = nil) or (Sym.NodeKind in [nkModule, nkNameScope])
        or (I > FQId.CountOfNames) then
    begin
    // 不报错，等CheckExpr
    {  if Sym = nil then
        ParseError(SErr_UndeclaredIdent, [FQId.Names[I-1]])
      else
        ParseError(SErr_InvalidIdent, [Sym.Name]);}
      I := 0;
      InValid := True;
    end
    else
      Dec(I);

    while I < FQId.CountOfNames do
    begin
      if Result = nil then
      begin
        Result := CreateSymbolExpr(FQId.Names[I]);
        if not Invalid then
          TSymbolExpr(Result).Reference := Sym;
      end
      else
        Result := CreateBinaryExpr(opMEMBER, Result, CreateSymbolExpr(FQId.Names[I]));
      Inc(I);
    end;
    if Invalid then
      Include(Result.Attr, eaInvalid);
  end;
end;

procedure TParser.StateClear(State: TParseState;
  out StateInfo: TParseStateInfo);
begin
  StateInfo.IsSet := State in FCurStates;
  StateInfo.State := State;
  Exclude(FCurStates, State);
end;

procedure TParser.StateRestore(const StateInfo: TParseStateInfo);
begin
  if StateInfo.IsSet then
    Include(FCurStates, StateInfo.State)
  else
    Exclude(FCurStates, StateInfo.State);
end;

procedure TParser.StateSet(State: TParseState;
  out StateInfo: TParseStateInfo);
begin
  StateInfo.IsSet := State in FCurStates;
  StateInfo.State := State;
  Include(FCurStates, State);
end;

function TParser.StmtAdjust(Stmt: TStatement): TStatement;
var
  E: TExpr;
  Sym, Ret: TSymbol;

  procedure StmtAdjust_Exit;
  var
    AsgStmt: TAssignmentStmt;
  begin
    Ret := FCurFunction.LocalSymbols.Find('Result');
    Assert(Ret <> nil, 'StmtAdjust,Result is nil');
    Assert(Ret.NodeKind = nkVariable, 'StmtAdjust,Result not var');
    AsgStmt := TAssignmentStmt(CreateStmt(TAssignmentStmt));
    AsgStmt.Left := Self.CreateSymbolExpr(Ret.Name);
    TSymbolExpr(AsgStmt.Left).Reference := Ret;
    AsgStmt.Left.Typ := TVariable(Ret).VarType;
    AsgStmt.Right := TUnaryExpr(TBinaryExpr(E).Right).Operand;
    AsgStmt.Right.Parent := nil;
    TBinaryExpr(E).Right := nil;
    Result := CreateStmt(TCompoundStmt);
    Result.Parent := Stmt.Parent;
    TCompoundStmt(Result).Statements.Add(AsgStmt);
    TCompoundStmt(Result).Statements.Add(Stmt);
    AsgStmt.Parent := Result;
    Stmt.Parent := Result;
  end;
begin
  Result := Stmt;
  if Stmt.StmtKind = skCallStmt then
  begin
  // 如果是Exit(0)，则调整为:
  // Result := 0;
  // Exit;
  // Inc(i,1) : i := i + 1;
    E := TCallStmt(Stmt).CallExpr;
    if (E <> nil) and (E.OpCode = opCALL) then
    begin
      Sym := TBinaryExpr(E).Left.GetReference;
      if (Sym <> nil) and (Sym.NodeKind = nkBuiltinFunc)
      //  and (TBuiltinFunction(Sym).Kind = bfExit)
        and (TBinaryExpr(E).Right <> nil)
        and (TUnaryExpr(TBinaryExpr(E).Right).Operand <> nil) then
      begin
        case TBuiltinFunction(Sym).Kind of
          bfExit: StmtAdjust_Exit;
//          bfInc: StmtAdjust_Inc;
//          bfDec: StmtAdjust_Dec;
        end;
      end;
    end;
  end;
end;

procedure TParser.UngetToken;
begin
  if FTokenBufferIndex = 0 then
    raise EParserError.Create(SErr_UngetTokenError, Scanner.CurFileName, 0, 0)
  else begin
    Dec(FTokenBufferIndex);
    if FTokenBufferIndex > 0 then begin
      FCurToken := FTokenBuffer[FTokenBufferIndex - 1];
      FCurTokenString := FTokenStringBuffer[FTokenBufferIndex - 1];
    end else begin
      FCurToken := tkWhitespace;
      FCurTokenString := '';
    end;
  end;
end;

{ TFunctionHeader }

constructor TFunctionHeader.Create;
begin
  Args := TList.Create;
  Args.Capacity := 20;
  SetLength(Names, 10);
end;

destructor TFunctionHeader.Destroy;
begin
  Args.Free;
  inherited;
end;

procedure TFunctionHeader.Reset;
var
  i: Integer;
begin
  Name := '';
  for i := 0 to CountOfNames - 1 do
    Names[i] := '';
  CountOfNames := 0;
  FileName := '';
  RoutineName := '';
  ImplementingName := '';
  RoutineNo := 0;
  MsgNo := 0;
  ReturnType := nil;
  Args.Count := 0;
  Directives := [];
  Modifiers := [];
  CallConvention := ccDefault;
  MethodKind := mkNormal;
  ObjectKind := okClass;
end;

{ TQualifiedID }

constructor TQualifiedID.Create;
begin
  SetLength(Names, 10);
end;

function TQualifiedID.Id: string;
var
  I: Integer;
begin
  Result := Name;
  for I := 1 to CountOfNames - 1 do
  begin
    if I = 1 then Result := Result + '.';
    Result := Result + Names[I];
  end;
end;

procedure TQualifiedID.Reset;
var
  I: Integer;
begin
  Name := '';
  for I := 0 to CountOfNames - 1 do
    Names[I] := '';
  CountOfNames := 0;
end;

function TQualifiedID.SameScope(const Scopes: array of string): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Scopes) do
  begin
    if (I >= Length(Names)) or not SameText(Scopes[I], Names[I]) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

end.
