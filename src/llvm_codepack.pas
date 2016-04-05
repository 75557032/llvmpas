unit llvm_codepack;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface
uses Classes, SysUtils, ast, cntx, hashtable, inst;

type
  {
  特殊类型: string,interface,dynamicarray,variant,set,record
  这些类型需要自动清理或者尺寸太大不能直接返回。

   1.调整Variant, string操作为函数调用
   2.对于某些内部函数的调用调整为实际函数的调用
   3.用Jump语句取代if, while, repeat, for，
   4.对于返回特殊类型的函数，需要分配变量来接收这些返回值。
   5.把property展开成直接存取字段或函数调用。
   6.把finally 分解成嵌套函数。
   7.处理 except 块：编译成 if E is EXxxxx then 之类形式。
   8.处理 构造函数 中调用另一个构造函数进行初始化的问题
   9.字符串字面量、Set字面量转成TConstant
  }

  TCode = class
  public
    Cmds: TList;
    Vars: TList; // TFuncParam, TVariable
//    Consts: THashTable; // TConstant
    Funcs: TList;
    VarCleanFunc: TFunction;
  public
    constructor Create;
    destructor Destroy; override;
  end;

//  TCmd = TExpr;
//  TUnaryOp = TUnaryExpr;

  { TCodePack }

  TCodePack = class
  private
    FFunc: TFunction;     // 顶层函数(level=0)
    FCurFunc: TFunction;  // 当前处理的函数。
    FContext: TCompileContext;
    FCurCode: TCode;
    FStrConstList, FSetConstList: THashTable;
//    FCleanupList: TList;
    FCleanupStack: TList;
    FCleanupIndex: Integer;
    FExceptCount: Integer; // 异常处理嵌套个数，处于最外层为1
    //FExprStmts: TPtrHashtable;
    FNodes: TList;
    FVarID, FFuncID, FLabelID, FLevelID: Integer;
    FSelfSym, FResultSym: TSymbol;
    FSelfTypeSym: TSymbol;
    FExceptVar, FExPtrVar: TVariable;
    FExceptVarID: Integer;
    FExceptVarStack: TList;
    //FSetBytesType: TOpenArrayType;
    //FByteSetType: TSetType;
    FBreakLabel, FContinueLabel, FQuitLabel: string;

    procedure CopyAttr(Op: TBaseOp; E: TExpr);
    function CopyOp(E: TBaseOp): TBaseOp;
    function CreateOpFrom(E: TExpr): TBaseOp;

    function CreateListOp(const Coord: TAstNodeCoord; const Args: array of TBaseOp; Start: Integer = 0): TListOp; overload;
    function CreateListOp(const Coord: TAstNodeCoord): TListOp; overload;
    function CreateListOpFrom(E: TExpr): TListOp;

    function CreateUnaryOp(Op: TOpCode; const Coord: TAstNodeCoord): TUnaryOp;
    function CreateUnaryOpFrom(E: TUnaryExpr): TUnaryOp;

    function CreateBinaryOp(Op: TOpCode; const Coord: TAstNodeCoord): TBinaryOp;
    function CreateBinaryOpFrom(E: TBinaryExpr): TBinaryOp;

    function CreateSymbolOp(Sym: TSymbol; const Coord: TAstNodeCoord): TSymbolOp;
    function CreateSymbolOpFrom(E: TSymbolExpr): TSymbolOp;

    function CreateConstOp(const Coord: TAstNodeCoord): TConstOp; overload;
    function CreateConstOp(const Coord: TAstNodeCoord; V: Integer): TConstOp; overload;
    function CreateConstOp(const Coord: TAstNodeCoord; V: Int64): TConstOp; overload;
    function CreateConstOp(const Coord: TAstNodeCoord; const V: TValueRec): TConstOp; overload;
    function CreateConstOpFrom(E: TConstExpr): TConstOp;

    function CreateFunc: TFunction;
    function CreateCmd(Cmd: TCmdClass; const Coord: TAstNodeCoord): TCmd;

    function CreateNode(NodeClass: TAstNodeClass): TAstNode;

    procedure CreateExPtrVar;
    function CreateExVar: TVariable;

    procedure RemoveNode(Node: TObject);
    procedure AddRawArgs;

    procedure SetupStmt(Stmt: TStatement);
    procedure SetupStmt_Assign(Stmt: TAssignmentStmt);
    procedure SetupStmt_Try(Stmt: TTryStmt);
    procedure SetupStmt_If(Stmt: TIfStmt);
    procedure SetupStmt_While(Stmt: TWhileStmt);
    procedure SetupStmt_Repeat(Stmt: TRepeatStmt);
    procedure SetupStmt_For(Stmt: TForStmt);
    procedure SetupStmt_Call(Stmt: TCallStmt);
    procedure SetupStmt_Raise(Stmt: TRaiseStmt);
    procedure SetupStmt_Case(Stmt: TCaseStmt);

    function EmitExpr(E: TExpr): TBaseOp;
    function EmitExpr_StrCmp(E: TBinaryExpr): TBaseOp;
    function EmitExpr_VarCmp(E: TBinaryExpr): TBinaryOp;
    function EmitExpr_StrAdd(E: TBinaryExpr): TBaseOp;
    function EmitExpr_VarOp(E: TBinaryExpr): TBaseOp;
    function EmitExpr_VarArrGet(E: TBinaryExpr): TBaseOp;
    function EmitExpr_UnaryVarOp(E: TUnaryExpr): TBaseOp;
    function EmitExpr_SetOp(E: TBinaryExpr): TBaseOp;
    function EmitExpr_Set(E: TUnaryExpr): TBaseOp;
    // 取函数/方法的指针
    function EmitExpr_ProcAddr(E: TUnaryExpr): TBaseOp;
    // 处理成员访问。
    function EmitExpr_Member(E: TBinaryExpr): TBaseOp;
    // 处理符号表达式。
    function EmitExpr_Symbol(E: TSymbolExpr): TBaseOp;
    // 处理常量
    function EmitExpr_Const(E: TConstExpr): TBaseOp;
    // 处理调用。AsgOp用于存贮返回值 有可能返回nil
    function EmitExpr_Call(E: TBinaryExpr; AsgOp: TBaseOp = nil): TBaseOp;
    // 处理调用。 有可能返回nil
    function EmitExpr_Builtin(E: TBinaryExpr; Sym: TBuiltinFunction): TBaseOp;
    function EmitExpr_In(E: TBinaryExpr): TBaseOp;
    function EmitExpr_Is(E: TBinaryExpr): TBaseOp;
    function EmitExpr_As(E: TBinaryExpr): TBaseOp;
    function EmitCallOp(CallOp: TBinaryOp; AsgOp: TBaseOp = nil): TBaseOp;

    // 把E转成接口。AsgOp用于存贮结果
    procedure EmitIntfCastOp(E: TBinaryExpr; AsgOp: TBaseOp);

    // 把Obj转成接口。Obj.Typ必须是typClass
    function EmitObj2Intf(Obj: TBaseOp; Intf: TInterfaceType; DoCast: Boolean = False): TBaseOp; overload;
    procedure EmitObj2Intf(Obj, AsgOp: TBaseOp; Intf: TInterfaceType; DoCast: Boolean = False); overload;

    function EmitStrCmp(E: TBinaryOp): TBaseOp;
    function EmitStrAddOp(E: TBinaryExpr): TSymbolOp; overload;
    // 处理字符串相加。
    procedure EmitStrAddOp(E: TBinaryExpr; AsgOp: TBaseOp); overload;
    procedure EmitStrAsg(E, Dest: TBaseOp);
    // 发出一个函数调用。AsgOp用于存贮返回值（特殊类型如Variant,string等）
    procedure EmitCallSpecial(E: TBinaryExpr; AsgOp: TBaseOp);
    // 对表达式E调用一次 _VarCopy，结果存于Dest。 将使用E和Dest
    procedure EmitVarCopy(E, Dest: TBaseOp); overload;
    // 对表达式E调用一次 _VarCopy，结果存于一临时变量，返回这个变量的表达式
    // 将使用表达式E
    function EmitVarCopy(E: TBaseOp): TSymbolOp; overload;

    // 对表达式E调用_VarFromXXX转换Variant，结果存于Dest。
    // 如果E已经是Variant，或ToOle=True并且E已经是OleVariant，则不处理。
    // 注意：将移动E和Dest的位置
    procedure EmitCastToVar(E, Dest: TBaseOp; ToOle: Boolean = False); overload;
    // 对表达式E调用_VarFromXXX转换Variant，结果存于一个临时变量，返回这个变量的表达式
    function EmitCastToVar(E: TBaseOp; ToOle: Boolean = False): TSymbolOp; overload;

    // 转换字符串类型。 转换的结果存入临时变量，并返回这个变量的表达式
    // typ是目标类型. strAnsi..strWShort
    function EmitCastToStr(E: TBaseOp; typ: TStringType): TSymbolOp; overload;
    // 转换字符串类型。 Dest是可被赋值的表达式，转换的结果存入此表达式中
    // typ是目标类型. strAnsi..strWShort
    // 注意：将移动E和Dest的位置
    procedure EmitCastToStr(E, Dest: TBaseOp; typ: TStringType); overload;

    procedure EmitCastToDynArray(E, Dest: TBaseOp); overload;
    function EmitCastToDynArray(E: TBaseOp; dynTyp: TType): TBaseOp; overload;

    function EmitOAInitStmt(const Coord: TAstNodeCoord; ElemType: TType;
      const Args: array of TBaseOp; Start: Integer = 0): TSymbolOp;
    procedure EmitMarkCmd(const Lab: string; const Coord: TAstNodeCoord);
    procedure EmitGotoCmd(const Target: string; const Coord: TAstNodeCoord);
    procedure EmitCallCmd(Func: TFunctionDecl; const Coord: TAstNodeCoord;
      const Args: array of TBaseOp);

//    function EmitProcAddr(E: TExpr): TBaseOp;

    // 创建一个Call表达式
    function CreateCall(Func: TFunctionDecl; const Coord: TAstNodeCoord;
      const Args: array of TBaseOp): TBinaryOp;
    // 添加字符串常量到列表，相同的字符串和类型不会再次添加
    function AddStrConst(const s: UTF8String; typ: TType): TConstant;
    function AddSetConst(V: TSetValue): TVariable;
    function AddVar(T: TType): TVariable;
    procedure AddFunc(F: TFunction);
    procedure AddCmd(Cmd: TCmd);
    procedure AttachStmt(E: TBaseOp; Start: Integer);
    procedure AddNode(Node: TObject);
    function LabelStr(const prefix: string = 'L.'): string;
    procedure SetupFunc(F: TFunction);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Setup(Acntx: TCompileContext; Func: TFunction);
    procedure Dump(List : TStrings);
    property StrConstList: THashTable read FStrConstList write FStrConstList;
    property SetConstList: THashTable read FSetConstList write FSetConstList;
  end;

function IsSpecialType(typ: TType): Boolean;

implementation

procedure Err(const S: string); overload;
begin
  raise ECompileContextError.Create('TCodePack.' + S);
end;

procedure Err(const S: string; const Args: array of const); overload;
begin
  raise ECompileContextError.CreateFmt('TCodePack.' + S, Args);
end;

// 是否特殊类型.
// 这种类型当返回值,不能直接返回,需要由调用方提供接收地址
function IsSpecialType(typ: TType): Boolean;
begin
  typ := typ.OriginalType;
  Result := typ.TypeCode in [typString, typRecord, typObject, typVariant,
      typArray, typDynamicArray, typInterface];
  if not Result then
    Result := ((typ.TypeCode = typSet) and (typ.Size > 4))
//      or ((typ.TypeCode = typProcedural) and (TProceduralType(typ).IsMethodPointer));
end;

function IsStructType(T: TType): Boolean;
const
  StructTypes = [typString, typVariant, 
    typRecord, typObject, typArray, typSet, typOpenArray]; // set如果小于4字节不当成结构类型

begin
  T := T.OriginalType;
  case T.TypeCode of
    typSet: Result := T.Size > 4;
//    typProcedural: Result := T.IsMethodPointer;
    typString: Result := TStringType(T).Kind in [strWShort, strAShort];
  else
    Result := T.TypeCode in StructTypes;
  end;
end;

function NeedFree(T: TType): Boolean;
begin
  case T.TypeCode of
    typArray: Result := staNeedInit in TArrayType(T).ArrayAttr;
    typRecord: Result := staNeedInit in TRecordType(T).RecordAttr;
    typString: Result := not (TStringType(T).Kind in [strAShort, strWShort]);
  else
    Result := T.TypeCode in AutoFreeTypes;
  end;
end;

const  // 遵从System.pas的定义
  opVarAdd =        0;
  opVarSub =        1;
  opVarMul =        2;
  opVarDiv =        3;
  opVarIntDiv =     4;
  opVarMod =        5;
  opVarSHL =        6;
  opVarSHR =        7;
  opVarAnd =        8;
  opVarOr =         9;
  opVarXor =        10;
  opVarCompare =    11;
  opVarNeg =        12;
  opVarNot =        13;

  opVarCmpEQ =      14;
  opVarCmpNE =      15;
  opVarCmpLT =      16;
  opVarCmpLE =      17;
  opVarCmpGT =      18;
  opVarCmpGE =      19;

  VarOpMaps: array[opNE..opSHR] of Integer = (
//  opNE,       opEQ,       opLT,       opLE,       opGT,       opGE,       opIN, opIS, opAS,
    opVarCmpNE, opVarCmpEQ, opVarCmpLT, opVarCmpLE, opVarCmpGT, opVarCmpGE, -1,   -1,   -1,
//  opADD,    opSUB,    opOR,     opXOR,
    opVarAdd, opVarSub, opVarOR, opVarXor,
//  opMUL,    opFDIV,   opIDIV,      opMOD,    opAND,    opSHL,    opSHR,
    opVarMul, opVarDiv, opVarIntDiv, opVarMod, opVarAnd, opVarSHL, opVarSHR
  );

{ TCodePack }

procedure TCodePack.AddCmd(Cmd: TCmd);
begin
  FCurCode.Cmds.Add(Cmd);
end;

procedure TCodePack.AddFunc(F: TFunction);
begin
  FCurCode.Funcs.Add(F);
end;

procedure TCodePack.AddNode(Node: TObject);
begin
  FNodes.Add(Node);
end;

procedure TCodePack.AddRawArgs;
begin
  //if FFunction.NodeKind = nkMethod then
//  begin
//  end;
end;

function TCodePack.AddSetConst(V: TSetValue): TVariable;
var
  s: string;
begin
  if V = nil then V := TSetValue.Create;

  s := V.AsString;
  Result := TVariable(FSetConstList.Get(s));
  if Result = nil then
  begin
    Result := TVariable.Create;
    FSetConstList.Put(s, Result);
    Result.Parent := FFunc.Module;
    Result.VarType := FContext.FByteSetType;
    Result.Name := 'set.' + IntToStr(FSetConstList.Count);
    Include(Result.Attr, saInternal);
    Include(Result.Attr, saTemp);
    ValFromSet(Result.Value, V);
    V := nil;
  end;
  if V <> nil then V.Free;
end;

function TCodePack.AddStrConst(const s: UTF8String; typ: TType): TConstant;
var
  p: THashtablePosition;
  List: THashTable;
  c: TConstant;
begin
  List := FStrConstList;
  p := List.FindFirst(s);
  repeat
    c := List.FindNext(p);
    if (c <> nil) and c.ConstType.Equals(typ) then
    begin
      Result := c;
      Exit;
    end;
  until p = nil;

  Result := TConstant.Create;
  List.Put(S, Result);
//  AddNode(Result); //不需要加入，因为StrConstList是外面传入的
  case typ.TypeCode of
    typPWideChar: Result.Name := 'const.pw.';
    typPAnsiChar: Result.Name := 'const.pa.';
    typString:
      case TStringType(typ).Kind of
        strAnsi: Result.Name := 'const.as.';
        strWide: Result.Name := 'const.ws.';
        strUnicode: Result.name := 'const.us.';
        strAShort: Result.Name := 'const.sas.';
        strWShort: Result.Name := 'const.sws.';
      else
        Assert(False);
      end;
  else
    Assert(False);
  end;

  Result.Parent := FFunc.Module;
  Result.ConstType := typ;
  Include(Result.Attr, saInternal);
  Include(Result.Attr, saTemp);
  Result.Name := Result.Name + IntToStr(List.Count);
  ValFromStr(Result.Value, S);
end;

function TCodePack.AddVar(T: TType): TVariable;
begin
  Inc(FVarID);
  Result := TVariable.Create;
  Result.Name := Format('.V%d', [FVarID]);
  Result.VarType := T;
  Result.Parent := FCurFunc;
  Include(Result.VarAttr, vaHidden);
  Include(Result.VarAttr, vaLocal);
//  Include(Result.States, vsNestRef);
  Include(Result.Attr, saTemp);
  Include(Result.Attr, saUsed);
  AddNode(Result);
  FCurCode.Vars.Add(Result);
end;

procedure TCodePack.AttachStmt(E: TBaseOp; Start: Integer);
var
  i: Integer;
  FStmts: TList;
  S: TCmd;
begin
  if E = nil then Exit;
  FStmts := FCurCode.Cmds;
  if Start >= FStmts.Count then Exit;
//  if E.OpCode <> opcSymbol then
//  begin
//    Assert(False);
//  end;

  for i := Start to FStmts.Count - 1 do
  begin
    S := TCmd(FStmts[i]);
    E.AddCmd(S);
  end;

  for i := FStmts.Count - 1 downto Start do
    FStmts.Delete(i);
end;

procedure TCodePack.CopyAttr(Op: TBaseOp; E: TExpr);
begin
  if eaConst in E.Attr then
    Include(Op.Attr, oprConst);
  if eaVarCast in E.Attr then
    Include(Op.Attr, oprVarCast);
  if eaInherited in E.Attr then
    Include(Op.Attr, oprInherited);
end;

function TCodePack.CopyOp(E: TBaseOp): TBaseOp;
var
  T: TClass;
begin
  T := E.ClassType;
  if T = TBinaryOp then
  begin
    Result := CreateBinaryOp(E.OpCode, E.Coord);
  end
  else if T = TUnaryOp then
    Result := CreateUnaryOp(E.OpCode, E.Coord)
  else if T = TSymbolOp then
    Result := CreateSymbolOp(TSymbolOp(E).Reference, E.Coord)
  else if T = TConstOp then
  begin
    Result := CreateConstOp(E.Coord);
    ValCopy(TConstOp(Result).Value, TConstOp(E).Value);
  end
  else begin
    Err('Unknown type of inst');
    Result := nil;
  end;
  Result.Typ := E.Typ;
  Result.Switches := E.Switches;
  Result.Attr := E.Attr;
end;

constructor TCodePack.Create;
begin
//  FCleanupList := TList.Create;
  FCleanupStack := TList.Create;
  FNodes := TList.Create;
  FNodes.Capacity := 64;
end;

function TCodePack.CreateBinaryOp(Op: TOpCode;
  const Coord: TAstNodeCoord): TBinaryOp;
begin
  Result := TBinaryOp.Create;
  Result.OpCode := Op;
  Result.Coord := Coord;
  AddNode(Result);
end;

function TCodePack.CreateBinaryOpFrom(E: TBinaryExpr): TBinaryOp;
begin
  Result := CreateBinaryOp(OpMap(E.OpCode), E.Coord);
  Result.Typ := E.Typ;
  Result.Switches := E.Switches;
  CopyAttr(Result, E);
end;

function TCodePack.CreateCall(Func: TFunctionDecl;
  const Coord: TAstNodeCoord; const Args: array of TBaseOp): TBinaryOp;
begin
  Result := CreateBinaryOp(opcCALL, Coord);
  Result.Left := CreateSymbolOp(Func, Coord);
  Result.Right := CreateListOp(Coord, Args);
  if Func.ReturnType = nil then
    Result.Typ := FContext.FUntype
  else
    Result.Typ := Func.ReturnType.NormalType;
end;

function TCodePack.CreateCmd(Cmd: TCmdClass;
  const Coord: TAstNodeCoord): TCmd;
begin
  Result := Cmd.Create;
  Result.Coord := Coord;
  AddNode(Result);
end;

function TCodePack.CreateConstOp(
  const Coord: TAstNodeCoord): TConstOp;
begin
  Result := TConstOp.Create;
  Result.Coord := Coord;
  AddNode(Result);
end;

function TCodePack.CreateConstOp(const Coord: TAstNodeCoord; V: Int64
  ): TConstOp;
begin
  Result := CreateConstOp(Coord);
  Result.Typ := FContext.FInt64Type;
  ValFromInt(Result.Value, V);
end;

function TCodePack.CreateConstOp(const Coord: TAstNodeCoord;
  V: Integer): TConstOp;
begin
  Result := CreateConstOp(Coord);
  Result.Typ := FContext.FIntegerType;
  ValFromInt(Result.Value, V);
end;

function TCodePack.CreateConstOp(const Coord: TAstNodeCoord;
  const V: TValueRec): TConstOp;
begin
  Result := CreateConstOp(Coord);

  case V.VT of
    vtInt: Result.Typ := FContext.FIntegerType;
    vtInt64: Result.Typ := FContext.FInt64Type;
    vtReal: Result.Typ := FContext.FDoubleType;
    vtBool: Result.Typ := FContext.FBooleanType;
    vtStr, vtWStr: Result.Typ := FContext.FUnicodeStringType;
    vtAChr: Result.Typ := FContext.FAnsiCharType;
    vtWChr: Result.Typ := FContext.FWideCharType;
  else
    Err('CreateConstExpr');
  end;
  ValCopy(Result.Value, V);
end;

function TCodePack.CreateConstOpFrom(E: TConstExpr): TConstOp;
begin
  Result := CreateConstOp(E.Coord);
  Result.Typ := E.Typ;
  CopyAttr(Result, E);
  Result.Switches := E.Switches;
  ValCopy(Result.Value, E.Value);
end;

procedure TCodePack.CreateExPtrVar;
var
  LExSelVar: TVariable;
begin
  if FExPtrVar = nil then
  begin
    FExPtrVar := TVariable.Create;
    AddNode(FExPtrVar);
    FExPtrVar.VarType := FContext.FPointerType;
    FExPtrVar.Name := '$exptr';
    FExPtrVar.Level := FCurFunc.Level;
    FExPtrVar.VarAttr := [vaLocal, vaHidden];
    Include(FExPtrVar.Attr, saUsed);
  //  FCurFunc.LocalSymbols.Add(FExPtrVar);
    FCurCode.Vars.Add(FExPtrVar);

    LExSelVar := TVariable.Create;
    AddNode(LExSelVar);
    LExSelVar.VarType := FContext.FIntegerType;
    LExSelVar.Name := '$exsel';
    LExSelVar.Level := FCurFunc.Level;
    LExSelVar.VarAttr := [vaLocal, vaHidden];
    Include(LExSelVar.Attr, saUsed);
  //  FCurFunc.LocalSymbols.Add(LExSelVar);
    FCurCode.Vars.Add(LExSelVar);
  end;
end;

function TCodePack.CreateExVar: TVariable;
begin
  Inc(FExceptVarID);
  Result := TVariable(CreateNode(TVariable));

  Result.VarType := FContext.FTObjectType;
  Result.Name := '$ex' + IntToStr(FExceptVarID);
  Result.Level := FCurFunc.Level;
  Result.VarAttr := [vaLocal, vaHidden];
  Include(Result.Attr, saUsed);
//  FCurFunc.LocalSymbols.Add(Result);
  FCurCode.Vars.Add(Result);
end;

function TCodePack.CreateFunc: TFunction;
begin
  Result := TFunction.Create;
  AddNode(Result);
end;

function TCodePack.CreateListOp(const Coord: TAstNodeCoord;
  const Args: array of TBaseOp; Start: Integer): TListOp;
var
  i: Integer;
begin
  Result := CreateListOp(Coord);
  Result.Typ := FContext.FUntype;
  for i := Start to High(Args) do
  begin
    TListOp(Result).Add(Args[i]);
  end;
end;

function TCodePack.CreateListOp(const Coord: TAstNodeCoord): TListOp;
begin
  Result := TListOp.Create;
  Result.Coord := Coord;
  AddNode(Result);
end;

function TCodePack.CreateListOpFrom(E: TExpr): TListOp;
begin
  Result := CreateListOp(E.Coord);
  Result.Typ := E.Typ;
  Result.Switches := E.Switches;
  CopyAttr(Result, E);
end;

function TCodePack.CreateNode(NodeClass: TAstNodeClass): TAstNode;
begin
  Result := NodeClass.Create;
  AddNode(Result);
end;

function TCodePack.CreateOpFrom(E: TExpr): TBaseOp;
begin
  case OpKinds[E.OpCode] of
    opkUnary: Result := CreateUnaryOpFrom(TUnaryExpr(E));
    opkBinary: Result := CreateBinaryOpFrom(TBinaryExpr(E));
    opkConst: Result := CreateConstOpFrom(TConstExpr(E));
    opkSymbol: Result := CreateSymbolOpFrom(TSymbolExpr(E));
    opkList: Result := CreateListOpFrom(TListExpr(E));
  else
    Assert(False, 'CreateOpFrom');
    Result := nil;
  end;
end;

function TCodePack.CreateSymbolOp(Sym: TSymbol;
  const Coord: TAstNodeCoord): TSymbolOp;
begin
  Result := TSymbolOp.Create;
  Result.Coord := Coord;
  Result.Reference := Sym;
  if Sym <> nil then
  begin
    Result.Name := Sym.Name;
    case Sym.NodeKind of
      nkVariable: Result.Typ := TVariable(Sym).VarType;
      nkConstant: Result.Typ := TConstant(Sym).ConstType;
      nkFuncParam: Result.Typ := TFuncParam(Sym).ParamType;
      nkType: Result.Typ := TType(Sym);
      nkField: Result.Typ := TField(Sym).FieldType;
      nkProperty: Result.Typ := TProperty(Sym).PropType;
      nkIntfProperty: Result.Typ := TIntfProperty(Sym).PropType;
    else
      Result.Typ := FContext.FUntype;
    end;
  end;
  AddNode(Result);
end;

function TCodePack.CreateSymbolOpFrom(E: TSymbolExpr): TSymbolOp;
begin
  Result := CreateSymbolOp(nil, E.Coord);
  Result.Reference := E.Reference;
  Result.Name := E.Name;
  Result.Typ := E.Typ;
  Result.Switches := E.Switches;
  CopyAttr(Result, E);
end;

function TCodePack.CreateUnaryOp(Op: TOpCode;
  const Coord: TAstNodeCoord): TUnaryOp;
begin
  Result := TUnaryOp.Create;
  Result.OpCode := Op;
  Result.Coord := Coord;
  AddNode(Result);
end;

function TCodePack.CreateUnaryOpFrom(E: TUnaryExpr): TUnaryOp;
begin
  Result := CreateUnaryOp(OpMap(E.OpCode), E.Coord);
  Result.Typ := E.Typ;
  Result.Switches := E.Switches;
  CopyAttr(Result, E);
end;

destructor TCodePack.Destroy;
var
  i: Integer;
begin
//  FCleanupList.Free;
  FCleanupStack.Free;
  FExceptVarStack.Free;
  for i := 0 to FNodes.Count-1 do
    TObject(FNodes[i]).Free;
  FNodes.Free;
  inherited Destroy;
end;

procedure TCodePack.Dump(List: TStrings);
begin

end;

procedure TCodePack.EmitCallCmd(Func: TFunctionDecl;
  const Coord: TAstNodeCoord; const Args: array of TBaseOp);
var
  S: TCallCmd;
begin
  S := TCallCmd(CreateCmd(TCallCmd, Coord));
  S.CallOp := CreateCall(Func, Coord, Args);
  AddCmd(S);
end;

function TCodePack.EmitCallOp(CallOp: TBinaryOp; AsgOp: TBaseOp): TBaseOp;

  // 检查这两个符号是否 TClassType ，并且处于同一继承分枝，
  // p1和p2等同或者p2在分枝上更靠近根部
  function IsSameBranch(p1, p2: TSymbol): Boolean;
  begin
    Result := (p1.NodeKind = nkType) and (p2.NodeKind = nkType)
      and (TType(p1).TypeCode = typClass)
      and (TType(p2).TypeCode = typClass)
      and ((p1 = p2) or
           (TClassType(p1).IsInheritedFrom(TClassType(p2)))
           );
  end;

  function IsSelf(Op: TBaseOp): Boolean;
  var
    sym: TSymbol;
  begin
    // 在 EmitExpr_Symbol 中已经调整了。访问当前类成员必定添加 Self
    Result := (Op.OpCode = opcMember)
            and (TBinaryOp(Op).Left.OpCode = opcSymbol);
    if Result then
    begin
      Sym := TSymbolOp(TBinaryOp(Op).Left).Reference;
      result := (sym.NodeKind = nkVariable) and
                (vaSelf in TVariable(sym).VarAttr);
    end;
  end;

  // 判断是否在构造器中调用另一个构造器
  function IsCallCtorInCtor(L: TBaseOp; Fun: TFunction; Sym: TSymbol): Boolean;
  begin
    Result := (Fun.NodeKind = nkMethod)
        and (TMethod(Fun).MethodKind = mkConstructor)
        and (Sym <> nil)
        and (Sym.NodeKind = nkMethod)
        and (TMethod(Sym).MethodKind = mkConstructor)
        and IsSelf(L)
        and IsSameBranch(Fun.Parent, Sym.Parent);
  end;

  function IsCallDtorInDtor(L: TBaseOp; Fun: TFunction; Sym: TSymbol): Boolean;
  begin
    Result := (Fun.NodeKind = nkMethod)
        and (TMethod(Fun).MethodKind = mkDestructor)
        and (Sym <> nil)
        and (Sym.NodeKind = nkMethod)
        and (TMethod(Sym).MethodKind = mkDestructor)
        and IsSelf(L)
        and IsSameBranch(Fun.Parent, Sym.Parent);
  end;

var
  Sym: TSymbol;
  I, ParamCount: Integer;
  LParams: TFuncParamList;
  LParam: TFuncParam;
  LRetType: TType;
  CallArgs: TListOp;
  Op: TBaseOp;
  V: TVariable;
  IsSafecall: Boolean;
  Cmd: TCmd;
begin
  Sym := CallOp.Left.GetReference;
  if (Sym = nil) or (sym.NodeKind = nkVariable) then
  begin
    if not CallOp.Left.Typ.IsProcedural then Err('EmitCallOp: not procedural');

    with TProceduralType(CallOp.Left.Typ) do
    begin
      IsSafecall := CallConvention = ccSafecall;
      LRetType := ReturnType;
      LParams := Params;
    end;
  end
  else if Sym.NodeKind in [nkFunc, nkMethod, nkExternalFunc] then
  begin
    IsSafecall := TFunctionDecl(Sym).CallConvention = ccSafecall;
    LRetType := TFunctionDecl(Sym).ReturnType;
    LParams := TFunctionDecl(Sym).Params;
  end
  else
  begin
    Err('EmitCallOp: Invalid Sym Kind');
    Result := nil;
    Exit;
  end;

  if LParams = nil then
    ParamCount := 0 else
      ParamCount := LParams.Count;

  CallArgs := TListOp(CallOp.Right);
  for I := 0 to ParamCount - 1 do
  begin
    LParam := LParams[i];
    if I >= CallArgs.Count then
      Op := nil
    else
      Op := CallArgs.Items[I];
    // to Variant, String, Intf, dynarray
    if Op <> nil then
    begin
      case LParam.ParamType.TypeCode of
        typVariant:
          if Op.Typ.TypeCode <> typVariant then
          begin
            Op := EmitCastToVar(Op, TVariantType(LParam.ParamType).IsOle);
            CallArgs.Replace(I, Op);
          end;
        typString:
          if not Op.Typ.Equals(LParam.ParamType) then
          begin
            Op := EmitCastToStr(Op, TStringType(LParam.ParamType));
            CallArgs.Replace(I, Op);
          end;
        typDynamicArray:
          if Op.Typ.TypeCode <> typDynamicArray then
          begin
            Op := EmitCastToDynArray(Op, Op.Typ);
            CallArgs.Replace(I, Op);
          end;
        typInterface:
          if not Op.Typ.Equals(LParam.ParamType) then
          begin
            Assert(False, 'Need impl');
          //  Op := EmitCastToIntf(Op, LParam.ParamType);
          end;
      end;
    end;
  end;

  if LRetType <> nil then LRetType := LRetType.NormalType;

  if LRetType = nil then
    CallOp.Typ := FContext.FUntype
  else
    CallOp.Typ := LRetType;

  // 检查 ctor 中调用另一个 ctor
  if IsCallCtorInCtor(CallOp.Left, FFunc, Sym) then
  begin
    Include(CallOp.Attr, oprCtorInner);
  end;

  if IsCallDtorInDtor(CallOp.Left, FFunc, Sym) then
  begin
    Include(CallOp.Attr, oprDtorInner);
  end;

  Result := CallOp;
  if Assigned(LRetType) and (IsSpecialType(LRetType) or IsSafecall) then
  begin
    CallOp.OpCode := opcCallSpecial;

    // 添加返回参数
    V := nil;
    if AsgOp = nil then
    begin
      V := AddVar(LRetType);
      CallArgs.Add(CreateSymbolOp(V, CallArgs.Coord));
    end
    else
      CallArgs.Add(AsgOp);

    CallOp.Detach;
    Cmd := CreateCmd(TCallCmd, CallOp.Coord);
    TCallCmd(Cmd).CallOp := CallOp;
    AddCmd(Cmd);

    if V <> nil then
      Result := CreateSymbolOp(V, CallArgs.Coord)
    else
      Result := nil;
  end;
end;

procedure TCodePack.EmitCallSpecial(E: TBinaryExpr; AsgOp: TBaseOp);
begin
  Assert(E.OpCode = opCALL);
  EmitExpr_Call(E, AsgOp);
{  newE := CreateBinaryOpFrom(E);
  newE.Left := EmitExpr(E.Left);
  newE.Right := EmitExpr(E.Right);
  EmitExpr_Call2(newE, AsgOp);}
end;

procedure TCodePack.EmitCastToDynArray(E, Dest: TBaseOp);
var
  Fun: TFunctionDecl;
begin
  Assert(E.Typ.TypeCode = typVariant);
  Assert(E.Typ.TypeCode = typDynamicArray);
  Fun := FContext.GetSystemRoutine(srVar2DynArr);
  Self.EmitCallCmd(Fun, E.Coord, [Dest, E]);
end;

function TCodePack.EmitCastToDynArray(E: TBaseOp; dynTyp: TType): TBaseOp;
var
  v: TVariable;
begin
  v := AddVar(dynTyp);
  Result := CreateSymbolOp(V, E.Coord);
  EmitCastToDynArray(E, CreateSymbolOp(V, E.Coord));
end;

procedure TCodePack.EmitCastToStr(E, Dest: TBaseOp; typ: TStringType);
const
//                   Source,       Dest
  strCastMaps: array[TStringKind, TStringKind] of TSystemRoutine = (
//           strAnsi,          strWide,         strUnicode,      strAShort,      strWShort
{strAnsi}    (srTrunc,         srWStrFromAStr,  srUStrFromAStr,  srSStrFromAStr, srRound),
{strWide}    (srAStrFromWStr,  srTrunc,         srUStrFromWStr,  srSStrFromWStr, srRound),
{strUnicode} (srAStrFromUStr,  srWStrFromUStr,  srTrunc,         srSStrFromUStr, srRound),
{strAShort}  (srAStrFromSStr,  srWStrFromSStr,  srUStrFromSStr,  srSStrAsg,      srRound),
{strWShort}  (srRound,         srRound,         srRound,         srRound,        srRound)
  );
  paCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,       strWide,        strUnicode,     strAShort,      strWShort
    srAStrFromPACh, srWStrFromPACh, srUStrFromPACh, srSStrFromPACh, srRound
  );
  pwCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,       strWide,        strUnicode,     strAShort,      strWShort
    srAStrFromPWCh, srWStrFromPWCh, srUStrFromPWCh, srSStrFromPWCh, srRound
  );
  aarrCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,         strWide,          strUnicode,      strAShort,        strWShort
    srAStrFromAArray, srWStrFromAArray, srUStrFromAArray, srSStrFromAArray, srRound
  );
  warrCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,         strWide,          strUnicode,      strAShort,        strWShort
    srAStrFromWArray, srWStrFromWArray, srUStrFromWArray, srSStrFromWArray, srRound
  );
  achrCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,      strWide,       strUnicode,    strAShort,     strWShort
    srAStrFromACh, srWStrFromACh, srUStrFromACh, srSStrFromACh, srRound
  );
  wchrCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,      strWide,       strUnicode,    strAShort,     strWShort
    srAStrFromWCh, srWStrFromWCh, srUStrFromWCh, srSStrFromWCh, srRound
  );
  varCastMaps: array[TStringKind] of TSystemRoutine = (
  // strAnsi,   strWide,    strUnicode, strAShort,  strWShort
    srVar2AStr, srVar2WStr, srVar2UStr, srVar2SStr, srRound
  );
  CastableTypes = [typString, typVariant, typPAnsiChar, typPWideChar,
                         typChar];
var
  Inst: TCallCmd;
  Func: TFunctionDecl;
  Routine: TSystemRoutine;
  Arg1, Arg2, Arg3, Arg4: TBaseOp;
begin
  if typ.TypeCode <> typString then
    Err('EmitCastToStr: Invalid typ');

  if E.Typ.Equals(typ) then Exit;

  if not (E.Typ.TypeCode in CastableTypes) and not E.Typ.IsPackedString then
    Err('EmitCastToStr: Invalid typ of E');

  if Dest.Typ.TypeCode <> typString then
    Err('EmitCastToStr: Invalid Dest type');
  if TStringType(Dest.Typ).Kind <> typ.Kind then
    Err('EmitCastToStr: Dest.Typ <> typ');

  case E.Typ.TypeCode of
    typString:
      Routine := strCastMaps[TStringType(E.Typ).Kind, typ.Kind];
    typVariant:
      Routine := varCastMaps[typ.Kind];
    typPAnsiChar:
      Routine := paCastMaps[typ.Kind];
    typPWideChar:
      Routine := pwCastMaps[typ.Kind];
    typArray:
      if E.Typ.IsPackedStringAnsi then
        Routine := aarrCastMaps[typ.Kind]
      else
        Routine := warrCastMaps[typ.Kind];
    typChar:
      if TCharType(E.Typ).Kind = charAnsi then
        Routine := achrCastMaps[typ.Kind]
      else
        Routine := wchrCastMaps[typ.Kind];
  else
    Routine := srRound;
  end;

  if Routine = srRound then
    Err('EmitCastToStr: Invalid dest string type');

  Arg1 := nil; Arg2 := nil; Arg3 := nil; Arg4 := nil;
  case E.Typ.TypeCode of
    typString, typPAnsiChar, typPWideChar, typChar:
      if (typ.Kind = strAShort) then
      begin
        // procedure _SStrAsg(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
        // procedure _SStrFromXXX(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
        Arg1 := Dest;
        Arg2 := CreateConstOp(E.Coord, typ.CharCount);
        Arg3 := E;
      end
      else
      begin
        // XXXFromYYY(var Dest: XXX; const Source: YYY);
        Arg1 := Dest;
        Arg2 := E;
      end;
    typVariant:
      if typ.Kind = strAShort then
      begin
        // procedure _Var2SStr(const V: TVarData; var Dest: ShortString; MaxChars: Integer);
        Arg1 := E;
        Arg2 := Dest;
        Arg3 := CreateConstOp(E.Coord, typ.CharCount);
      end
      else
      begin
        // procedure _Var2xxx(const V: TVarData; var Dest: xxx);
        Arg1 := E;
        Arg2 := Dest;
      end;
    typArray:
      if typ.Kind = strAShort then
      begin
//procedure _SStrFromAArray(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar; Count: Integer);
//procedure _SStrFromWArray(var Dest: ShortString; MaxChars: Integer; Buf: PWideChar; Count: Integer);
        Arg1 := Dest;
        Arg2 := CreateConstOp(E.Coord, typ.CharCount);
        Arg3 := E;
        Arg4 := CreateConstOp(E.Coord, TArrayType(E.Typ).ElementCount);
      end
      else
      begin
// procedure _XXXFromAArray(var Dest: UnicodeString; Buf: PAnsiChar; Count: Integer);
// procedure _UStrFromWArray(var Dest: UnicodeString; Buf: PWideChar; Count: Integer);
        Arg1 := Dest;
        Arg2 := E;
        Arg3 := CreateConstOp(E.Coord, TArrayType(E.Typ).ElementCount);
      end;
  end;

  Func := FContext.GetSystemRoutine(Routine);
  Inst := TCallCmd(CreateCmd(TCallCmd, E.Coord));
  if Arg4 <> nil then
    Inst.CallOp := CreateCall(Func, E.Coord, [Arg1, Arg2, Arg3, Arg4])
  else if (Arg3 <> nil) then
    Inst.CallOp := CreateCall(Func, E.Coord, [Arg1, Arg2, Arg3])
  else
    Inst.CallOp := CreateCall(Func, E.Coord, [Arg1, Arg2]);
  AddCmd(Inst);
end;

function TCodePack.EmitCastToStr(E: TBaseOp; typ: TStringType): TSymbolOp;
var
  S: TSymbolOp;

  function CastStrConst(C: TConstant): TSymbolOp;
  begin
    C := AddStrConst(ValToStr(C.Value), typ);
    Result := CreateSymbolOp(C, E.Coord);
  end;

begin
  if (E.OpCode = opcSymbol)
    and (TSymbolOp(E).Reference <> nil)
    and (TSymbolOp(E).Reference.NodeKind = nkConstant) then
  begin
    Result := CastStrConst(TConstant(TSymbolOp(E).Reference));
    Exit;
  end;
  S := CreateSymbolOp(AddVar(typ), E.Coord);
  Result := CreateSymbolOp(S.Reference, S.Coord);
  EmitCastToStr(E, S, typ);
end;

procedure TCodePack.EmitCastToVar(E, Dest: TBaseOp; ToOle: Boolean);

const
  RoutineFromInt: array[TIntKind] of TSystemRoutine =
  (
    srVarFromShortint, srVarFromByte, srVarFromSmallint, srVarFromWord,
    srVarFromLongint, srVarFromLongWord, srVarFromInt64, srVarFromUInt64
  );
  RoutineFromStr: array[TStringKind] of TSystemRoutine =
  (
    srVarFromAStr, srVarFromWStr, srVarFromUStr, srVarFromSStr, srVarFromSWStr
  );
var
  Inst: TCallCmd;
  CastFunc: TFunctionDecl;
  Routine: TSystemRoutine;
begin
  if E.Typ.TypeCode = typVariant then
  begin
    if not (ToOle and not TVariantType(E.Typ).IsOle) then
    begin
      Exit;
    end;
  end;

  if Dest.Typ.TypeCode <> typVariant then
    Err('EmitCastToVar: Invalid Dest type');

  Inst := TCallCmd(CreateCmd(TCallCmd, E.Coord));

  case E.Typ.TypeCode of
    typInt:
      Routine := RoutineFromInt[TIntType(E.Typ).Kind];
    typNumeric:
      Routine := srVarFromReal;
    typBool:
      Routine := srVarFromBool;
    typChar:
      if TCharType(E.Typ).Kind = charAnsi then
        Routine := srVarFromAChr
      else
        Routine := srVarFromWChr;
    typPAnsiChar:
      Routine := srVarFromPAChr;
    typPWideChar:
      Routine := srVarFromPWChr;
    typString:
      Routine := RoutineFromStr[TStringType(E.Typ).Kind];
    typVariant:  // 这时E.Typ的IsOle=False，前面已经检查过
      Routine := srOleVarFromVar;
    typDynamicArray:
      Routine := srVarFromDynArr;
  else
    Exit;
  end;

  if ToOle then
    case Routine of
      srVarFromPAChr: Routine := srOleVarFromPAChr;
      srVarFromPWChr: Routine := srOleVarFromPWChr;
      srVarFromAStr: Routine := srOleVarFromAStr;
      srVarFromWStr: Routine := srOleVarFromWStr;
      srVarFromUStr: Routine := srOleVarFromUStr;
      srVarFromSStr: Routine := srOleVarFromSStr;
    end;

  CastFunc := fContext.GetSystemRoutine(Routine);
  Inst.CallOp := CreateCall(CastFunc, E.Coord, [Dest, E]);
  AddCmd(Inst);
end;

function TCodePack.EmitCastToVar(E: TBaseOp; ToOle: Boolean): TSymbolOp;
var
  V: TVariable;
  S: TSymbolOp;
begin
  if ToOle then
    V := AddVar(FContext.FOleVariantType)
  else
    V := AddVar(FContext.FVariantType);
  S := CreateSymbolOp(V, E.Coord);
  Result := CreateSymbolOp(V, E.Coord);
  EmitCastToVar(E, S);
end;

function TCodePack.EmitExpr(E: TExpr): TBaseOp;

  procedure CopyList(E: TListExpr; List: TListOp);
  var
    N: TBaseOp;
    I: Integer;
  begin
    List.SetCapacity(E.Count);
    for I := 0 to E.Count - 1 do
    begin
      N := EmitExpr(E.Items[I]);
      List.Add(N);
    end;
  end;

  function EmitExpr_Bin(E: TBinaryExpr): TBinaryOp;
  begin
    Result := CreateBinaryOpFrom(E);
    Result.Left := EmitExpr(E.Left);
    Result.Right := EmitExpr(E.Right);
  end;

  function EmitExpr_Un(E: TUnaryExpr): TUnaryOp;
  begin
    Result := CreateUnaryOpFrom(E);
    Result.Operand := EmitExpr(E.Operand);
  end;

var
  Old: Integer;
begin
  Old := FCurCode.Cmds.Count;
  case E.OpCode of
    opNE..opGE:
      if eaStrOp in E.Attr then
        Result := EmitExpr_StrCmp(TBinaryExpr(E))
      else if eaVarOp in E.Attr then
        Result := EmitExpr_VarCmp(TBinaryExpr(E))
      else if eaSetOp in E.Attr then
        Result := EmitExpr_SetOp(TBinaryExpr(E))
      else
        Result := EmitExpr_Bin(TBinaryExpr(E));
    opIN: Result := EmitExpr_In(TBinaryExpr(E));
    opIS: Result := EmitExpr_Is(TBinaryExpr(E));
    opAS: Result := EmitExpr_As(TBinaryExpr(E));

    opADD, opSUB, opOR, opXOR, opMUL, opFDIV,
    opIDIV, opMOD, opAND, opSHL, opSHR:
      if eaStrOp in E.Attr then
        Result := EmitExpr_StrAdd(TBinaryExpr(E))
      else if eaVarOp in E.Attr then
        Result := EmitExpr_VarOp(TBinaryExpr(E))
      else if eaSetOp in E.Attr then
        Result := EmitExpr_SetOp(TBinaryExpr(E))
      else
        Result := EmitExpr_Bin(TBinaryExpr(E));

    opMEMBER:
      Result := EmitExpr_Member(TBinaryExpr(E));
    opCAST:
      if E.Typ.TypeCode = typVariant then
        Result := Self.EmitCastToVar(EmitExpr(TBinaryExpr(E).Right), TVariantType(E.Typ).IsOle)
      else
        Result := EmitExpr_Bin(TBinaryExpr(E));

    opCALL: Result := EmitExpr_Call(TBinaryExpr(E));

    opINDEX:
      if eaArrayProp in E.Attr then
      begin
        Result := EmitExpr(TBinaryExpr(E).Left);
        Assert(Result.OpCode = opcCALL);
        Assert(TBinaryExpr(E).Right <> nil);
        CopyList(TListExpr(TBinaryExpr(E).Right),
                  TListOp(TBinaryOp(Result).Right));
        Result := EmitCallOp(TBinaryOp(Result));
      end
      else if eaVarOp in E.Attr then
        Result := EmitExpr_VarArrGet(TBinaryExpr(E))
      else
        Result := EmitExpr_Bin(TBinaryExpr(E));
    opNOT, opNEG:
      if eaVarOp in E.Attr then
        Result := EmitExpr_UnaryVarOp(TUnaryExpr(E))
      else
        Result := EmitExpr_Un(TUnaryExpr(E));

    opPOS: Result := EmitExpr(E);
    //opINHERITED: Result := EmitExpr_Call(TBinaryExpr(E));
    opSET: Result := EmitExpr_Set(TUnaryExpr(E));
    opADDR:
      if TUnaryExpr(E).Operand.IsFunction then
//      if E.Typ.TypeCode = typProcedural then
      // todo 1: 有问题，类型有可能是pointer
      // 解决方法：在ExprAttr中增加一个标志，指示这个Expr取函数指针
        Result := EmitExpr_ProcAddr(TUnaryExpr(E))
      else
        Result := EmitExpr_Un(TUnaryExpr(E));

    opINST: Result := EmitExpr_Un(TUnaryExpr(E));

    opNIL, opCONST:
      Result := Self.EmitExpr_Const(TConstExpr(E));

    opSYMBOL: Result := Self.EmitExpr_Symbol(TSymbolExpr(E));
    opLIST:
      begin
        Result := CreateListOpFrom(E);
        CopyList(TListExpr(E), TListOp(Result));
      end
  else
    Assert(False);
    Result := nil;
  end;
  AttachStmt(Result, Old);
end;

function TCodePack.EmitExpr_As(E: TBinaryExpr): TBaseOp;
var
  Fun: TFunctionDecl;
begin
  if E.Left.Typ.TypeCode = typInterface then
  begin
    if E.Right.Typ.TypeCode = typInterface then
    begin
      Fun := FContext.GetSystemRoutine(srIntfCast);
      //Result := CreateCall(Fun, E.Coord, [EmitExpr(E.Left), EmitExpr(E.Right)
    end;
  end;
  Fun := FContext.GetSystemRoutine(srAsClass);
  Result := CreateCall(Fun, E.Coord, [EmitExpr(E.Left), EmitExpr(E.Right)]);
  //Result.Typ := E.Typ;
end;

function TCodePack.EmitExpr_Builtin(E: TBinaryExpr; Sym: TBuiltinFunction
  ): TBaseOp;

  procedure EmitExit;
  var
    I: Integer;
    S: TCmd;
  begin
    // 退出之前执行finally块代码
    for I := FCleanupStack.Count - 1 downto 0 do
    begin
      EmitCallCmd(TFunction(FCleanupStack[I]), E.Coord, []);
    end;

    // 退出之前执行异常终止指令
    for I := 0 to FExceptCount - 1 do
    begin
      S := CreateCmd(TEndExceptCmd, E.Coord);
      // 退出之前释放 exobj
      Assert(I < FExceptVarStack.Count, 'FExceptVarStack');
      TEndExceptCmd(S).ExceptVar := TVariable(FExceptVarStack[I]).Name;
      AddCmd(S);
    end;

    // 带参数的Exit
    if TListExpr(E.Right).Count > 0 then
    begin
      S := CreateCmd(TAssignCmd, E.Coord);
      TAssignCmd(S).Left := CreateSymbolOp(FResultSym, E.Coord);
    //  TListOp(E.Right).Delete(0);
      TAssignCmd(S).Right := EmitExpr(TListExpr(E.Right).Items[0]);
      AddCmd(S);
    end;

    EmitGotoCmd(FQuitLabel, E.Coord);
  end;

  procedure EmitContinue;
  var
    I: Integer;
    S: TCmd;
  begin
    for I := FCleanupStack.Count - 1 downto FCleanupIndex do
    begin
      EmitCallCmd(TFunction(FCleanupStack[I]), E.Coord, []);
    end;
    // 退出之前执行异常终止指令
    for I := 0 to FExceptCount - 1 do
    begin
      S := CreateCmd(TEndExceptCmd, E.Coord);
      // 退出之前释放 exobj
      Assert(I < FExceptVarStack.Count, 'FExceptVarStack');
      TEndExceptCmd(S).ExceptVar := TVariable(FExceptVarStack[I]).Name;
      AddCmd(S);
    end;
    EmitGotoCmd(FContinueLabel, E.Coord);
  end;

  procedure EmitBreak;
  var
    I: Integer;
    S: TCmd;
  begin
    for I := FCleanupStack.Count - 1 downto FCleanupIndex do
    begin
      EmitCallCmd(TFunction(FCleanupStack[I]), E.Coord, []);
    end;
    // 退出之前执行异常终止指令
    for I := 0 to FExceptCount - 1 do
    begin
      S := CreateCmd(TEndExceptCmd, E.Coord);
      // 退出之前释放 exobj
      Assert(I < FExceptVarStack.Count, 'FExceptVarStack');
      TEndExceptCmd(S).ExceptVar := TVariable(FExceptVarStack[I]).Name;
      AddCmd(S);
    end;
    EmitGotoCmd(FBreakLabel, E.Coord);
  end;

  function EmitAssigned: TBinaryOp;
  begin
    Result := CreateBinaryOp(opcEQ, E.Coord);
    Assert(E.Right.OpCode=opLIST);
    Result.Left := EmitExpr(TListExpr(E.Right).Items[0]);
    Result.Right := CreateConstOp(Result.Left.Coord);
    Result.Right.OpCode := opcNIL;
    Result.Right.Typ := FContext.FPointerType;
    Result.Typ := FContext.FBooleanType;
  end;

  procedure EmitSizeOf;
  begin
    Assert(TListExpr(E.Right).Count > 0, 'EmitSizeOf');
    Result := CreateConstOp(E.Coord, TListExpr(E.Right).Items[0].Typ.Size);
  end;
begin
  Result := nil;
  case Sym.Kind of
    bfAssigned: Result := EmitAssigned;

    bfExit: EmitExit;

    bfContinue: EmitContinue;

    bfBreak: EmitBreak;

    bfSizeOf: EmitSizeOf;
  else
    Result := CreateBinaryOp(opcCallBuiltin, E.Coord);
    TBinaryOp(Result).Left := EmitExpr(E.Left);
    TBinaryOp(Result).Right := EmitExpr(E.Right);
    TBinaryOp(Result).Typ := E.Typ;
  end;
end;

function TCodePack.EmitExpr_Call(E: TBinaryExpr; AsgOp: TBaseOp): TBaseOp;
var
  Sym: TSymbol;
begin
  // 添加Sepcial参数到末尾。
  Sym := E.Left.GetReference;
  if (Sym <> nil) and (Sym.NodeKind = nkBuiltinFunc) then
  begin
    Result := EmitExpr_Builtin(E, TBuiltinFunction(Sym));
    Exit;
  end;

  Result := CreateBinaryOp(opcCALL, E.Coord);
  Result.Switches := E.Switches;
  CopyAttr(Result, E);
  if (E.Left.OpCode = opSymbol) and (eaInherited in E.Left.Attr) then
    Include(Result.Attr, oprInherited);
  TBinaryOp(Result).Left := EmitExpr(E.Left);
  TBinaryOp(Result).Right := EmitExpr(E.Right);

  Result := EmitCallOp(TBinaryOp(Result), AsgOp);
end;

function TCodePack.EmitExpr_Const(E: TConstExpr): TBaseOp;
var
  C: TConstant;
  s: Utf8String;
begin
  case E.Value.VT of
    vtStr, vtWStr:
      begin
        s := ValToStr(E.Value);
        C := AddStrConst(s, E.Typ);
        Result := CreateSymbolOp(C, E.Coord);
      end;
    // 不会出现vtSet
    //vtSet:
  else
    Result := CreateConstOpFrom(E);
  end;
end;

function TCodePack.EmitExpr_In(E: TBinaryExpr): TBaseOp;

  {function EmitBuildSet(List: TListExpr): TBaseOp;
  var
    i: Integer;
    Op: TBaseOp;
    Elem: TExpr;
    IsConst: Boolean;
    MaxEl, MinEl: Byte;
  begin
    for i := 0 to List.Count-1 do
    begin
      Elem := List.Items[i];
      if not (eaConst in Elem.Attr) then
      begin
        IsConst := False;
        Break;
      end;
    end;

    if IsConst then
    begin

    end;
  end;  }
var
  RT: TType;
  V: TValueRec;
  Op1, Op2, Op3: TBaseOp;
  Fun: TFunctionDecl;
begin

  RT := E.Right.Typ;
  Assert(RT.TypeCode = typSet);

  if E.Right.OpCode = opSET then
  begin
    if TListExpr(TUnaryExpr(E.Right).Operand).Count = 0 then
    begin
      ValInit(V);
      ValFromBool(V, False);
      Result := CreateConstOp(E.Coord, V);
      Exit;
    end;

    //Op2 := EmitBuildSet(TListExpr(E.Right));
    Op2 := EmitExpr(E.Right);
  end
  else
    Op2 := EmitExpr(E.Right);
  Op1 := EmitExpr(E.Left);
  Op3 := CreateConstOp(E.Coord, Op2.Typ.Size);
  Fun := FContext.GetSystemRoutine(srSetIn);
  // function _SetIn(var S; Size, Elem: Byte): Boolean;
  Result := CreateCall(Fun, E.Coord, [Op2, Op3, Op1]);
end;

function TCodePack.EmitExpr_Is(E: TBinaryExpr): TBaseOp;
var
  Fun: TFunctionDecl;
begin
  Fun := FContext.GetSystemRoutine(srIsClass);
  Result := CreateCall(Fun, E.Coord, [EmitExpr(E.Left), EmitExpr(E.Right)]);
  //Result.Typ := E.Typ;
end;

function TCodePack.EmitExpr_Member(E: TBinaryExpr): TBaseOp;
var
  L, R, Leaf: TBaseOp;
begin
  case E.Left.OpCode of
    opMEMBER: L := EmitExpr_Member(TBinaryExpr(E.Left));
    opSYMBOL: L := EmitExpr_Symbol(TSymbolExpr(E.Left));
  else
    L := EmitExpr(E.Left);
  end;

  Assert(E.Right.OpCode = opSYMBOL, 'EmitExpr_Member: Right is not symbol');

  R := EmitExpr_Symbol(TSymbolExpr(E.Right));
  case R.OpCode of
    opcSYMBOL:
      begin
        Result := CreateBinaryOpFrom(E);
        TBinaryOp(Result).Left := L;
        TBinaryOp(Result).Right := R;
      end;
    opcCALL:
      begin
        //Result := R;
        // 这时不可能有多级访问方法调用的，而且也不会在右边添加Self
        Assert(TBinaryOp(R).Left.OpCode = opcSYMBOL);
{
     *
    / \
   a   opcCall
       /\
      b  args

      ==>
           opcCall
           / \
          *   args
         / \
        a   b
}
        Result := CreateBinaryOp(opcMEMBER, E.Coord);
        Result.Typ := TBinaryOp(R).Left.Typ;
        TBinaryOp(Result).Left := L;
        TBinaryOp(Result).Right := TBinaryOp(R).Left;
        TBinaryOp(R).Left := Result;
        Result := R;
      end;
    opcMEMBER:
      begin
        Leaf := R;
        while TBinaryOp(Leaf).Left.OpCode = opcMEMBER do
          Leaf := TBinaryOp(Leaf).Left;
        // 返回opMember只能是多级访问器，所以此处必定是符号
        Assert(TBinaryOp(Leaf).Left.OpCode = opcSYMBOL);
{    Left   Right
      *      *         *
     / \    / \       / \
    a   b  c   d     *   d
                    / \
                   *   c
                  / \
                 a   b
}
        Result := CreateBinaryOp(opcMEMBER, E.Coord);
        Result.Typ := TBinaryOp(Leaf).Left.Typ;
        TBinaryOp(Result).Left := L;
        TBinaryOp(Result).Right := TBinaryOp(Leaf).Left;
        TBinaryOp(Leaf).Left := Result;

        Result := R;
      end;
  else
    Assert(False);
    Result := nil;
  end;
end;

function TCodePack.EmitExpr_ProcAddr(E: TUnaryExpr): TBaseOp;
var
  Op: TBaseOp;
begin
  Op := EmitExpr(E.Operand);
  Result := CreateUnaryOp(opcProcAddr, E.Coord);
  Result.Typ := E.Typ;
  TUnaryOp(Result).Operand := Op;
end;

function TCodePack.EmitExpr_Set(E: TUnaryExpr): TBaseOp;

  function GetIntConst(E: TExpr; out V: Integer): Boolean;
  var
    C: TConstant;
  begin
    Result := True;
    if (E.OpCode in [opCONST]) then
      V := ValToInt(TConstExpr(E).Value)
    else begin
      C := E.GetConstantSymbol;
      if C <> nil then
        V := ValToInt(C.Value)
      else
        Result := False;
    end;
  end;

  function BuildSet: TSymbolOp;
  var
    List: TListExpr;
    i, V1, V2: Integer;
    Elem: TExpr;
    Op1, Op2: TBinaryOp;
    SetVal: TSetValue;
    setV, localV: TVariable;
    localT: TSetType;
    Fun: TFunctionDecl;
    Vars: array of TExpr;
    VarCount: Integer;

    procedure Add(E: TExpr);
    begin
      if VarCount = Length(Vars) then
        SetLength(Vars, VarCount + 4);
      Vars[VarCount] := E;
      Inc(VarCount);
    end;

  begin
    List := TListExpr(E.Operand);
    SetVal := TSetValue.Create;
    VarCount := 0;
    try
      for i := 0 to List.Count-1 do
      begin
        Elem := List.Items[i];
        if GetIntConst(Elem, V1) then
        begin
          SetVal.SetBits(V1, True);
        end
        else if Elem.OpCode = opRANGE then
        begin
          if GetIntConst(TBinaryExpr(Elem).Left, V1)
            and GetIntConst(TBinaryExpr(Elem).Right, V2) then
          begin
            SetVal.SetRange(V1, V2, True);
          end
          else
          begin
            // Emit SetRange
            Add(Elem);
          end;
        end
        else
        begin
          // Emit SetElem
          Add(Elem);
        end;
      end;
    except
      SetVal.Free;
      raise;
    end;

    SetVal.Update;
    setV := AddSetConst(SetVal);

    // 如果 Vars 为空，则不创建临时变量。 返回由SetVal存贮的常量。
    if VarCount = 0 then
    begin
      Result := CreateSymbolOp(setV, E.Coord);
      Exit;
    end;

    localT := TSetType.Create;
    localT.RangeType := TSubrangeType.Create;
    AddNode(localT);
    AddNode(localT.RangeType);
    localT.RangeType.BaseType := FContext.FByteType;
    localT.RangeType.RangeBegin := 0;
    localT.RangeType.RangeEnd := 255;
{    if SetVal.BitCount = 0 then
    begin
    end
    else
    begin
      localT.RangeType.RangeBegin := SetVal.BitStart;
      localT.RangeType.RangeEnd := SetVal.BitStart + SetVal.BitCount - 1;
    end;}
    localT.Update;
    localV := AddVar(localT);

    Fun := FContext.GetSystemRoutine(srSetCopy);
    EmitCallCmd(Fun, E.Coord, [
      CreateSymbolOp(localV, E.Coord),
      CreateSymbolOp(setV, E.Coord),
      CreateConstOp(E.Coord, localT.LowByte),
      CreateConstOp(E.Coord, localT.HighByte),
      CreateConstOp(E.Coord, TSetType(SetV.VarType).LowByte),
      CreateConstOp(E.Coord, TSetType(SetV.VarType).HighByte)
      ]);

    for i := 0 to VarCount - 1 do
    begin
      if Vars[i].OpCode = opRANGE then
      begin
        Fun := FContext.GetSystemRoutine(srSetRange);
        Op1 := CreateBinaryOp(opcSUB, E.Coord);
        Op1.Left := EmitExpr(TBinaryExpr(Vars[i]).Left);
        Op1.Right := CreateConstOp(E.Coord, localT.LowByte);
        Op1.Typ := FContext.FIntegerType;

        Op2 := CreateBinaryOp(opcSUB, E.Coord);
        Op2.Left := EmitExpr(TBinaryExpr(Vars[i]).Right);
        Op2.Right := CreateConstOp(E.Coord, localT.LowByte);
        Op2.Typ := FContext.FIntegerType;
        EmitCallCmd(Fun, E.Coord, [
            CreateSymbolOp(localV, E.Coord),
            CreateConstOp(E.Coord, localT.Size),
            Op1, Op2
          ]);
      end
      else
      begin
        Fun := FContext.GetSystemRoutine(srSetElem);
        Op1 := CreateBinaryOp(opcSUB, E.Coord);
        Op1.Left := EmitExpr(Vars[i]);
        Op1.Right := CreateConstOp(E.Coord, localT.LowByte);
        Op1.Typ := FContext.FIntegerType;
        EmitCallCmd(Fun, E.Coord, [
            CreateSymbolOp(localV, E.Coord),
            CreateConstOp(E.Coord, localT.Size),
            Op1
          ]);
      end;
    end;

    Result := CreateSymbolOp(localV, E.Coord);
  end;

  function BuildOA: TSymbolOp;
  var
    Args: array of TBaseOp;
    List: TListExpr;
    i: Integer;
  begin
    List := TListExpr(E.Operand);
    SetLength(Args, List.Count);
    for i := 0 to Length(Args)-1 do
      Args[i] := EmitExpr(List.Items[i]);
    Result := EmitOAInitStmt(E.Coord,
      TOpenArrayType(E.Typ).ElementType, Args);
  end;
begin
  // opSET有两种类型： TSetType 和 TOpenArrayType
  if E.Typ.TypeCode = typSET then
  begin
    Result := BuildSet;
  end
  else
  begin
    if E.Typ.TypeCode <> typOpenArray then
      Err('EmitExpr_Set: Invalid openarray type');
    Result := BuildOA;
  end;
end;

function TCodePack.EmitExpr_SetOp(E: TBinaryExpr): TBaseOp;

  function DoExpand(Op: TBaseOp; desT: TSetType): TBaseOp;
  var
    Fun: TFunctionDecl;
    LV: TSymbolOp;
    LSet: TSetType;
  begin
    // proc _SetInflate(var Dest; const Src; lo,hi,maxHi:Byte);

    Assert(Op.Typ.TypeCode = typSet);
    LSet := TSetType(Op.Typ);

    Assert(desT.HighByte >= LSet.HighByte);
    Assert(desT.LowByte <= LSet.LowByte);

    LV := CreateSymbolOp(AddVar(desT), Op.Coord);
    Fun := FContext.GetSystemRoutine(srSetInflate);
    EmitCallCmd(Fun, Op.Coord, [LV, Op,
        CreateConstOp(Op.Coord, Byte(LSet.LowByte - desT.LowByte)),
        CreateConstOp(Op.Coord, Byte(LSet.HighByte - desT.LowByte)),
        CreateConstOp(Op.Coord, desT.HighByte)
      ]);
    Result := CreateSymbolOp(LV.Reference, Op.Coord);
  end;

  function DoSetCopy(Op: TBaseOp): TBaseOp;
  var
    Fun: TFunctionDecl;
    LV: TSymbolOp;
    LSet: TSetType;
  begin
    // proc _SetCopy(var Dest; const Src; d_lo, d_hi, s_lo, s_hi: Byte);

    Assert(Op.Typ.TypeCode = typSet);
    LSet := TSetType(Op.Typ);

    LV := CreateSymbolOp(AddVar(LSet), Op.Coord);
    Fun := FContext.GetSystemRoutine(srSetCopy);
    EmitCallCmd(Fun, Op.Coord, [LV, Op,
        CreateConstOp(Op.Coord, LSet.LowByte),
        CreateConstOp(Op.Coord, LSet.HighByte),
        CreateConstOp(Op.Coord, LSet.LowByte),
        CreateConstOp(Op.Coord, LSet.HighByte)
      ]);
    Result := CreateSymbolOp(LV.Reference, Op.Coord);

  end;

  function NeedExpand(LT, RT: TSetType; out ST: TSetType): Boolean;
  begin
    if (LT.LowByte <> RT.LowByte) or (LT.HighByte <> RT.HighByte) or (LT.Size <> RT.Size) then
    begin
      Result := True;
      // 如果某一方的能够完全包括另一方，扩展后的类型取较大方的，否则统一为通用类型
      if (LT.LowByte <= RT.LowByte) and (LT.HighByte >= RT.HighByte) then
        ST := LT
      else if (RT.LowByte <= LT.LowByte) and (RT.HighByte >= LT.HighByte) then
        ST := RT
      else
        ST := FContext.FByteSetType;
    end
    else
    begin
      Result := False;
      ST := FContext.FByteSetType;
    end;
  end;

  function IsHiddenVar(Op: TBaseOp): Boolean;
  var
    Ref: TSymbol;
  begin
    Ref := op.GetReference;
    Result := (Ref <> nil) and (Ref.NodeKind = nkVariable)
        and (vaHidden in TVariable(Ref).VarAttr);
  end;

var
  L, R: TBaseOp;
  Fun: TFunctionDecl;
  DesT: TSetType;
  sr: TSystemRoutine;
begin
  // +, -, *, <=, >=, =, <>
  L := EmitExpr(E.Left);
  R := EmitExpr(E.Right);

  Assert(L.Typ.TypeCode = typSet, 'SetOp');
  Assert(R.Typ.TypeCode = typSet, 'SetOp');

  if NeedExpand(TSetType(L.Typ), TSetType(R.Typ), DesT) then
  begin
    // Expand set
    if L.Typ.Size <> DesT.Size then
      L := DoExpand(L, desT);
    if R.Typ.Size <> DesT.Size then
      R := DoExpand(R, DesT);
  end
  else
    DesT := TSetType(L.Typ);

  if not IsHiddenVar(L) then
  begin
    L := DoSetCopy(L);
  end;

  case E.OpCode of
    opADD: sr := srSetUnion;
    opSUB: sr := srSetSub;
    opMUL: sr := srSetInterset;
    opLE: sr := srSetLE;
    opGE: sr := srSetGE;
    opEQ: sr := srSetEQ;
    opNE: sr := srSetNE;
  else
    Assert(False, 'SetOp');
    sr := srTrunc;
  end;
  // proc SetUnion(var Dest; const Src; Size: Byte);
  // proc SetSub(var Dest; const Src; Size: Byte);
  // proc SetInterset(var Dest; const Src; Size: Byte);
  // func SetLE(const Left, Right; Size: Byte): Boolean;
  // func SetGE(const Left, Right; Size: Byte): Boolean;
  // func SetEQ(const Left, Right; Size: Byte): Boolean;
  // func SetNE(const Left, Right; Size: Byte): Boolean;

  Fun := FContext.GetSystemRoutine(sr);
  if E.OpCode in [opADD, opSUB, opMUL] then
  begin
    EmitCallCmd(Fun, E.Coord, [L, R, CreateConstOp(E.Coord, desT.Size)]);
    Assert(L.OpCode = opcSymbol);
    Result := CreateSymbolOp(TSymbolOp(L).Reference, L.Coord);
  end
  else
    Result := CreateCall(Fun, E.Coord, [L, R, CreateConstOp(E.Coord, desT.Size)]);
end;

function TCodePack.EmitExpr_StrAdd(E: TBinaryExpr): TBaseOp;
begin
  Result := Self.EmitStrAddOp(E);
end;

function TCodePack.EmitExpr_StrCmp(E: TBinaryExpr): TBaseOp;

  function CompareEmptyStr(E: TBinaryExpr; S: TExpr): TBinaryOp;
  var
    CastOp: TBinaryOp;
  begin
    Result := CreateBinaryOpFrom(E);

    CastOp := CreateBinaryOp(opcCast, S.Coord);
    CastOp.Right := EmitExpr(S);
    CastOp.Left := CreateSymbolOp(FContext.FPointerType, S.Coord);
    Result.Left := CastOp;
    Result.Right := CreateConstOp(E.Coord);
    Result.Right.OpCode := opcNIL;
//    ValFromPtr(TConstOp(Result.Right).Value, nil);
  end;

begin
  // 与空字符串比较需要特殊处理
  if E.Right.IsEmptyString then
  begin
    Result := CompareEmptyStr(E, E.Left);
    Exit;
  end
  else if E.Left.IsEmptyString then
  begin
    Result := CompareEmptyStr(E, E.Right);
    Exit;
  end;

  Result := CreateBinaryOpFrom(E);
  TBinaryOp(Result).Left := EmitExpr(E.Left);
  TBinaryOp(Result).Right := EmitExpr(E.Right);

  Result := EmitStrCmp(TBinaryOp(Result));
end;

function TCodePack.EmitExpr_Symbol(E: TSymbolExpr): TBaseOp;

  function CreateMultiAccessorExpr(M: TMultiAccessor; const Coord: TAstNodeCoord): TBaseOp;
  var
    i: Integer;
    L: TBaseOp;
  begin
    Assert(M.FieldCount > 1);
    Result := CreateSymbolOp(M.Fields[0], Coord);
    for i := 1 to M.FieldCount - 1 do
    begin
      L := Result;
      Result := CreateBinaryOp(opcMEMBER, Coord);
      TBinaryOp(Result).Left := L;
      TBinaryOp(Result).Right := CreateSymbolOp(M.Fields[i], Coord);
    end;
  end;

  // 取Ref的实例变量
  // 如 Ref是个外围类的符号，则实例变量是外围类
  function GetInstVar(Ref: TSymbol): TSymbol;
  var
    P1, P2: TSymbol;
  begin
    P1 := Ref.Parent;
    P2 := FFunc.Parent;
    Assert((P1.NodeKind = nkType) and (P2.NodeKind = nkType));
    Result := FSelfSym;
    if (saClass in Ref.Attr) and (P1 <> P2) then
    begin
      // 考虑这种情况：FFunc是内围类的方法,Ref是外围类的符号，那么
      // FFunc中可以直接访问Ref。这时需要补上
      // 需要注意的是：P1<>P2的判断不能省，因为class方法是可以虚的
      // 用Self来调用和用类名调用，结果不一定相同

      // 确定FFunc所在的类型是否是Ref所在类型中的嵌套类型
      while Assigned(P2) do
      begin
        if P2 = P1 then Break;
        P2 := P2.Parent;
      end;
      if P2 = P1 then Result := P1;
    end;
  end;

var
  Ref, Getter: TSymbol;
  NeedSelf: Boolean;
  L, Leaf, S: TBaseOp;
begin
  Ref := TSymbolExpr(E).Reference;
  // 假如它是saClass? 也得加Self
  NeedSelf := (Ref.NodeKind in [nkField, nkProperty, nkIntfProperty, nkMethod])
    and not (saStatic in Ref.Attr)
    and ((E.Parent = nil) or (E.Parent.OpCode <> opMEMBER)
          or (TBinaryExpr(E.Parent).Left = E)
        );

  case Ref.NodeKind of
    nkProperty, nkIntfProperty:
      begin
        if Ref.NodeKind = nkProperty then
          Getter := TProperty(Ref).Getter
        else
          Getter := TIntfProperty(Ref).Getter;

        if Getter = nil then Err('EmitExpr_Symbol: Getter is nil');

        case Getter.NodeKind of
          nkField: Result := CreateSymbolOp(Getter, E.Coord);
          nkAccessor:
            Result := CreateMultiAccessorExpr(TMultiAccessor(Getter), E.Coord);
          nkMethod:
            Result := CreateSymbolOp(Getter, E.Coord);
        else
          Assert(False, 'EmitExpr_Symbol');
          Result := nil;
        end;

        S := Result;
        // 转为调用。
        if Getter.NodeKind = nkMethod then
        begin
          Result := CreateBinaryOp(opcCALL, E.Coord);
          Result.Typ := E.Typ;
          TBinaryOp(Result).Left := S;
          TBinaryOp(Result).Right := CreateListOp(E.Coord);
        end;
      end;
  else
    Result := CreateSymbolOpFrom(TSymbolExpr(E));
    S := Result;
  end;

  if NeedSelf then
  begin
    // 添加Self
    L := CreateBinaryOp(opcMEMBER, E.Coord);
    L.Typ := E.Typ;
    TBinaryOp(L).Left := CreateSymbolOp(GetInstVar(Ref), E.Coord);
    if TSymbolOp(TBinaryOp(L).Left).Reference = FSelfSym then
    begin
      if Self.FCurFunc.Level > 0 then
        Include(TVariable(FSelfSym).States, vsNestRef);
    end;
    if S.OpCode = opcMEMBER then
    begin
      Leaf := S;
      while TBinaryOp(Leaf).Left.OpCode = opcMEMBER do
        Leaf := TBinaryOp(Leaf).Left;
      Assert(TBinaryOp(Leaf).Left.OpCode = opcSYMBOL);
      TBinaryOp(L).Right := TBinaryOp(Leaf).Left;
      TBinaryOp(Leaf).Left := L;
    end
    else
    begin
      TBinaryOp(L).Right := S;
      Result := L
    end;
  end;

  if (Ref.NodeKind = nkConstant) then
  begin
    if TConstant(Ref).ConstType.TypeCode = typString then
    begin
      Ref := AddStrConst(ValToStr(TConstant(Ref).Value), TConstant(Ref).ConstType);
      Assert(Result.OpCode = opcSYMBOL);
      TSymbolOp(Result).Reference := Ref;
    end
    else if TConstant(Ref).ConstType.TypeCode = typSet then
    begin
      Ref := AddSetConst(ValToSet(TConstant(Ref).Value));
      Assert(Result.OpCode = opcSYMBOL);
      TSymbolOp(Result).Reference := Ref;
    end;
  end;
end;

function TCodePack.EmitExpr_UnaryVarOp(E: TUnaryExpr): TBaseOp;
var
  Fun: TFunctionDecl;
  Op1: TBaseOp;
begin
  case E.OpCode of
    opNOT: Fun := FContext.GetSystemRoutine(srVarNot);
    opNEG: Fun := FContext.GetSystemRoutine(srVarNeg);
  else
    Err('EmitExpr_UnaryVarOp: Invalid variant op');
    Fun := nil;
  end;

  Op1 := EmitExpr(E.Operand);
  if Op1.Typ.TypeCode <> typVariant then
    Err('EmitExpr_UnaryVarOp: Op1 not variant');
  Op1 := EmitVarCopy(Op1);
  EmitCallCmd(Fun, E.Coord, [Op1]);
  Result := CopyOP(Op1);
end;

function TCodePack.EmitExpr_VarArrGet(E: TBinaryExpr): TBaseOp;
var
  Fun: TFunctionDecl;
  V: TVariable;
  S, SArgs: TSymbolOp;
  Args: array of TBaseOp;
  L: TBaseOp;
  I: Integer;
begin
  // procedure _VarArrayGet(var V, Result: Variant; Indices: array of NativeInt);

  Assert(E.Right.OpCode = opLIST);

  V := AddVar(FContext.FVariantType);
  Result := CreateSymbolOp(V, E.Coord);
  S := CreateSymbolOp(V, E.Coord);
  L := EmitExpr(E.Left);
  SetLength(Args, TListExpr(E.Right).Count);
  for I := 0 to Length(Args) - 1 do
    Args[I] := EmitExpr(TListExpr(E.Right).Items[I]);

  Fun := FContext.GetSystemRoutine(srVarArrayGet);
  SArgs := EmitOAInitStmt(E.Right.Coord, FContext.FIntegerType, Args);
  EmitCallCmd(Fun, E.Coord, [L, S, SArgs]);
end;

function TCodePack.EmitExpr_VarCmp(E: TBinaryExpr): TBinaryOp;
var
  Op1, Op2, Op3: TBaseOp;
  varOpCode: Integer;
  Fun: TFunction;
  CallOp: TBinaryOp;
begin
  // opNE..opGE
  Result := CreateBinaryOpFrom(E);
  Op1 := EmitExpr(E.Left);
  Op2 := EmitExpr(E.Right);

  if Op1.Typ.TypeCode <> typVariant then
    Op1 := EmitCastToVar(Op1, False);

  if Op2.Typ.TypeCode <> typVariant then
    Op2 := EmitCastToVar(Op2, False);

  if (E.OpCode >= opNE) and (E.OpCode <= opGE) then
    varOpCode := VarOpMaps[E.OpCode]
  else
    varOpCode := -1;
  Assert(varOpCode <> -1, 'TCodeSetup.EmitVarCmp: Invalid variant op');
  Op3 := CreateConstOp(E.Coord, varOpCode);

  Fun := FContext.GetSystemRoutine(srVarComp);
  CallOp := CreateCall(Fun, E.Coord, [Op1, Op2, Op3]);

  Result.Left := CallOp;
  Result.Left.Typ := Fun.ReturnType;
  Result.Right := CreateConstOp(E.Coord, 0);
end;

function TCodePack.EmitExpr_VarOp(E: TBinaryExpr): TBaseOp;
var
  Op1, Op2, Op3: TBaseOp;
  varOpcode: Integer;
  Fun: TFunction;
  Ref: TSymbol;
begin
  if E.Left.Typ.TypeCode = typVariant then
  begin
    Ref := E.Left.GetReference;
    // 如果左表达式是临时变量，则直接使用它；反之，则复制一份
    if (Ref <> nil) and (Ref.NodeKind = nkVariable)
        and (vaHidden in TVariable(Ref).VarAttr) then
      Op1 := EmitExpr(E.Left)
    else
      Op1 := EmitVarCopy(EmitExpr(E.Left));
  end
  else
    Op1 := EmitCastToVar(EmitExpr(E.Left), False);

  if E.Right.Typ.TypeCode = typVariant then
    Op2 := EmitExpr(E.Right)
  else
    Op2 := EmitCastToVar(EmitExpr(E.Right), False);

  if (E.OpCode >= opADD) and (E.OpCode <= opSHR) then
    varOpCode := VarOpMaps[E.OpCode]
  else
    varOpCode := -1;
  Assert(varOpCode <> -1, 'TCodeSetup.EmitExpr_VarOp: Invalid variant op');
  Op3 := CreateConstOp(E.Coord, varOpCode);

  Fun := FContext.GetSystemRoutine(srVarOp);
  EmitCallCmd(Fun, E.Coord, [Op1, Op2, Op3]);
  Result := CopyOP(Op1);
end;

procedure TCodePack.EmitGotoCmd(const Target: string; const Coord: TAstNodeCoord);
var
  S: TCmd;
begin
  S := CreateCmd(TGotoCmd, Coord);
  TGotoCmd(S).Target := Target;
  AddCmd(S);
end;

procedure TCodePack.EmitIntfCastOp(E: TBinaryExpr; AsgOp: TBaseOp);
var
//  ValOp: TBinaryOp;
  IidOp: TConstOp;
  LT: TType;
  IntfTyp: TInterfaceType;
//  Entry: TClassIntfEntry;
  Cmd: TCmd;
begin
  Assert(E.OpCode = opAS);
  Assert(E.Right.Typ.TypeCode = typInterface);
  Assert(AsgOp.Typ.TypeCode = typInterface);

  LT := E.Left.Typ;
  IntfTyp := TInterfaceType(E.Right.Typ);

  if LT.TypeCode = typClass then
  begin
    // cast class to intf
    EmitObj2Intf(EmitExpr(E.Left), AsgOp, IntfTyp, True);
  end
  else if LT.TypeCode = typInterface then
  begin
    // Call _IntfCast
    IidOp := CreateConstOp(E.Right.Coord);
    ValFromIID(IidOp.Value, IntfTyp);
    Cmd := CreateCmd(TCallCmd, E.Coord);
    TCallCmd(Cmd).CallOp :=
        CreateCall(FContext.GetSystemRoutine(srIntfCast), E.Coord,
                [AsgOp, EmitExpr(E.Left), IidOp]);
    AddCmd(Cmd);
  end;
end;

procedure TCodePack.EmitMarkCmd(const Lab: string; const Coord: TAstNodeCoord
  );
var
  Cmd: TCmd;
begin
  Cmd := CreateCmd(TMarkCmd, Coord);
  TMarkCmd(Cmd).LabelName := Lab;
  AddCmd(Cmd);
end;

function TCodePack.EmitOAInitStmt(const Coord: TAstNodeCoord;
  ElemType: TType; const Args: array of TBaseOp; Start: Integer): TSymbolOp;
var
  LStmt: TOAInitCmd;
  LArrType: TOpenArrayType;
  LArrVar: TVariable;
  i: Integer;
begin
  if Length(Args) - Start > 0 then
  begin
    // 创建数组类型
    LArrType := TOpenArrayType.Create;
    AddNode(LArrType);
    LArrType.ElementCount := Length(Args) - Start;
    LArrType.ElementType := ElemType;
    LArrType.Parent := FCurFunc;

    LArrVar := AddVar(LArrType);
  end
  else
    LArrVar := nil;
  LStmt := TOAInitCmd(CreateCmd(TOAInitCmd, Coord));
  LStmt.ArrayVar := LArrVar;
  for i := Start to High(Args) do
    LStmt.Add(Args[i]);
  AddCmd(LStmt);
  Result := CreateSymbolOp(LArrVar, Coord);
end;

procedure TCodePack.EmitObj2Intf(Obj, AsgOp: TBaseOp; Intf: TInterfaceType;
  DoCast: Boolean);
var
  typ: TClassType;
  Entry: TClassIntfEntry;
  ValOp, CallOp: TBinaryOp;
  IidOp: TConstOp;
  Cmd: TCallCmd;
begin
  if Obj.Typ.TypeCode <> typClass then
    Err('EmitObj2IntfOp: Not class instance');
  typ := TClassType(Obj.Typ);
  if DoCast then
    Entry := typ.FindIntfEntry(FContext.FIUnknownType)
  else
    Entry := typ.FindIntfEntry(Intf);
  if Entry = nil then
    Err('EmitObj2IntOp: Intf Entry not found');
  Obj.Detach;
  if Entry.ImplGetter = nil then
  begin
    ValOp := CreateBinaryOp(opcAddrOffset, Obj.Coord);
    ValOp.Left := Obj;
    valOp.Right := CreateConstOp(Obj.Coord, Entry.Offset);
    ValOp.Typ := Intf;
    if DoCast then
    begin
      IidOp := CreateConstOp(Obj.Coord);
      ValFromIID(IidOp.Value, Intf);
      CallOp := CreateCall(FContext.GetSystemRoutine(srIntfCast),
                         Obj.Coord, [
                            AsgOp, ValOp, IidOp
                         ]);
    end
    else
      CallOp := CreateCall(FContext.GetSystemRoutine(srIntfCopy),
                         Obj.Coord, [
                            AsgOp, ValOp
                         ]);
  end
  else
  begin
    // call obj.ImplGetter
    ValOp := CreateBinaryOp(opcMember, Obj.Coord);
    ValOp.Left := Obj;
    ValOp.Right := CreateSymbolOp(Entry.ImplGetter, Obj.Coord);
    ValOp.Typ := FContext.FUntype;
    CallOp := CreateBinaryOp(opcCall, Obj.Coord);
    CallOp.Left := ValOp;
    CallOp.Right := CreateListOp(Obj.Coord, [AsgOp]);
    CallOp.Typ := Intf;
  end;
  Cmd := TCallCmd(CreateCmd(TCallCmd, Obj.Coord));
  Cmd.CallOp := callOp;
  AddCmd(Cmd);
end;

function TCodePack.EmitObj2Intf(Obj: TBaseOp; Intf: TInterfaceType;
  DoCast: Boolean): TBaseOp;
var
  S: TSymbolOp;
begin
  S := CreateSymbolOp(AddVar(Intf), Obj.Coord);
  Result := CreateSymbolOp(S.Reference, S.Coord);
  EmitObj2Intf(Obj, S, Intf, DoCast);
end;

procedure TCodePack.EmitStrAddOp(E: TBinaryExpr; AsgOp: TBaseOp);
var
  LCount: Integer;
  LAddends: array of TExpr;
  LOperands: array of TBaseOp;

  procedure Append(E: TExpr);
  begin
    if LCount = Length(LAddends) then
      SetLength(LAddends, LCount + 10);
    LAddends[LCount] := E;
    Inc(LCount);
  end;

  // 取得所有相加的字符串
  procedure GetOperands(E: TBinaryExpr);
  begin
    if E.Left.OpCode = opADD then
      GetOperands(TBinaryExpr(E.Left))
    else
      Append(E.Left);

    if E.Right.OpCode = opADD then
      GetOperands(TBinaryExpr(E.Right))
    else
      Append(E.Right);
  end;

const
  LongStrTypes = [strAnsi, strWide, strUnicode];
  StrTypes = [strAnsi, strWide, strUnicode, strAShort, strWShort];

var
  i, cat: Integer;
  desT: TStringType;
  desE: TBaseOp;
  Fun: TFunction;
  Inst: TCallCmd;

  function EmitStrCatN: TCallCmd;
  var
    LHighExpr: TConstOp;
    LArr: TSymbolOp;
  begin
    LHighExpr := CreateConstOp(E.Coord, LCount - 1);
    LArr := EmitOAInitStmt(E.Coord, desT, LOperands, 0);
    Result := TCallCmd(CreateCmd(TCallCmd, E.Coord));
    Result.CallOp := CreateCall(Fun, E.Coord, [desE, LArr, LHighExpr]);
  end;
begin
  // 字符串相加，含有UnicodeString，转为UnicodeString相加
  // 含有WideString，转为WideString
  // ShortString相加，转为String

  // 为了效率，则一次性连接多个字符：
  // 如 s := s1 + s2 + s3 + s4，转为函数调用：
  // AStrCatN(s, s1, s2, s3, s4)

  LCount := 0;
  GetOperands(E);

  SetLength(LOperands, LCount);
  for i := 0 to LCount - 1 do
  begin
    // LOperands[i].Typ 必须是与字符串类型兼容的
    LOperands[i] := EmitExpr(LAddends[i]);
  end;

  if AsgOp.Typ.TypeCode <> typString then
    Err('EmitStrAddOp: Invalid AsgExpr type');
  if E.Typ.TypeCode <> typString then
    Err('EmitStrAddOp: Invalid E type');

  desT := TStringType(E.Typ);
  if TStringType(AsgOp.Typ).Kind <> desT.Kind then
    desE := CreateSymbolOp(AddVar(desT), E.Coord)
  else
    desE := AsgOp;

  for i := 0 to LCount - 1 do
  begin
    if not LOperands[i].Typ.Equals(desT) then
      LOperands[i] := EmitCastToStr(LOperands[i], desT);
  end;

  // todo 9: 需要合并字符串常量

  if LCount = 2 then
  begin
    if desE = LOperands[0] then
      cat := 2
    else
      cat := 3;
  end
  else
    cat := 4;
  case TStringType(desT).Kind of
    strAnsi:
      if cat = 2 then
        Fun := FContext.GetSystemRoutine(srAStrCat)
      else if cat = 3 then
        Fun := FContext.GetSystemRoutine(srAStrCat3)
      else
        Fun := FContext.GetSystemRoutine(srAStrCatN);
    strWide:
      if cat = 2 then
        Fun := FContext.GetSystemRoutine(srWStrCat)
      else if cat = 3 then
        Fun := FContext.GetSystemRoutine(srWStrCat3)
      else
        Fun := FContext.GetSystemRoutine(srWStrCatN);
    strUnicode:
      if cat = 2 then
        Fun := FContext.GetSystemRoutine(srUStrCat)
      else if cat = 3 then
        Fun := FContext.GetSystemRoutine(srUStrCat3)
      else
        Fun := FContext.GetSystemRoutine(srUStrCatN);
  else
    Assert(False);
  end;

  Inst := TCallCmd(CreateCmd(TCallCmd, E.Coord));
  if cat = 2 then
    Inst.CallOp := CreateCall(Fun, E.Coord, [desE, LOperands[1]])
  else if cat = 3 then
    Inst.CallOp := CreateCall(Fun, E.Coord, [desE, LOperands[0], LOperands[1]])
  else
    Inst := EmitStrCatN;
  AddCmd(Inst);
  if desE <> AsgOp then
  begin
    // 此时desE是个TSymbolExpr
    desE := CreateSymbolOp(TSymbolOp(desE).Reference, desE.Coord);
    EmitCastToStr(desE, AsgOp, desT);
  end;
end;

function TCodePack.EmitStrAddOp(E: TBinaryExpr): TSymbolOp;
var
  S: TSymbolOp;
begin
  S := CreateSymbolOp(AddVar(E.Typ), E.Coord);
  Result := CreateSymbolOp(S.Reference, S.Coord);
  EmitStrAddOp(E, S);
end;

procedure TCodePack.EmitStrAsg(E, Dest: TBaseOp);
const
  StrAsgProc: array[TStringKind] of TSystemRoutine = (
    srAStrAsg, srWStrAsg, srUStrAsg, srSStrAsg, srTrunc//srSWStrAsg
    );
var
  sr: TSystemRoutine;
  Fun: TFunctionDecl;
begin
  Assert(E.Typ.TypeCode = typString);
  Assert(Dest.Typ.TypeCode = typString);
  Assert(TStringType(E.Typ).Kind = TStringType(Dest.Typ).Kind);
  sr := StrAsgProc[TStringType(E.Typ).Kind];
  Fun := FContext.GetSystemRoutine(sr);
  if sr = srSStrAsg then
    Self.EmitCallCmd(Fun, E.Coord, [Dest,
                  CreateConstOp(E.Coord, TStringType(E.Typ).CharCount), E]
          )
  else
    Self.EmitCallCmd(Fun, E.Coord, [Dest, E]);
end;

function TCodePack.EmitStrCmp(E: TBinaryOp): TBaseOp;
type
  TBaseKind = (bkUStr, bkWStr, bkAStr, bkSStr, bkSWStr, bkPACh, bkPWCh,
               bkAArr, bkWArr, bkAChr, bkWChr);
const               
  BaseStr: array[bkUStr..bkWChr] of string = (
    'UStr', 'WStr', 'AStr', 'SStr', 'SWStr', 'Pa', 'Pw', 'Aarr', 'Warr', 'ACh', 'WCh'
  );

  function GetBase(T: TType): TBaseKind;
  var
    bk: TBaseKind;
  begin
    bk := bkUStr;
    case T.TypeCode of
      typString:
        case TStringType(T).Kind of
          strAnsi: bk := bkAStr;
          strWide: bk := bkWStr;
          strUnicode: bk := bkUStr;
          strAShort: bk := bkSStr;
          strWShort: bk := bkSWStr;
        else
          Assert(False);
        end;
      typChar:
        if TCharType(T).Kind = charAnsi then
          bk := bkAChr
        else
          bk := bkWChr;
      typPAnsiChar: bk := bkPACh;
      typPWideChar: bk := bkPWCh;
    else
      if T.IsPackedStringAnsi then
        bk := bkAArr
      else if T.IsPackedStringWide then
        bk := bkWArr
      else
        Assert(False);
    end;
    Result := bk;
  end;

  procedure AdjustType(Op: TSymbolOp; typ: TType);
  var
    c: TConstant;
  begin
    if Op.Typ.Equals(typ) then Exit;

    c := Op.GetConstantSymbol;
    if c = nil then Exit;

    if ((typ.TypeCode = typString)
          and (TStringType(typ).Kind in [strAnsi..strUnicode]))
      or typ.IsPAnsiChar or typ.IsPWideChar then
    begin
      C := AddStrConst(ValToStr(c.Value), typ);
      Op.Reference := C;
      Op.Typ := C.ConstType;
      Op.Name := C.Name;
    end;
  end;

  function CompareEmptyStr(E: TBinaryOp; S: TBaseOp): TBaseOp;
  var
    CastOp: TBinaryOp;
  begin
    CastOp := CreateBinaryOp(opcCast, S.Coord);
    CastOp.Right := S;
    CastOp.Left := CreateSymbolOp(FContext.FPointerType, S.Coord);
    E.Left := CastOp;
    E.Right := CreateConstOp(E.Coord);
    E.OpCode := opcNIL;
    //ValFromPtr(TConstOp(E.Right).Value, nil);
    Result := E;
  end;

var
  s: string;
  Lb, Rb: TBaseKind;
  Sym: TSymbol;
  Fun: TFunctionDecl;
  CallOp: TBinaryOp;
begin
  Assert(E.OpCode in [opcNE, opcEQ, opcLT, opcLE, opcGT, opcGE]);

  if E.Left.IsConstSymbol then
    AdjustType(TSymbolOp(E.Left), E.Right.Typ)
  else if E.Right.IsConstSymbol then
    AdjustType(TSymbolOp(E.Right), E.Left.Typ);

  Lb := GetBase(E.Left.Typ);
  Rb := GetBase(E.Right.Typ);
  if Lb = Rb then
  begin
    case Lb of
      bkUStr: s := '_UStrComp';
      bkWStr: s := '_WStrComp';
      bkAStr: s := '_AStrComp';
      bkSStr: s := '_SStrComp';
      bkSWStr: s := '_SWStrComp';
      bkAArr: s := '_AArrComp';
      bkWArr: s := '_WArrComp';
      bkAChr, bkWChr, bkPACh, bkPWCh:
        begin
        // 直接比较
          Result := E;
          Exit;
        end;
    end;
  end
  else if ((Lb = bkAChr) and (Rb = bkWChr)) or
          ((Rb = bkAChr) and (Lb = bkWChr)) then
  begin
    Err('EmitStrOp: Operand not applicable');
  end
  else if ((Lb = bkPACh) and (Rb = bkPWCh)) or
          ((Rb = bkPACh) and (Lb = bkPWCh)) then
  begin
    Err('EmitStrOp: Operand not applicable');
  end
  else
  begin
    s := '_' + BaseStr[Lb] + 'Comp' + BaseStr[Rb];
  end;

  Sym := FContext.FSystemUnit.FindSymbol(s);
  if not (Assigned(Sym) and (Sym.NodeKind = nkFunc)) then
    Err('EmitStrOp: System routine %s not missing', [s]);

  Fun := TFunctionDecl(Sym);
  if Lb in [bkAArr, bkWArr] then
  begin
    if rb in [bkAArr, bkWArr] then
    begin
      CallOp := CreateCall(Fun, E.Coord, [
        E.Left,
        CreateConstOp(E.Coord, TArrayType(E.Left.Typ).ElementCount),
        E.Right,
        CreateConstOp(E.Coord, TArrayType(E.Left.Typ).ElementCount)
      ]);
    end
    else
    begin
      CallOp := CreateCall(Fun, E.Coord, [
        E.Left,
        CreateConstOp(E.Coord, TArrayType(E.Left.Typ).ElementCount),
        E.Right
      ]);
    end;
  end
  else if rb in [bkAArr, bkWArr] then
  begin
    CallOp := CreateCall(Fun, E.Coord, [
      E.Left,
      E.Right,
      CreateConstOp(E.Coord, TArrayType(E.Left.Typ).ElementCount)
    ]);
  end
  else
    CallOp := CreateCall(Fun, E.Coord, [E.Left, E.Right]);
  E.Left := CallOp;
  E.Left.Typ := Fun.ReturnType;
  E.Right := CreateConstOp(E.Coord, 0);
  Result := E;
end;

procedure TCodePack.EmitVarCopy(E, Dest: TBaseOp);
var
  Fun: TFunction;
  Inst: TCallCmd;
begin
  if (E.Typ.TypeCode <> typVariant) or (Dest.Typ.TypeCode <> typVariant) then
    Err('EmitVarCopy: E or Dest not variant');
  Fun := FContext.GetSystemRoutine(srVarCopy);
  Inst := TCallCmd(CreateCmd(TCallCmd, Dest.Coord));
  Inst.CallOp := CreateCall(Fun, Dest.Coord, [Dest, E]);
  AddCmd(Inst);
end;

function TCodePack.EmitVarCopy(E: TBaseOp): TSymbolOp;
var
  V: TVariable;
  S: TSymbolOp;
begin
  V := AddVar(FContext.FVariantType);
  S := Self.CreateSymbolOp(V, E.Coord);
  Result := CreateSymbolOp(V, E.Coord);
  EmitVarCopy(E, S);
end;

function TCodePack.LabelStr(const prefix: string): string;
begin
  Inc(FLabelID);
  Result := prefix + IntToStr(FLabelID);
end;

procedure TCodePack.RemoveNode(Node: TObject);
var
  I: Integer;
begin
  for I := FNodes.Count - 1 downto 0 do
    if FNodes[I] = Node then
    begin
      Node.Free;
      FNodes.Delete(I);
      Exit;
    end;
end;

procedure TCodePack.Setup(Acntx: TCompileContext; Func: TFunction);
begin
  FContext := ACntx;
  FFunc := Func;
  if Func.NodeKind = nkMethod then
  begin
    FSelfSym := Func.LocalSymbols.Find('Self');
    FSelfTypeSym := Func.Parent;
    Assert(FSelfSym <> nil);
    Assert((FSelfTypeSym <> nil) and (FSelfTypeSym.NodeKind = nkType) );
  end
  else
  begin
    FSelfSym := nil;
    FSelfTypeSym := nil;
  end;
  SetupFunc(FFunc);
  AddRawArgs;
end;

procedure TCodePack.SetupFunc(F: TFunction);

  procedure AddVarCleanCmd(Code: TCode; const Coord: TAstNodeCoord);
  var
    Cmd: tCmd;
  begin
    Cmd := Self.CreateCmd(TUninitVarCmd, Coord);
    Code.Cmds.Add(Cmd);
  end;

  function IsVarCleanNeed(Vars: TList): Boolean;
  var
    i: Integer;
    sym: TSymbol;
  begin
    Result := True;
    for i := 0 to Vars.Count - 1 do
    begin
      sym := TSymbol(Vars[i]);
      case sym.NodeKind of
        nkVariable:
          if (vaLocal in TVariable(sym).VarAttr)
            and not (vaSelf in TVariable(sym).VarAttr)
            and not (vaResult in TVariable(sym).VarAttr)
            and NeedFree(TVariable(sym).VarType) then Exit;
        nkFuncParam:
          if (saUsed in sym.Attr)
            and (TFuncParam(Sym).Modifier = argDefault)
            and NeedFree(TFuncParam(sym).ParamType) then Exit;
      end;
    end;
    Result := False;
  end;

  procedure RemoveVarCleanFunc();
  var
    i: Integer;
    cmd: TCmd;
    sym: TSymbol;
  begin
    i := 0;
    while i < FCurCode.Cmds.Count do
    begin
      cmd := TCmd(FCurCode.Cmds[i]);
      if cmd.Kind = insCall then
      begin
        sym := TCallCmd(cmd).CallOp.Left.GetReference;
        if (sym = FCurCode.VarCleanFunc) then
        begin
          FCurCode.Cmds.Delete(i);
          Continue;
        end;
      end;
      Inc(i);
    end;
  end;

  procedure SetupNested(Funcs: TList);
  var
    i: Integer;
  begin
    for i := 0 to Funcs.Count - 1 do
    begin
      SetupFunc(TFunction(Funcs[i]));
    end;
  end;

var
  I: Integer;
  Sym: TSymbol;
  LCode: TCode;
  LCmd: TCmd;
begin
  FResultSym := F.LocalSymbols.Find('Result');
  if not ((FResultSym <> nil)
      and (FResultSym.NodeKind = nkVariable)
      and (vaResult in TVariable(FResultSym).VarAttr)) then
  begin
    FResultSym := nil;
  end;

  FCurFunc := F;
  FCurCode := TCode.Create;

  FCurCode.VarCleanFunc := CreateFunc;
  FCurCode.VarCleanFunc.Parent := FCurFunc;
  FCurCode.VarCleanFunc.Attr := [saInternal];
  FCurCode.VarCleanFunc.Name := '$varclean';
  FCurCode.VarCleanFunc.Level := FCurFunc.Level + 1;

  // 需要先加cleanup过程，Exit 要调用之后才能退出函数
  FCleanupStack.Add(FCurCode.VarCleanFunc);

  for i := 0 to F.LocalSymbols.Count - 1 do
  begin
    Sym := F.LocalSymbols[i];
    case Sym.NodeKind of
      nkVariable, nkFuncParam:
        FCurCode.Vars.Add(Sym);
      nkFunc:
        FCurCode.Funcs.Add(Sym);
    end;
  end;

  LCmd := CreateCmd(TCleanupCmd, F.Coord);
  TCleanupCmd(LCmd).CleanupProc := FCurCode.VarCleanFunc;
  AddCmd(LCmd);

  FQuitLabel := '.quit';
  SetupStmt(F.StartStmt);

  Assert(FCleanupStack.Count=1);
  FCleanupStack.Clear;

  F.Codes := FCurCode;

  // 创建清除过程
  if IsVarCleanNeed(FCurCode.Vars) then
  begin
    LCmd := CreateCmd(TLeaveBlockCmd, F.Coord);
    AddCmd(LCmd);
    EmitCallCmd(FCurCode.VarCleanFunc, F.Coord, []);

    LCode := TCode.Create;
    FCurCode.VarCleanFunc.Codes := LCode;
    AddVarCleanCmd(LCode, F.Coord);
  end
  else
  begin
    RemoveVarCleanFunc();
    FCurCode.Cmds.Delete(0);  // 第一个是Cleanup
    FCurCode.VarCleanFunc := nil;
  end;

  if FCurFunc.CallConvention = ccSafecall then
  begin
    LCmd := CreateCmd(THandleScExceptCmd, FCurFunc.Coord);
    FCurCode.Cmds.Insert(0, LCmd);
    CreateExPtrVar;
  end
  else if (FCurFunc.NodeKind = nkMethod)
      and (TMethod(FCurFunc).MethodKind = mkConstructor) then
  begin
    LCmd := CreateCmd(THandleCtorExceptCmd, FCurFunc.Coord);
    FCurCode.Cmds.Insert(0, LCmd);
    CreateExPtrVar;
  end;

  EmitMarkCmd(FQuitLabel, F.Coord);

  if (FCurCode.Funcs.Count > 0) and (FSelfSym <> nil) then
  begin
    Include((FSelfSym as TVariable).States, vsNestRef);
  end;

  FCurCode := nil;
  FCurFunc := nil;
  SetupNested(TCode(F.Codes).Funcs);
end;

procedure TCodePack.SetupStmt(Stmt: TStatement);
var
  i: Integer;
begin
  case Stmt.StmtKind of
    skCompoundStmt:
      for i := 0 to TCompoundStmt(Stmt).Statements.Count - 1 do
      begin
        SetupStmt(TStatement(TCompoundStmt(Stmt).Statements[i]));
      end;
    skAssignmentStmt:
      SetupStmt_Assign(TAssignmentStmt(Stmt));
    skIfStmt:
      SetupStmt_If(TIfStmt(Stmt));
    skWhileStmt:
      SetupStmt_While(TWhileStmt(Stmt));
    skRepeatStmt:
      SetupStmt_Repeat(TRepeatStmt(Stmt));
    skForStmt:
      SetupStmt_For(TForStmt(Stmt));
    skTryStmt:
      SetupStmt_Try(TTryStmt(Stmt));
    skCallStmt:
      SetupStmt_Call(TCallStmt(Stmt));
    skRaiseStmt:
      SetupStmt_Raise(TRaiseStmt(Stmt));
    skCaseStmt:
      SetupStmt_Case(TCaseStmt(Stmt));
  end;
end;

procedure TCodePack.SetupStmt_Assign(Stmt: TAssignmentStmt);
var
  LV, RV: TBaseOp;
  R: TExpr;
  S: TAssignCmd;
  function IsSafecall(E: TExpr): Boolean;
  var
    Sym: TFunctionDecl;
  begin
    Result := False;
    if E.OpCode = opCALL then
    begin
      Sym := TBinaryExpr(E).Left.GetFunctionSymbol;
      if Sym <> nil then
        Result := Sym.CallConvention = ccSafecall
      else if TBinaryExpr(E).Left.Typ.TypeCode = typProcedural then
        Result := TProceduralType(TBinaryExpr(E).Left.Typ).CallConvention = ccSafecall;
    end;
  end;

  function CastStrConst(C: TConstant; typ: TType; const Coord: TAstNodeCoord): TSymbolOp;
  begin
    C := AddStrConst(ValToStr(C.Value), typ);
    Result := CreateSymbolOp(C, Coord);
  end;

  procedure EmitSetAssign(RV, LV: TBaseOp);
  var
    Fun: TFunctionDecl;
    LT, RT: TSetType;
  begin
    // proc _SetCopy(var Dest; const Src; d_lo, d_hi, s_lo, s_hi: Byte);

    Assert(LV.Typ.TypeCode = typSet);
    Assert(RV.Typ.TypeCode = typSet);
    LT := TSetType(LV.Typ);
    RT := TSetType(RV.Typ);

    Fun := FContext.GetSystemRoutine(srSetCopy);
    EmitCallCmd(Fun, LV.Coord, [LV, RV,
        CreateConstOp(LV.Coord, LT.LowByte),
        CreateConstOp(LV.Coord, LT.HighByte),
        CreateConstOp(LV.Coord, RT.LowByte),
        CreateConstOp(LV.Coord, RT.HighByte)
      ]);
  end;

  procedure EmitStrClr(LV: TBaseOp);
  const
    ClrFun: array[TStringKind] of TSystemRoutine = (
      srAStrClr, srWStrClr, srUStrClr, srSStrClr, srSWStrClr
    );
  var
    Fun: TFunctionDecl;
    Cmd: TCmd;
  begin
    Assert(LV.Typ.TypeCode = typString);
    Cmd := CreateCmd(TCallCmd, LV.Coord);
    Fun := FContext.GetSystemRoutine(ClrFun[TStringType(LV.Typ).Kind]);
    TCallCmd(Cmd).CallOp := CreateCall(Fun, LV.Coord, [LV]);
    AddCmd(Cmd);
  end;

  procedure EmitIntfClr(Op: TBaseOp);
  var
    Cmd: TCmd;
  begin
    Cmd := CreateCmd(TCallCmd, Op.Coord);
    TCallCmd(cmd).CallOp := CreateCall(
            FContext.GetSystemRoutine(srIntfClr),
            Op.Coord, [Op]
          );
    AddCmd(Cmd);
  end;

  procedure EmitIntfCopy(Des, Src: TBaseOp);
  var
    Cmd: TCmd;
  begin
    Cmd := CreateCmd(TCallCmd, Des.Coord);
    TCallCmd(cmd).CallOp := CreateCall(
            FContext.GetSystemRoutine(srIntfCopy),
            Des.Coord, [Des, Src]
          );
    AddCmd(Cmd);
  end;

  function EmitProcAddr(E: TExpr): TBaseOp;
  var
    L: TExpr;
    Sym: TSymbol;
  begin
    Sym := E.GetReference;
    if Assigned(Sym) and (Sym.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
    begin
      if E.OpCode <> opADDR then
      begin
        L := E;
        E := TExpr(Self.CreateNode(TUnaryExpr));
        E.Coord := L.Coord;
        E.Typ := L.Typ;
        E.OpCode := opADDR;
        TUnaryExpr(E).Operand := L;
      end;
      Result := Self.EmitExpr_ProcAddr(TUnaryExpr(E));
    end
    else
      Result := Self.EmitExpr(E);
  end;
begin
  LV := EmitExpr(Stmt.Left);

  R := Stmt.Right;
  // 忽略无用的Cast
  while (R.OpCode = opCAST) and R.Typ.Equals(TBinaryExpr(R).Right.Typ) do
  begin
    R := TBinaryExpr(R).Right;
  end;

  // 对于string, record, interface, variant, array, dynarray这些类型
  // 的赋值，以及Safecall，必须转为函数调用
  if IsSpecialType(LV.Typ) or IsSafecall(R) then
  begin
    if LV.Typ.TypeCode = typString then
    begin
      if R.OpCode = opADD then
        EmitStrAddOp(TBinaryExpr(R), LV)
      else if (R.OpCode = opCALL) and R.Typ.Equals(LV.Typ) then
        EmitCallSpecial(TBinaryExpr(R), LV)
      else if R.IsEmptyString then
        EmitStrClr(LV)
      else begin
        RV := Self.EmitExpr(R);
        // 做赋值
        if not RV.Typ.IsStringArithCompatible then
          Err('SetupStmt_Assign: Invalid string assign');
        if RV.Typ.Equals(LV.Typ) then
          Self.EmitStrAsg(RV, LV)
        else if RV.IsConstSymbol then
        begin
          Self.EmitStrAsg(
            CastStrConst(TConstant(TSymbolOp(RV).Reference), Lv.Typ, LV.Coord),
            LV);
        end
        else
          Self.EmitCastToStr(RV, LV, TStringType(LV.Typ));
      end;
    end
    else if LV.Typ.TypeCode = typInterface then
    begin
      if R.OpCode = opAS then
      begin
        // two case:
        // intf as InterfaceType
        // obj as InterfaceType
        EmitIntfCastOp(TBinaryExpr(R), LV);
      end
      else if (R.OpCode = opCALL) and (R.Typ.TypeCode = typInterface) then
      begin
        // 无需检查接口类型是否一致，因为CheckExpr已经保证它们是可以赋值的
        EmitCallSpecial(TBinaryExpr(R), LV)
      end
      else if R.Typ.TypeCode = typClass then
      begin
        EmitObj2Intf(EmitExpr(R), LV, TInterfaceType(LV.Typ));
      end
      else
      begin
        if R.IsNilConst then
          EmitIntfClr(LV)
        else
          EmitIntfCopy(LV, EmitExpr(R))
      end;
    end
    else if (R.OpCode = opCALL) and R.Typ.Equals(LV.Typ) then
    begin
      EmitCallSpecial(TBinaryExpr(R), LV)
    end
    else
    begin
      RV := Self.EmitExpr(R);
      // 做赋值
      case LV.Typ.TypeCode of
        typRecord: Assert(False);
        //typVariant:
        //typDynamicArray:
        //typArray:
        typSet:
          EmitSetAssign(RV, LV);
      end;
    end;
  end
  else
  begin
    if LV.Typ.TypeCode = typProcedural then
      RV := EmitProcAddr(R)
    else
      RV := EmitExpr(R);
    if (LV.Typ.TypeCode in [typPAnsiChar, typPWideChar]) and RV.IsConstSymbol then
    begin
      RV := CastStrConst(TConstant(TSymbolOp(RV).Reference), Lv.Typ, LV.Coord);
    end;

    // 对于Set，调用 _Setcopy
    if LV.Typ.TypeCode = typSet then
    begin
      EmitSetAssign(RV, LV);
    end
    else
    begin
      S := TAssignCmd(CreateCmd(TAssignCmd, Stmt.Coord));
      S.Left := LV;
      S.Right := RV;
      AddCmd(S);
    end;
  end;

end;

procedure TCodePack.SetupStmt_Call(Stmt: TCallStmt);
var
  Cmd: TCmd;
  Op: TBaseOp;
begin
  Op := EmitExpr(Stmt.CallExpr);
  // op 可能是一个Symbol
  if (Op <> nil)
    and (Op.OpCode in [opcCall, opcCallSpecial, opcCallBuiltin]) then
  begin
    Cmd := CreateCmd(TCallCmd, Stmt.Coord);
    TCallCmd(Cmd).CallOp := TBinaryOp(Op);
    AddCmd(Cmd);
  end;
end;

procedure TCodePack.SetupStmt_Case(Stmt: TCaseStmt);
  {
  function CreateCaseValueAsg(V: TVariable; CaseValue: TExpr): TAssignmentStmt;
  begin
    Result := TAssignmentStmt(CreateNode(TAssignmentStmt));
    Result.Coord := CaseValue.Coord;
    Result.Left := TExpr(CreateNode(TSymbolExpr));
    Result.Right := CaseValue.Expr;
    with TSymbolExpr(Result.Left) do
    begin
      Coord := Stmt.Expr.Coord;
      OpCode := opSYMBOL;
      Reference := V;
    end
  end;

  function CreateCompare(r1: Int64; v: TVariable): TBinaryExpr;
  begin
    Result := TBinaryExpr(CreateNode(TBinaryExpr));
    Result.OpCode := opEQ;
    Result.Left := CreateNode(TSymbolExpr);
    Result.Right := CreateNode(TConstExpr);
  end;

  function CaseSelectorToIfStmt(Stmt: TCaseStmt; Sel: TCaseSelector): TIfStmt;
  var
    i: Integer;
    r1, r2: Int64;
  begin
    Result := TIfStmt(CreateNode(TIfStmt));
    for i := 0 to Sel.Count - 1 do
    begin

    end;
  end;

  procedure CaseStmtToIfStmt(Stmt: TCaseStmt);
  var
    i, j: Integer;
    r1, r2: Int64;
    Sel: TCaseSelector;
    IfStmt: TIfStmt;
    AsgStmt: TAssignmentStmt;
    CmpEx: TBinaryExpr;
    V: TVariable;
  begin
    V := Stmt.Expr.GetVariableSymbol;
    if V = nil then
    begin
      V := AddVar(Stmt.Expr.Typ);
      AsgStmt := CreateCaseValueAsg(V, Stmt.Expr);
      SetupStmt(AsgStmt);
    end;

    for i := 0 to Stmt.Count - 1 do
    begin
      Sel := Stmt.Selectors[i];
      for j := 0 to Sel.Count - 1 do
      begin
        r1 := Sel.Values[j].Start;
        r2 := Sel.Values[j].Stop;
        if r1 = r2 then
          CmpEx := CreateCompare(r1, V)
        else
          CmdEx := CreateCompareRange(r1, r2, V);
      end;
    end;
  end;
       }

  function CreateCaseValAsgCmd(V: TVariable; E: TExpr): TAssignCmd;
  begin
    Result := TAssignCmd(CreateCmd(TAssignCmd, E.Coord));
    Result.Left := CreateSymbolOp(V, E.Coord);
    Result.Right := EmitExpr(E);
  end;

  function CreateCompareOp(V: TVariable; R: Int64; const Coord: TAstNodeCoord): TBinaryOp;
  begin
    Result := TBinaryOp(CreateBinaryOp(opcEQ, Coord));
    Result.Left := CreateSymbolOp(V, Coord);
    Result.Right := CreateConstOp(Coord, R);
    Result.Typ := FContext.FBooleanType;
  end;

  function RangeToCompareOp(V: TVariable; R: TCaseRange): TBinaryOp;
  begin
    if R.Start = R.Stop then
    begin
      Result := CreateCompareOp(V, R.Start, R.Coord);
    end
    else
    begin
      Result := TBinaryOp(CreateBinaryOp(opcAND, R.Coord));
      Result.Typ := FContext.FBooleanType;
      Result.Left := CreateCompareOp(V, R.Start, R.Coord);
      Result.Left.OpCode := opcGE;
      Result.Right := CreateCompareOp(V, R.Stop, R.Coord);
      Result.Right.OpCode := opcLE;
    end;
  end;

  function CaseSelToIfCmd(V: TVariable; Sel: TCaseSelector): TJumpCmd;
  var
    OpL, OpR: TBinaryOp;
    i: Integer;
  begin
    Assert(Sel.Count > 0);
    OpL := RangeToCompareOp(V, Sel.Values[0]);
    for i := 1 to sel.Count - 1 do
    begin
      OpR := OpL;
      OpL := TBinaryOp(CreateBinaryOp(opcOR, Sel.Values[i].Coord));
      OpL.Left := OpR;
      OpL.Right := RangeToCompareOp(V, Sel.Values[i]);
      OpL.Typ := FContext.FBooleanType;
    end;
    Result := TJumpCmd(CreateCmd(TJumpCmd, Sel.Values[0].Coord));
    Result.Condition := OpL;
  end;

  procedure CaseStmtToIfStmt(Stmt: TCaseStmt);
  var
    i: Integer;
    V: TVariable;
    Cmd: TCmd;
    LbPre, LbTrue, LbFalse, LbEnd: string;
  begin
    V := Stmt.Expr.GetVariableSymbol;
    if V = nil then
    begin
      V := AddVar(Stmt.Expr.Typ);
      Cmd := CreateCaseValAsgCmd(V, Stmt.Expr);
      AddCmd(Cmd);
    end;

    Inc(FLevelID);
    LbPre := Format('case%d_', [FLevelID]);
    LbEnd := LbPre + 'end';

    for i := 0 to Stmt.Count - 1 do
    begin
      LbTrue := Format('%s%d_true', [LbPre, i + 1]);
      LbFalse := Format('%s%d_false', [LbPre, i + 1]);
      Cmd := CaseSelToIfCmd(V, Stmt.Selectors[i]);
      TJumpCmd(Cmd).TrueTarget := LbTrue;
      TJumpCmd(Cmd).FalseTarget := LbFalse;
      AddCmd(Cmd);
      EmitMarkCmd(LbTrue, Cmd.Coord);
      SetupStmt(Stmt.Selectors[i].Stmt);
      EmitGotoCmd(LbEnd, Cmd.Coord);
      EmitMarkCmd(LbFalse, Cmd.Coord);
    end;
    if Stmt.Default <> nil then
    begin
      SetupStmt(Stmt.Default);
    end;
    EmitMarkCmd(LbEnd, Stmt.Coord);
  end;

var
  Cmd: TSwitchCmd;
  i, j: Integer;
  k: Int64;
  Sel: TCaseSelector;
  LbPre, LbSel, LbEnd: string;
begin
  // 值太多，转为if语句。
  // 这处判断太简单,需要优化
  if Stmt.TotalValueCount > 10 then
  begin
    CaseStmtToIfStmt(Stmt);
    Exit;
  end;

  Cmd := TSwitchCmd(CreateCmd(TSwitchCmd, Stmt.Coord));
  Cmd.Value := EmitExpr(Stmt.Expr);
  AddCmd(Cmd);

  Inc(FLevelID);
  LbPre := Format('case%d_', [FLevelID]);
  LbEnd := Format('case%d_end', [FLevelID]);

  EmitGotoCmd(LbEnd, Stmt.Coord);
  for i := 0 to Stmt.Count - 1 do
  begin
    Sel := Stmt.Selectors[i];
    LbSel := Format('%s%d', [LbPre, i + 1]);
    EmitMarkCmd(LbSel, Sel.Stmt.Coord);

    SetupStmt(Sel.Stmt);
    EmitGotoCmd(LbEnd, Sel.Stmt.Coord);

    for j := 0 to Sel.Count - 1 do
    begin
      k := Sel.Values[j].Start;
      while k <= Sel.Values[j].Stop do
      begin
        Cmd.AddEntry(k, LbSel);
        Inc(k);
      end;
    end;
  end;

  if Stmt.Default <> nil then
  begin
    LbSel := Format('%s%d', [LbPre, 0]);
    Cmd.OtherwiseTarget := LbSel;
    EmitMarkCmd(LbSel, Stmt.Default.Coord);
    SetupStmt(Stmt.Default);
    EmitGotoCmd(LbEnd, Stmt.Default.Coord);
  end
  else
    Cmd.OtherwiseTarget := LbEnd;

  EmitMarkCmd(LbEnd, Stmt.Coord);
end;

procedure TCodePack.SetupStmt_For(Stmt: TForStmt);

  procedure AddForCompare(CtrlV, StopV: TSymbolOp; const BrLab: string; IsDown: Boolean);
  var
    Jump: TJumpCmd;
    CmpExpr: TBinaryOp;
  begin
    if IsDown then
      CmpExpr := CreateBinaryOp(opcGE, StopV.Coord)
    else
      CmpExpr := CreateBinaryOp(opcLE, StopV.Coord);

    CmpExpr.Left := CreateSymbolOp(CtrlV.Reference, CtrlV.Coord);
    CmpExpr.Right := CreateSymbolOp(StopV.Reference, StopV.Coord);
    CmpExpr.Typ := FContext.FBooleanType;

    Jump := TJumpCmd(CreateCmd(TJumpCmd, StopV.Coord));
    Jump.Condition := CmpExpr;
    Jump.TrueTarget := LabelStr('forloop');
    Jump.FalseTarget := BrLab;
    AddCmd(Jump);

    EmitMarkCmd(Jump.TrueTarget, Jump.Coord);
  end;

  procedure AddInc(CtrlV: TSymbolOp; IsDown: Boolean);
  var
    Op: TBinaryOp;
    IncProc: TSymbol;
    S: TCallCmd;
    Delta: Integer;
  begin
    Op := CreateBinaryOp(opcCALL, CtrlV.Coord);

    IncProc := FContext.FSystemUnit.FindSymbol('inc');
    Assert(IncProc <> nil);
    Op.Left := CreateSymbolOp(IncProc, CtrlV.Coord);
    Op.Right := CreateListOp(CtrlV.Coord);
    TListOp(Op.Right).Add(CreateSymbolOp(CtrlV.Reference, CtrlV.Coord));
    if IsDown then
      Delta := -1 else
      Delta := 1;
    TListOp(Op.Right).Add(CreateConstOp(CtrlV.Coord, Delta));
    Op.Typ := FContext.FUntype;

    S := TCallCmd(CreateCmd(TCallCmd, Op.Coord));
    S.CallOp := Op;
    AddCmd(S);
  end;

var
  AsgStmt: TAssignCmd;
  CtrlV, StopV: TSymbolOp;
  OldIndex: Integer;
  LbPre, LbStart, LbBr, LbCont, OldBr, OldCont: string;
begin
  Inc(FLevelID);
  LbPre := Format('for%d', [FLevelID]);

  LbStart := LbPre + '.begin';
  LbBr := LbPre + '.br';
  LbCont := LbPre + '.cont';
  {
    for i := start_expr to stop_expr do
    begin
      if i > 10 then break;
      if i < 10 then continue;
    end;

    ==>

    i := start_expr;
    stopv := stop_expr;
  for_start:
    jumpfalse i <= stopv, for_br
      jumptrue i > 10, for_br;
      jumptrue i < 10, for_cont;
  for_cont:
    inc(i);
    jump for_start;
  for_br:
  }

  CtrlV := CreateSymbolOp(Stmt.Value, Stmt.Coord);
  StopV := CreateSymbolOp(Self.AddVar(CtrlV.Typ), Stmt.Stop.Coord);
  AsgStmt := TAssignCmd(CreateCmd(TAssignCmd, Stmt.Start.Coord));
  AsgStmt.Left := CtrlV;
  AsgStmt.Right := EmitExpr(Stmt.Start);
  AddCmd(AsgStmt);

  AsgStmt := TAssignCmd(CreateCmd(TAssignCmd, StopV.Coord));
  AsgStmt.Left := StopV;
  AsgStmt.Right := EmitExpr(Stmt.Stop);
  AddCmd(AsgStmt);

  // label start:
  EmitMarkCmd(LbStart, Stmt.Coord);

  // compare statement
  AddForCompare(CtrlV, StopV, LbBr, stmt.Down);

  OldBr := FBreakLabel;
  OldCont := FContinueLabel;
  OldIndex := FCleanupIndex;
  FBreakLabel := LbBr;
  FContinueLabel := LbCont;
  FCleanupIndex := FCleanupStack.Count;

  // for statement's body
  SetupStmt(Stmt.Stmt);

  // Inc ctrl var, then jump to label start
  EmitMarkCmd(LbCont, Stmt.Coord);

  AddInc(CtrlV, stmt.Down);
  EmitGotoCmd(LbStart, Stmt.Coord);

  // 这是Break用的标号
  EmitMarkCmd(LbBr, Stmt.Coord);

  FBreakLabel := OldBr;
  FContinueLabel := OldCont;
  FCleanupIndex := OldIndex;
end;

procedure TCodePack.SetupStmt_If(Stmt: TIfStmt);
var
  E: TBaseOp;
  S: TJumpCmd;
  lbEnd, lbPre: string;
begin
  Inc(FLevelID);
  lbPre := Format('if%d', [FLevelID]);

  E := EmitExpr(Stmt.Value);

  S := TJumpCmd(CreateCmd(TJumpCmd, Stmt.Coord));
  S.Condition := E;
  S.TrueTarget := lbPre + '.true';
  S.FalseTarget := lbPre + '.false';
  AddCmd(S);
  EmitMarkCmd(S.TrueTarget, Stmt.Coord);

  SetupStmt(Stmt.TrueStmt);

  if Stmt.FalseStmt <> nil then
  begin
    lbEnd := lbPre + '.end';
    EmitGotoCmd(lbEnd, Stmt.Coord);
  end;

  EmitMarkCmd(S.FalseTarget, Stmt.Coord);

  if Stmt.FalseStmt <> nil then
  begin
    SetupStmt(Stmt.FalseStmt);
    EmitMarkCmd(lbEnd, Stmt.Coord);
  end;
end;

procedure TCodePack.SetupStmt_Raise(Stmt: TRaiseStmt);
var
  Cmd: TCmd;
  ExObj: TBaseOp;
begin
  if Stmt.Expr = nil then
  begin
    Cmd := CreateCmd(TReraiseCmd, Stmt.Coord);
    TReraiseCmd(Cmd).ExceptVar := FExceptVar.Name;
    AddCmd(Cmd);
  end
  else
  begin
    ExObj := EmitExpr(Stmt.Expr);
    Cmd := CreateCmd(TRaiseCmd, Stmt.Coord);
    AddCmd(Cmd);
    TRaiseCmd(Cmd).Exception := ExObj;
  end;
end;

procedure TCodePack.SetupStmt_Repeat(Stmt: TRepeatStmt);
var
  Jump: TJumpCmd;
  OldBr, OldCont, LbStart, LbStop, LbPre: string;
  OldIndex: Integer;
begin
  Inc(FLevelID);
  LbPre := Format('repeat%d', [FLevelID]);
  LbStart := lbPre + '.begin';
  LbStop := lbPre + '.end';

  EmitMarkCmd(LbStart, Stmt.Coord);

  OldBr := FBreakLabel;
  OldCont := FContinueLabel;
  OldIndex := FCleanupIndex;
  FBreakLabel := LbStop;
  FContinueLabel := LbStart;
  FCleanupIndex := FCleanupStack.Count;

  // Loop body
  SetupStmt(Stmt.Stmt);

  // Until statement
  Jump := TJumpCmd(CreateCmd(TJumpCmd, Stmt.Coord));
  Jump.Condition := EmitExpr(Stmt.Condition);
  Jump.TrueTarget := LbStop;
  Jump.FalseTarget := LbStart;
  AddCmd(Jump);

  EmitMarkCmd(LbStop, Stmt.Coord);

  FBreakLabel := OldBr;
  FContinueLabel := OldCont;
  FCleanupIndex := OldIndex;
end;

procedure TCodePack.SetupStmt_Try(Stmt: TTryStmt);

  function MakeIsExceptExpr(Handler: TExceptHandler; V: TVariable): TExpr;
  var
    E: TBinaryExpr;
    L, R: TSymbolExpr;
  begin
    E := TBinaryExpr(CreateNode(TBinaryExpr));
    L := TSymbolExpr(CreateNode(TSymbolExpr));
    R := TSymbolExpr(CreateNode(TSymbolExpr));
    L.Reference := V;
    L.Typ := V.VarType;
    L.OpCode := opSYMBOL;
    R.Reference := Handler.ExceptVar.VarType;
    R.Typ := Handler.ExceptVar.VarType;
    R.OpCode := opSYMBOL;

    E.OpCode:= opIS;
    E.Left := L;
    E.Right := R;
    E.Typ := FContext.FTrueConst.ConstType;
    Result := E;
  end;

  function MakeOnExceptStmt(Handler: TExceptHandler; V: TVariable): TCompoundStmt;
  var
    S: TAssignmentStmt;
    L, R: TSymbolExpr;
  begin
    Result := TCompoundStmt(CreateNode(TCompoundStmt));
    S := TAssignmentStmt(CreateNode(TAssignmentStmt));
    // 构造: exceptvar := V
    L := TSymbolExpr(CreateNode(TSymbolExpr));
    R := TSymbolExpr(CreateNode(TSymbolExpr));

    L.Reference := Handler.ExceptVar;
    L.Typ := Handler.ExceptVar.VarType;
    L.OpCode := opSYMBOL;
    R.Reference := V;
    R.Typ := V.VarType;
    R.OpCode := opSYMBOL;

    S.Left := L;
    S.Right := R;
    Result.Statements.Add(S);
    if Handler.Stmt <> nil then // Stmt may be nil
      Result.Statements.Add(Handler.Stmt);
  end;

  function MakeReraiseStmt: TStatement;
  begin
    Result := TRaiseStmt(CreateNode(TRaiseStmt));
  end;

  function MakeExceptStmt(ExBlock: TExceptBlock): TStatement;
  var
    i: Integer;
    S: array of TIfStmt;
    Handler: TExceptHandler;
  begin
    if ExBlock.Count = 0 then
    begin
      Result := ExBlock.Default;
      Exit;
    end;

    SetLength(S, ExBlock.Count);
    for i := 0 to ExBlock.Count-1 do
    begin
      S[i] := TIfStmt(CreateNode(TIfStmt));
      Handler := ExBlock.ExceptHandlers[i];
      Inc(FVarID);
      with Handler.ExceptVar do
      begin
        // 改名避免冲突
        Name := '$' + Name + IntToStr(FVarID);
        Parent := nil;
        Level := FCurFunc.Level;
        Include(Attr, saUsed);
      end;
      //FCurFunc.LocalSymbols.Add(Handler.ExceptVar);
      FCurCode.Vars.Add(Handler.ExceptVar);
      S[i].Value := MakeIsExceptExpr(Handler, FExceptVar);
      S[i].TrueStmt := MakeOnExceptStmt(Handler, FExceptVar);
    end;

    // 把if语句串起来, 形成：
    // if $e is EMyError then
    //   domyerror($e)
    // else if $e is EFailed then
    //   dofailed($e)
    // else
    //   doelse;
    for i := 0 to ExBlock.Count - 2 do
    begin
      S[i].FalseStmt := S[i + 1];
    end;
    S[Length(S) - 1].FalseStmt := ExBlock.Default;
    if ExBlock.Default = nil then
    begin
      // 抛出
      S[Length(S) - 1].FalseStmt := MakeReraiseStmt;
    end;
    Result := S[0];
  end;

  function EmitCallFreeStmt: TCallStmt;
  var
    Call: TBinaryExpr;
    L, Arg: TSymbolExpr;
    R: TListExpr;
  begin
    Call := TBinaryExpr(CreateNode(TBinaryExpr));
    L := TSymbolExpr(CreateNode(TSymbolExpr));
    L.Reference := FContext.GetSystemRoutine(srFreeAndNil);
    L.Typ := FContext.FUntype;
    L.OpCode := opSYMBOL;
    Arg := TSymbolExpr(CreateNode(TSymbolExpr));
    Arg.Reference := Self.FExceptVar;
    Arg.Typ := Self.FexceptVar.VarType;
    Arg.OpCode := opSYMBOL;

    R := TListExpr(CreateNode(TListExpr));
    R.Add(Arg);
    R.Typ := FContext.FUntype;

    Call.Left := L;
    Call.Right := R;
    Call.OpCode := opCALL;
    Call.Typ := FContext.FUntype;

    Result := TCallStmt(CreateNode(TCallStmt));
    Result.CallExpr := Call;
  end;

  procedure EmitExceptCmds(ExBlock: TExceptBlock);
  var
    S: TStatement;
  begin
    S := MakeExceptStmt(ExBlock);
    if S = nil then
      S := TEmptyStmt(CreateNode(TEmptyStmt));
    SetupStmt(S);
  end;

{  procedure EmitExceptCmds(ExBlock: TExceptBlock);
  var
    S: TStatement;
    C: TCompoundStmt;
  begin
    S := MakeExceptStmt(ExBlock);
    C := TCompoundStmt(CreateNode(TCompoundStmt));
    if S <> nil then
      C.Statements.Add(S);
    C.Statements.Add(EmitCallFreeStmt);
    SetupStmt(C);
  end;}

  procedure AttachExceptCmds(List: TList; Start: Integer);
  var
    i: Integer;
  begin
    for i := Start to FCurCode.Cmds.Count - 1 do
    begin
      List.Add(FCurCode.Cmds[i]);
    end;
    for i := FCurCode.Cmds.Count - 1 downto Start do
      FCurCode.Cmds.Delete(i);
  end;

var
  LFunc: TFunction;
  LIns: TCmd;
  Start: Integer;
  OldVar: TVariable;
begin
  // finally 中不可以有直接的 Break Continue Exit ，所以可以单独一个函数
  if Stmt.FinallyStmt <> nil then
  begin
    CreateExPtrVar;
    // 创建一个函数，把 finally 代码移至这个函数。
    // 因为finally块需要在 Exit,Break,continue中调用，做成函数比较方便
    LFunc := CreateFunc;
    LFunc.Parent := FCurFunc;
    LFunc.Level := FCurFunc.Level + 1;
    LFunc.Attr := [saInternal];

    Inc(FFuncID);
    LFunc.Name := FCurFunc.Name + 'fin.' + IntToStr(FFuncID);
    LFunc.StartStmt := Stmt.FinallyStmt;
    FCurCode.Funcs.Add(LFunc);
    FCleanupStack.Add(LFunc);

    LIns := CreateCmd(TCleanupCmd, Stmt.FinallyStmt.Coord);
    TCleanupCmd(LIns).CleanupProc := LFunc;
    AddCmd(LIns);
  end;

  // 要考虑到如果在 except 中有个 Exit Break Continue ，
  // 如何实现跳转
  if Stmt.ExceptBlock <> nil then
  begin
    Inc(FExceptCount);
    LIns := CreateCmd(THandleExceptCmd, Stmt.Coord);
    AddCmd(LIns);

    CreateExPtrVar;
    OldVar := FExceptVar;
    FExceptVar := CreateExVar;

    if FExceptVarStack = nil then FExceptVarStack := TList.Create;
    FExceptVarStack.Add(FExceptVar);

    try
      THandleExceptCmd(LIns).ExceptVar := FExceptVar.Name;
      THandleExceptCmd(LIns).Level := FExceptCount;

      Start := FCurCode.Cmds.Count;
      EmitExceptCmds(Stmt.ExceptBlock);
      AttachExceptCmds(THandleExceptCmd(LIns).Cmds, Start);
    finally
      FExceptVar := OldVar;
      FExceptVarStack.Delete(FExceptVarStack.Count-1);
    end;
  end;

  SetupStmt(Stmt.Stmt);

  if Stmt.FinallyStmt <> nil then
  begin
    EmitCallCmd(TFunctionDecl(FCleanupStack.Last), Stmt.FinallyStmt.Coord, []);
    FCleanupStack.Delete(FCleanupStack.Count-1);
    LIns := CreateCmd(TLeaveBlockCmd, Stmt.FinallyStmt.Coord);
    AddCmd(LIns);
  end;
  if Stmt.ExceptBlock <> nil then
  begin
    Dec(FExceptCount);
    LIns := CreateCmd(TLeaveBlockCmd, Stmt.Coord);
    AddCmd(LIns);
  end;
end;

procedure TCodePack.SetupStmt_While(Stmt: TWhileStmt);
var
  Jump: TJumpCmd;
  OldBr, OldCont, LbStart, LbStop, LbPre: string;
  OldIndex: Integer;
begin
  Inc(FLevelID);
  LbPre := Format('while%d', [FLevelID]);

  LbStart := LbPre + '.begin';
  LbStop := LbPre + '.end';

  EmitMarkCmd(LbStart, Stmt.Coord);

  Jump := TJumpCmd(CreateCmd(TJumpCmd, Stmt.Coord));
  Jump.Condition := EmitExpr(Stmt.Condition);
  Jump.TrueTarget := LbPre + '.body';
  Jump.FalseTarget := LbStop;
  AddCmd(Jump);

  EmitMarkCmd(Jump.TrueTarget, Stmt.Coord);

  // loop body
  OldBr := FBreakLabel;
  OldCont := FContinueLabel;
  OldIndex := FCleanupIndex;
  FBreakLabel := LbStop;
  FContinueLabel := LbStart;
  FCleanupIndex := FCleanupStack.Count;

  SetupStmt(Stmt.Stmt);

  EmitMarkCmd(LbStop, Stmt.Coord);

  FBreakLabel := OldBr;
  FContinueLabel := OldCont;
  FCleanupIndex := OldIndex;
end;

{ TCode }

constructor TCode.Create;
begin
  Cmds := TList.Create;
  Vars := TList.Create;
//  Consts := THashTable.Create(100, True);
  Funcs := TList.Create;
end;

destructor TCode.Destroy;
begin
  Cmds.Free;
  Vars.Free;
//  Consts.Free;
  Funcs.Free;
  inherited;
end;

end.

