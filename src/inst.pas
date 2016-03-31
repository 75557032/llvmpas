unit inst;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses SysUtils, Classes, ast;

type
  TOpCode = (
    opcNone,
    opcNE, opcEQ, opcLT, opcLE, opcGT, opcGE, opcIN, opcIS, opcAS,
    opcADD, opcSUB, opcOR, opcXOR,
    opcMUL, opcFDIV, opcIDIV, opcMOD, opcAND, opcSHL, opcSHR,
    opcMEMBER, // eg. SomeObj.A, the A is member of SomeObj
    opcCAST,   // Integer(a);
    opcCALL,   // function call
    opcRANGE,  // .. subrange? maybe not use
    opcINDEX,  // [] array index
    opcASSIGN, // :=
    opcNOT, opcNEG,
    opcSET,  // [] set / open array constructor
    opcADDR, // @ get address
    opcINST, // ^ dereference
//    opcVARCALL,
    opcLIST, // express list, seperated by comma
    opcNIL,
    opcCONST,
    opcSymbol,
    opcCallSpecial,
    opcCallBuiltin,
    opcProcAddr,  // operand is proc/method symbol
    opcAddrOffset, // calc a pointer offset,
                   // left node is a pointer(obj,intf,...),
                   // right node is number of bytes to offset
    opcLoad   // unary. load byte/word/longint from pointer, operand is pointer
  );

  TOPRAttribute = (
    oprConst,
    oprVarCast,
    oprVarSet,     // 大小可变的变量
    oprInherited,  // 调用基类方法（仅opcCall）
    oprCtorInner,  // 内层ctor调用（仅opcCall）
    oprDtorInner   // 内层dtor调用（仅opcCall）
  );
  TOPRAttributes = set of TOPRAttribute;

  TCmd = class;

  TCmdList = array of TCmd;

  TNode = class
  public
    Coord: TAstNodeCoord;
    constructor Create; virtual;
  end;

  { TBaseOp }

  TBaseOp = class(TNode)
  private
    FCmds: TCmdList;
    FCmdCount: Integer;
    FParent: TBaseOp;
    FTyp: TType;
    FOpCode: TOpCode;
  public
    Switches: TCodeSwitches;
    Attr: TOPRAttributes;
    property OpCode: TOpCode read FOpCode write FOpCode;
    property Parent: TBaseOp read FParent;
    property Typ: TType read FTyp write FTyp;
    property Cmds: TCmdList read FCmds;
    property CmdCount: Integer read FCmdCount;
    procedure AddCmd(Cmd: TCmd);
    // Remove child node
    procedure Remove(Child: TBaseOp); virtual;
    // Remove self from parent
    procedure Detach;

    function GetReference: TSymbol;
    function GetConstantSymbol: TConstant;
    function IsTypeSymbol: Boolean;
    function IsConstSymbol: Boolean;
    function IsEmptyStr: Boolean;
    function IsFunctionSymbol: Boolean;
  end;

  TUnaryOp = class(TBaseOp)
  private
    FOperand: TBaseOp;
    procedure SetOperand(const Value: TBaseOp);
  public
    property Operand: TBaseOp read FOperand write SetOperand;
    procedure Remove(Child: TBaseOp); override;
  end;

  TBinaryOp = class(TBaseOp)
  private
    FLeft: TBaseOp;
    FRight: TBaseOp;
    procedure SetLeft(const Value: TBaseOp);
    procedure SetRight(const Value: TBaseOp);
  public
    property Left: TBaseOp read FLeft write SetLeft;
    property Right: TBaseOp read FRight write SetRight;
    procedure Remove(Child: TBaseOp); override;
  end;

  TCallKind = (
    callFunc, callMethod,
    callBuiltin, callNestFunc,
    callFuncPtr, callMethodPtr
  );
  TCallAttributes = set of (caVirtual, caClass, caSpecial);

  // 未使用
  TCallOp = class(TBinaryOp)
  public
    SpecialVar: TVariable;
    Kind: TCallKind;
    CallAttr: TCallAttributes;
  end;

  TLoadType = (ldInt8, ldInt16, ldInt32, ldInt64, ldPtr);
  // 未使用
  TLoadOp = class(TUnaryOp)
  public
    LoadType: TLoadType;
    Align: Byte; // 1..8
  end;

  { TListOp }

  TListOp = class(TBaseOp)
  public
    Items: array of TBaseOp;
    Count: Integer;
    constructor Create; override;
    procedure Add(E: TBaseOp);
    procedure Insert(Index: Integer; E: TBaseOp);
    procedure Delete(Index: Integer);
    procedure Remove(E: TBaseOp); override;
    function IndexOf(E: TBaseOp): Integer;
    procedure Replace(Index: Integer; E: TBaseOp);
    procedure SetCapacity(Num: Integer);
  end;

  TSymbolOp = class(TBaseOp)
  public
    Reference: TSymbol;
    Name: string;
    constructor Create; override;
  end;

  TConstOp = class(TBaseOp)
  public
    Value: TValueRec;
    constructor Create; override;
  end;

  TCmdKind = (
      insCall,
      insAssign,
      insSwitch,
      insCleanup,          // 清理
      insHandleExcept,     // 普通异常
      insHandleCtorExcept, // 处理构造函数最外围异常
      insHandleScExcept,   // 处理safecall函数最外围异常
      insLeaveBlock,       // 生成一个标签，指示以下是正常代码
      insEndExcept,        // 中止异常处理（例如在异常处理中使用Exit,Continue,Break跳出）
      insOAInit,
      insInitVar,
      insUninitVar,
      insMark,
      insGoto,
      insJump,
      insRaise,
      insReraise,
      insSetMem
    );

  TCmd = class(TNode)
  private
    FKind: TCmdKind;
  public
    property Kind: TCmdKind read FKind;
  end;

  TCmdClass = class of TCmd;

  TCallCmd = class(TCmd)
  public
    CallOp: TBinaryOp;
    constructor Create; override;
  end;

  TAssignCmd = class(TCmd)
  public
    Left, Right: TBaseOp;
    constructor Create; override;
  end;

  TSwitchEntry = record
    Value: Int64;
    Target: string;
  end;

  TSwitchEntryList = array of TSwitchEntry;

  { TSwitchCmd }

  TSwitchCmd = class(TCmd)
  private
    FEntries: TSwitchEntryList;
    FEntryCount: Integer;
  public
    OtherwiseTarget: string;
    Value: TBaseOp;
    constructor Create; override;

    procedure AddEntry(Value: Int64; const Target: string);
    property Entries: TSwitchEntryList read FEntries;
    property EntryCount: Integer read FEntryCount;
  end;

  TJumpCmd = class(TCmd)
  public
    Condition: TBaseOp;
    TrueTarget, FalseTarget: string;
    constructor Create; override;
  end;

  TGotoCmd = class(TCmd)
  public
    Target: string;
    constructor Create; override;
  end;

  TMarkCmd = class(TCmd)
  public
    LabelName: string;
    constructor Create; override;
  end;

  { TUninitVarCmd }

  TUninitVarCmd = class(TCmd)
  public
    Count: Integer;
    Variables: array of TSymbol;
    constructor Create; override;
    procedure Add(V: TSymbol);
  end;

  { TCleanupCmd }

  TCleanupCmd = class(TCmd)
  public
    OutterLPad: string; // 外围的landingpad 符号
    CleanupProc: TFunction;
    constructor Create; override;
  end;

  { THandleExceptCmd }

  THandleExceptCmd = class(TCmd)
  public
//    Handler: TFunction;
    OutterLPad, ExceptVar: string; // 外围的landingpad 符号
    Cmds: TList;
    constructor Create; override;
    destructor Destroy; override;
  end;

  THandleCtorExceptCmd = class(TCmd)
  public
    constructor Create; override;
  end;

  THandleScExceptCmd = class(TCmd)
  public
    constructor Create; override;
  end;

  { TLeaveBlockCmd }

  TLeaveBlockCmd = class(TCmd)
  public
    constructor Create; override;
  end;

  { TEndExceptCmd }

  TEndExceptCmd = class(TCmd)
  public
    constructor Create; override;
  end;

  { TOAInitCmd }

  TOAInitCmd = class(TCmd)
  public
    ArrayVar: TVariable;
    Elements: array of TBaseOp;
    ElementCount: Integer;

    constructor Create; override;
    procedure Add(E: TBaseOp);
  end;

  { TRaiseCmd }

  TRaiseCmd = class(TCmd)
  public
    Exception: TBaseOp;
    constructor Create; override;
  end;

  { TReraiseCmd }

  TReraiseCmd = class(TCmd)
  public
    constructor Create; override;
  end;

  { TSetMemCmd }

  TSetMemCmd = class(TCmd)
  public
    Target: TBaseOp;
    Value: Int64;
    Align: Byte;

    constructor Create; override;
  end;

function OpMap(op: TExprOpCode): TOPCode;

implementation

function OpMap(op: TExprOpCode): TOPCode;
const
  Maps: array[TExprOpCode] of TOPCode =
    (
    opcNONE,
//    opNE, opEQ, opLT, opLE, opGT, opGE, opIN, opIS, opAS,
    opcNE, opcEQ, opcLT, opcLE, opcGT, opcGE, opcIN, opcIS, opcAS,
  //  opADD, opSUB, opOR, opXOR,
    opcADD, opcSUB, opcOR, opcXOR,
  //  opMUL, opFDIV, opIDIV, opMOD, opAND, opSHL, opSHR,
    opcMUL, opcFDIV, opcIDIV, opcMOD, opcAND, opcSHL, opcSHR,

  //  opMEMBER, opCAST, opCALL, opRANGE, opINDEX, opASSIGN,
    opcMEMBER, opcCAST, opcCALL, opcRANGE, opcINDEX, opcASSIGN,
  //  opNOT, opNEG, opPOS, opINHERITED, opSET, opLIST,
    opcNOT, opcNEG, opcNONE, opcNONE, opcSET, opcLIST,
  //  opADDR, opDBLADDR, opINST, opDISPCALL,
    opcADDR, opcADDR,    opcINST, opcNONE,
  //  opNIL, opCONST,
    opcNIL, opcCONST,
  //  opSYMBOL
    opcSYMBOL
    );
begin
  Result := Maps[op];
end;

{ TNode }

constructor TNode.Create;
begin

end;

{ TBaseOp }

procedure TBaseOp.AddCmd(Cmd: TCmd);
begin
  if FCmdCount >= Length(FCmds) then
    SetLength(FCmds, FCmdCount + 10);
  FCmds[FCmdCount] := Cmd;
  Inc(FCmdCount);
end;

procedure TBaseOp.Detach;
begin
  if FParent <> nil then FParent.Remove(Self);
end;

function TBaseOp.GetConstantSymbol: TConstant;
var
  Sym: TSymbol;
begin
  Sym := GetReference;
  if Sym.NodeKind = nkConstant then
    Result := TConstant(Sym)
  else
    Result := nil;
end;

function TBaseOp.GetReference: TSymbol;
begin
  case OpCode of
    opcSYMBOL: Result := TSymbolOp(Self).Reference;
    opcMEMBER:
      if TBinaryOp(Self).Right <> nil then
        Result := TBinaryOp(Self).Right.GetReference
      else
        Result := nil;
  else
    Result := nil;
  end;
end;

function TBaseOp.IsConstSymbol: Boolean;
var
  Sym: TSymbol;
begin
  Sym := GetReference;
  Result := (Sym <> nil) and (Sym.NodeKind = nkConstant);
end;

function TBaseOp.IsEmptyStr: Boolean;

  function IsEmptyVal(const V: TValueRec): Boolean;
  begin
    Result := (V.VT = vtEmpty) or ((V.VT in [vtStr, vtWStr]) and (AnsiString(V.VStr) = ''));
  end;
var
  Sym: TSymbol;
begin
  Result := False;
  case OpCode of
    opcSYMBOL:
      begin
        Sym := TSymbol(TSymbolOp(Self).Reference);
        if (Sym <> nil) and (Sym.NodeKind = nkConstant) then
          Result := IsEmptyVal(TConstant(Sym).Value);
      end;
    opcConst:
      begin
        Result := IsEmptyVal(TConstOp(Self).Value);
      end;
  end;
end;

function TBaseOp.IsFunctionSymbol: Boolean;
var
  Sym: TSymbol;
begin
  Sym := GetReference;
  Result := (Sym <> nil) and (Sym.NodeKind in [nkFunc, nkMethod, nkExternalFunc]);
end;

function TBaseOp.IsTypeSymbol: Boolean;
var
  Sym: TSymbol;
begin
  Sym := GetReference;
  Result := (Sym <> nil) and (Sym.NodeKind = nkType);
end;

procedure TBaseOp.Remove(Child: TBaseOp);
begin
// do nothing
end;

{ TUnaryOp }

procedure TUnaryOp.Remove(Child: TBaseOp);
begin
  inherited;
  if FOperand = Child then
  begin
    FOperand := nil;
    Child.FParent := nil;
  end;
end;

procedure TUnaryOp.SetOperand(const Value: TBaseOp);
begin
  if FOperand = Value then Exit;

  if (Value <> nil) and (Value.Parent <> nil) then
    Value.Parent.Remove(Value);

  if FOperand <> nil then FOperand.FParent := nil;
  FOperand := Value;
  if FOperand <> nil then
    FOperand.FParent := Self;
end;

{ TBinaryOp }

procedure TBinaryOp.Remove(Child: TBaseOp);
begin
  inherited;
  if FLeft = Child then
  begin
    FLeft := nil;
    Child.FParent := nil;
  end
  else if FRight = Child then
  begin
    FRight := nil;
    Child.FParent := nil;
  end;
end;

procedure TBinaryOp.SetLeft(const Value: TBaseOp);
begin
  if Value = FLeft then Exit;

  if (Value <> nil) and (Value.Parent <> nil) then
    Value.Parent.Remove(Value);

  if FLeft <> nil then FLeft.FParent := nil;
  FLeft := Value;
  if FLeft <> nil then
    FLeft.FParent := Self;
end;

procedure TBinaryOp.SetRight(const Value: TBaseOp);
begin
  if Value = FRight then Exit;

  if (Value <> nil) and (Value.Parent <> nil) then
    Value.Parent.Remove(Value);

  if FRight <> nil then FRight.FParent := nil;;
  FRight := Value;
  if FRight <> nil then
    FRight.FParent := Self;
end;

{ TListOp }

procedure TListOp.Add(E: TBaseOp);
begin
  if E.Parent = Self then raise EASTError.Create('expr is in use');
  if Count >= Length(Items) then
    SetLength(Items, Count + 4);
  Items[Count] := E;
  Inc(Count);
  if Assigned(E.Parent) then E.Parent.Remove(E);
  E.FParent := Self;
end;

constructor TListOp.Create;
begin
  inherited Create;
  FOpCode := opcLIST;
end;

procedure TListOp.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index >= Count) then
    raise EAStError.Create('Index out of bound');

  Items[Index].FParent := nil;
  for i := Index + 1 to Count - 1 do
    Items[i - 1] := Items[i];
  Dec(Count);
end;

function TListOp.IndexOf(E: TBaseOp): Integer;
var
  I: Integer;
begin
  for i := 0 to Self.Count - 1 do
    if Items[i] = E then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TListOp.Insert(Index: Integer; E: TBaseOp);
var
  i: Integer;
begin
//  if E.Parent <>  nil then raise EASTError.Create('expr is in use');
  if E.Parent = Self then raise EASTError.Create('expr is in use');

  if (Index < 0) or (Index > Count) then
    raise EASTError.Create('Index out of bound');

  if Count >= Length(Items) then
    SetLength(Items, Count + 4);

  Inc(Count);
  for i := Index + 1 to Count - 1 do
    Items[i] := Items[i - 1];

  Items[Index] := E;
  if E.Parent <> nil then E.Parent.Remove(E);
  E.FParent := Self;
end;

procedure TListOp.Remove(E: TBaseOp);
var
  i: Integer;
begin
  inherited Remove(E);
  for i := 0 to Self.Count - 1 do
    if Items[i] = E then
    begin
      Delete(i);
      Exit;
    end;
end;

procedure TListOp.Replace(Index: Integer; E: TBaseOp);
begin
  if (Index < 0) or (Index >= Count) then
    raise EASTError.Create('Index out of bound');

  Items[Index].FParent := nil;
  Items[Index] := E;
  if E.Parent <> nil then E.Parent.Remove(E);
  E.FParent := Self;
end;

procedure TListOp.SetCapacity(Num: Integer);
begin
  if Num < Count then Exit;
  SetLength(Items, Num);
end;

{ TSymbolOp }

constructor TSymbolOp.Create;
begin
  inherited;
  FOpCode := opcSYMBOL;
end;

{ TConstOp }

constructor TConstOp.Create;
begin
  inherited;
  FOpCode := opcCONST;
end;

{ TCallCmd }

constructor TCallCmd.Create;
begin
  inherited;
  FKind := insCall;
end;

{ TAssignCmd }

constructor TAssignCmd.Create;
begin
  inherited;
  FKind := insAssign;
end;

{ TSwitchCmd }

procedure TSwitchCmd.AddEntry(Value: Int64; const Target: string);
begin
  if FEntryCount >= Length(FEntries) then
    SetLength(FEntries, FEntryCount + 10);
  FEntries[FEntryCount].Value := Value;
  FEntries[FEntryCount].Target := Target;
  Inc(FEntryCount);
end;

constructor TSwitchCmd.Create;
begin
  inherited;
  FKind := insSwitch;
end;

{ TJumpCmd }

constructor TJumpCmd.Create;
begin
  inherited;
  FKind := insJump;
end;

{ TGotoCmd }

constructor TGotoCmd.Create;
begin
  inherited;
  FKind := insGoto;
end;

{ TMarkCmd }

constructor TMarkCmd.Create;
begin
  inherited;
  FKind := insMark;
end;

{ TCleanupCmd }

constructor TCleanupCmd.Create;
begin
  inherited Create;
  FKind := insCleanup;
end;

{ THandleExceptCmd }

constructor THandleExceptCmd.Create;
begin
  inherited Create;
  FKind := insHandleExcept;
  Cmds := TList.Create;
end;

destructor THandleExceptCmd.Destroy;
begin
  Cmds.Free;
  inherited Destroy;
end;

{ THandleCtorExceptCmd }

constructor THandleCtorExceptCmd.Create;
begin
  inherited;
  FKind := insHandleCtorExcept;
end;

{ THandleScExceptCmd }

constructor THandleScExceptCmd.Create;
begin
  inherited;
  FKind := insHandleScExcept;
end;

{ TLeaveBlockCmd }

constructor TLeaveBlockCmd.Create;
begin
  inherited;
  FKind := insLeaveBlock;
end;

{ TEndExceptCmd }

constructor TEndExceptCmd.Create;
begin
  inherited;
  FKind := insEndExcept;
end;

{ TOAInitCmd }

procedure TOAInitCmd.Add(E: TBaseOp);
begin
  if ElementCount = Length(Elements) then
    SetLength(Elements, ElementCount + 10);
  Elements[ElementCount] := E;
  Inc(ElementCount);
end;

constructor TOAInitCmd.Create;
begin
  inherited Create;
  FKind := insOAInit;
end;

{ TUninitVarCmd }

procedure TUninitVarCmd.Add(V: TSymbol);
begin
  if Count = Length(Variables) then
    SetLength(Variables, Count + 4);
  Variables[Count] := V;
  Inc(Count);
end;

constructor TUninitVarCmd.Create;
begin
  inherited Create;
  FKind := insUninitVar;
end;

{ TReraiseCmd }

constructor TReraiseCmd.Create;
begin
  inherited Create;
  FKind := insReraise;
end;

{ TRaiseCmd }

constructor TRaiseCmd.Create;
begin
  inherited Create;
  FKind := insRaise;
end;

{ TSetMemCmd }

constructor TSetMemCmd.Create;
begin
  inherited Create;
  FKind := insSetMem;
end;

end.
