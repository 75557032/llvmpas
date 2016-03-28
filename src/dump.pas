unit dump;
// µ¼³öAST
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, ast;

type
  TDumpOption = (
    doDumpLoc, doDumpType, doDumpVar, doDumpConst,
    doDumpField, doDumpProp, doDumpFunc, doDumpFuncBody,
    doDumpLabel, doDumpMR
  );
  TDumpOptions = set of TDumpOption;

  TSimpleWriter = class
  public
    function BeginObject: TSimpleWriter; virtual; abstract;
    function EndObject: TSimpleWriter; virtual; abstract;
    function BeginArray: TSimpleWriter; virtual; abstract;
    function EndArray: TSimpleWriter; virtual; abstract;
    function Key(const name: string): TSimpleWriter; virtual; abstract;
    function Value(const value: string): TSimpleWriter; overload; virtual; abstract;
    function Value(const value: Double): TSimpleWriter; overload; virtual; abstract;
    function Value(const value: Integer): TSimpleWriter; overload; virtual; abstract;
    function Value(const value: Int64): TSimpleWriter; overload; virtual; abstract;
    function Value(const value: Boolean): TSimpleWriter; overload; virtual; abstract;
    function NullValue: TSimpleWriter; overload; virtual; abstract;
    function GetContent: string; virtual; abstract;
  end;

  { TDump }

  TDump = class
  private
    procedure DumpSym(s: TSymbol);
  protected
    FWriter: TSimpleWriter;
    FOptions: TDumpOptions;

    function SymbolAttrStr(attr: TSymbolAttributes): string;
    function VarAttrStr(attr: TVarAttributes): string;
    function VarStateStr(stat: TVarStates): string;
    function StructAttrStr(attr: TStructAttributes): string;
    function ClassAttrStr(attr: TClassAttributes): string;
    function ObjectAttrStr(attr: TObjectAttributes): string;
    function ExprAttrStr(attr: TExprAttributes): string;
    function CodeSwitchStr(s: TCodeSwitches): string;
    function IntfPropAttrStr(attr: TIntfPropertyAttributes): string;
    function PropAttrStr(attr: TPropertyAttributes): string;

    procedure DumpVal(const V: TValueRec);
    procedure DumpExpr(E: TExpr);
    procedure DumpSymBase(S: TSymbol);
    procedure DumpType(T: TType);
    procedure DumpFunc(F: TFunctionDecl);
    procedure DumpParam(A: TFuncParam);
    procedure DumpVar(V: TVariable);
    procedure DumpConst(C: TConstant);
    procedure DumpField(F: TField);
    procedure DumpProp(P: TProperty);
    procedure DumpIntfProp(P: TIntfProperty);
    procedure DumpLabel(Lab: TStmtLabel);
    procedure DumpMR(MR: TMethodResolution);
    procedure DumpAccessor(Acssr: TMultiAccessor);
  public
    function Dump(M: TModule; Options: TDumpOptions): string;
  end;

  { TJsonDump }

  TJsonDump = class(TDump)
  public
    constructor Create;
    destructor Destroy; override;
  end;

const
  DumpAllOptions = [Low(TDumpOption)..High(TDumpOption)];
implementation

const
  NodeKNames: array[TAstNodeKind] of string = (
    // nkSymbol, nkExpr, nkType, nkNameScope, nkModule,
    'symbol', 'expr', 'type', 'namepart', 'module',
    // nkVariable, nkConstant, nkField, nkProperty, nkIntfProperty,
    'var', 'const', 'field', 'prop', 'intfprop',
    // nkMethod, nkMethodResolution, nkFuncParam, nkEnumElement,
    'method', 'methreso', 'param', 'enumel', 'func',
    // nkFunc, nkExternalFunc, nkBuiltinFunc, nkAccessor,
    'externalfunc', 'builtinfunc', 'accessor',
    // nkLabel, nkStmt, nkSymbolOffset
    'label', 'stmt', 'symoffset'
  );

  CCNames: array[TCallingConvention] of string = (
  //(ccDefault, ccRegister, ccPascal, ccCDecl, ccStdCall, ccSafeCall)
    'default', 'register', 'pascal', 'cdecl', 'stdcall', 'safecall'
  );

  MethKNames: array[TMethodKind] of string = (
    'normal', 'ctor', 'dtor', 'objctor', 'objdtor', 'recctor', 'recdtor'
  );

  ObjKNames: array[TObjectKind] of string = (
    'class', 'object', 'record'
  );

  OpStr: array[TExprOpCode] of string = (
    'none',
//  opNE, opEQ, opLT, opLE, opGT, opGE, opIN, opIS, opAS,
    '<>', '=', '<', '<=', '>', '>=', 'in', 'is', 'as',
//  opADD, opSUB, opOR, opXOR, opMUL, opDIV, opIDIV,
    '+', '-', 'or', 'xor', '*', '/', 'div',
//  opMOD, opAND, opSHL, opSHR,
    'mod', 'and', 'shl', 'shr',
//  opMEMBER, opCAST, opCALL, opRANGE, opINDEX, opASSIGN, opNOT, opNEG, opPOS,
    'memberof', 'cast', 'call', 'range', 'index', ':=', 'not', 'neg', 'pos',
// opINHERITED, opSET, opLIST, opADDR, opDBLADDR, opINST, opDISPCALL, opNIL,
    'inherited', 'set', 'list', '@',   '@@',      '^',    '.', 'nil',
// opCONST, opSYMBOL
    'const', 'sym'
  );

function JsStrEncode(const s: string): string;

  procedure Replace(var s: string; i: Integer; const c1, c2: Char);
  begin
    Insert(c1, s, i);
    Result[i + 1] := c2;
  end;

var
  i: Integer;
begin
  Result := s;
  for i := Length(s) downto 1 do
  begin
    case s[i] of
      #8: Replace(Result, i, '\', 'b');
      #9: Replace(Result, i, '\', 't');
      #10: Replace(Result, i, '\', 'n');
      #12: Replace(Result, i, '\', 'f');
      #13: Replace(Result, i, '\', 'r');
      '"':Replace(Result, i, '\', '"');
      '\':Replace(Result, i, '\', '\');
      '/':Replace(Result, i, '\', '/');
    end;
  end;
  Result := '"' + Result + '"';
end;

type
  EJWError = class(Exception);

  TJWState = (jwsNone, jwsObject, jwsArray, jwsKey);
  TJWStates = set of TJWState;

  { TJsonWriter }

  TJsonWriter = class(TSimpleWriter)
  private
    FContent: string;
    FStates: array of TJWState;
    FStateCount: Integer;
    procedure PushState(S: TJWState);
    procedure PopState;
    function LastChar: Char;
    procedure RemoveLastChar;
    procedure JWError(const msg: string);
    function GetCurState: TJWState;
    property CurState: TJWState read GetCurState;
  public
    function BeginObject: TSimpleWriter; override;
    function EndObject: TSimpleWriter; override;
    function BeginArray: TSimpleWriter; override;
    function EndArray: TSimpleWriter; override;
    function Key(const name: string): TSimpleWriter; override;
    function Value(const value: string): TSimpleWriter; overload; override;
    function Value(const value: Double): TSimpleWriter; overload; override;
    function Value(const value: Integer): TSimpleWriter; overload; override;
    function Value(const value: Int64): TSimpleWriter; overload; override;
    function Value(const value: Boolean): TSimpleWriter; overload; override;
    function NullValue: TSimpleWriter; overload; override;
    function GetContent: string; override;
  end;

{ TJsonWriter }

function TJsonWriter.BeginArray: TSimpleWriter;
begin
  if not (CurState in [jwsNone, jwsArray, jwsKey]) then
    JWError('array not allow in here');
  FContent := FContent + '[';
  PushState(jwsArray);
  Result := Self;
end;

function TJsonWriter.BeginObject: TSimpleWriter;
begin
  if not (CurState in [jwsNone, jwsArray, jwsKey]) then
    JWError('array not allow in here');
  FContent := FContent + '{';
  PushState(jwsObject);
  Result := Self;
end;

function TJsonWriter.EndArray: TSimpleWriter;
begin
  if CurState <> jwsArray then
    JWError('State error, expect array');
  if LastChar = ',' then
    RemoveLastChar;
  FContent := FContent + '],';
  PopState;
  if CurState = jwsKey then PopState;
  Result := Self;
end;

function TJsonWriter.EndObject: TSimpleWriter;
begin
  if CurState <> jwsObject then
    JWError('State error, expect object');
  if LastChar = ',' then
    RemoveLastChar;
  FContent := FContent + '},';
  PopState;
  if CurState = jwsKey then PopState;
  Result := Self;
end;

function TJsonWriter.GetContent: string;
begin
  if CurState <> jwsNone then
    JWError('State error, expect none');
  if LastChar = ',' then
    RemoveLastChar;
  Result := FContent;
end;

function TJsonWriter.GetCurState: TJWState;
begin
  if FStateCount = 0 then
    Result := jwsNone
  else
    Result := FStates[FStateCount-1];
end;

procedure TJsonWriter.JWError(const msg: string);
begin
  raise EJWError.Create(msg);
end;

function TJsonWriter.Key(const name: string): TSimpleWriter;
begin
  if CurState <> jwsObject then
    JWError('key not allow in here');
  FContent := FContent + JsStrEncode(name) + ':';
  PushState(jwsKey);
  Result := Self;
end;

function TJsonWriter.LastChar: Char;
begin
  if FContent = '' then
    Result := #0
  else
    Result := FContent[Length(fContent)];
end;

function TJsonWriter.NullValue: TSimpleWriter;
begin
  if CurState <> jwsKey then
    JWError('expect key');
  FContent := FContent + 'null,';
  PopState;
  Result := Self;
end;

procedure TJsonWriter.PopState;
begin
  if FStateCount > 0 then
  begin
    Dec(FStateCount);
  end;
end;

procedure TJsonWriter.PushState(S: TJWState);
begin
  if FStateCount = Length(FStates) then
    SetLength(FStates, FStateCount + 4);
  FStates[FStateCount] := S;
  Inc(FStateCount);
end;

procedure TJsonWriter.RemoveLastChar;
begin
  Delete(fContent, Length(FContent), 1);
end;

function TJsonWriter.Value(const value: Boolean): TSimpleWriter;
begin
  if not (CurState in [jwsKey, jwsArray]) then
    JWError('TJsonWriter.Value: expect key');
  if value then
    FContent := FContent + 'true,'
  else
    FContent := FContent + 'false,';
  if CurState <> jwsArray then
    PopState;
  Result := Self;
end;

function TJsonWriter.Value(const value: Double): TSimpleWriter;
begin
  if not (CurState in [jwsKey, jwsArray]) then
    JWError('TJsonWriter.Value: expect key');
  FContent := FContent + FloatToStr(value) + ',';
  if CurState <> jwsArray then
    PopState;
  Result := Self;
end;

function TJsonWriter.Value(const value: Int64): TSimpleWriter;
begin
  if not (CurState in [jwsKey, jwsArray]) then
    JWError('TJsonWriter.Value: expect key');
  FContent := FContent + IntToStr(value) + ',';
  if CurState <> jwsArray then
    PopState;
  Result := Self;
end;

function TJsonWriter.Value(const value: Integer): TSimpleWriter;
begin
  if not (CurState in [jwsKey, jwsArray]) then
    JWError('TJsonWriter.Value: expect key');
  FContent := FContent + IntToStr(value) + ',';
  if CurState <> jwsArray then
    PopState;
  Result := Self;
end;

function TJsonWriter.Value(const value: string): TSimpleWriter;
begin
  if not (CurState in [jwsKey, jwsArray]) then
    JWError('TJsonWriter.Value: expect key');
  FContent := FContent + JsStrEncode(value) + ',';
  if CurState <> jwsArray then
    PopState;
  Result := Self;
end;

{ TDump }

function TDump.ClassAttrStr(attr: TClassAttributes): string;
const
  AttrStr: array[TClassAttribute] of string = (
    'Sealed', 'Abstract', 'Rtti'
  );
var
  i: TClassAttribute;
begin
  Result := '';
  for i := Low(TClassAttribute) to High(TClassAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.CodeSwitchStr(s: TCodeSwitches): string;
const
  CSStr: array[cdBoolEval..cdSafeDivide] of string = (
    'BoolEval','IOChecks','OverflowChecks','RangeChecks','SafeDivide'
  );
var
  i: TCompilerDirective;
begin
  Result := '';
  for i := cdBoolEval to cdSafeDivide do
    if i in s then
      Result := Result + CSStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.Dump(M: TModule; Options: TDumpOptions): string;
var
  i: Integer;
  sym: TSymbol;
begin
  FOptions := Options;
  FWriter.BeginArray;
  for i := 0 to M.Symbols.Count-1 do
  begin
    sym := M.Symbols[i];
    DumpSym(sym);
  end;
  for i := 0 to M.InternalSymbols.Count-1 do
  begin
    sym := M.InternalSymbols[i];
    DumpSym(sym);
  end;
  FWriter.EndArray;
  Result := FWriter.GetContent;
end;

procedure TDump.DumpAccessor(Acssr: TMultiAccessor);
begin
  FWriter.BeginObject;
  DumpSymBase(Acssr);
  // todo 1: need
  FWriter.EndObject;
end;

procedure TDump.DumpConst(C: TConstant);
begin
  FWriter.BeginObject;
  DumpSymBase(C);
  FWriter.Key('isresstr').Value(C.IsResStr);
  FWriter.Key('typ');
  if C.ConstType.Name = '' then
    DumpType(C.ConstType)
  else
    FWriter.Value(C.ConstType.FullName);
  FWriter.Key('value');
  DumpVal(C.Value);
  FWriter.EndObject;
end;

procedure TDump.DumpExpr(E: TExpr);
  procedure WriteList(E: TListExpr);
  var
    i: Integer;
  begin
    FWriter.BeginArray;
    for i := 0 to E.Count-1 do
      DumpExpr(E.Items[i]);
    FWriter.EndArray;
  end;

begin
  if E = nil then
  begin
    FWriter.NullValue;
    Exit;
  end;

  FWriter.BeginObject;
  if doDumpLoc in FOptions then
    FWriter.Key('loc').Value(Format('%d,%d', [E.Coord.Row, E.Coord.Col]));
  FWriter.Key('op').Value(OpStr[E.OpCode]);
  FWriter.Key('attr').Value(ExprAttrStr(E.Attr));
  FWriter.Key('switches').Value(CodeSwitchStr(E.Switches));
  FWriter.Key('typ');
  if E.Typ.Name = '' then
    DumpType(E.Typ)
  else
    FWriter.Value(E.Typ.FullName);
  case ExprKinds[E.OpCode] of
    1: begin
      FWriter.Key('operand');
      DumpExpr(TUnaryExpr(E).Operand);
    end;
    2: begin
      FWriter.Key('left');
      DumpExpr(TBinaryExpr(E).Left);
      FWriter.Key('right');
      DumpExpr(TBinaryExpr(E).Right);
    end;
    5: begin
      FWriter.Key('items');
      WriteList(TListExpr(E));
    end;
  else
    case E.OpCode of
      opNIL, opCONST:
        begin
          FWriter.Key('value');
          DumpVal(TConstExpr(E).Value);
        end;
      opSYMBOL:
        begin
          FWriter.Key('ref').Value(TSymbolExpr(E).Reference.FullName);
        end;
    end;
  end;
  FWriter.EndObject;
end;

procedure TDump.DumpField(F: TField);
begin
  FWriter.BeginObject;
  DumpSymBase(F);
  FWriter.Key('typ');
  if F.FieldType.Name = '' then
    DumpType(F.FieldType)
  else
    FWriter.Value(F.FieldType.FullName);
  FWriter.Key('offset').Value(F.Offset);
  FWriter.EndObject;

end;

procedure TDump.DumpFunc(F: TFunctionDecl);
var
  i: Integer;
begin
  FWriter.BeginObject;
  DumpSymBase(F);

  FWriter.Key('level').Value(F.Level);
  FWriter.Key('cc').Value(CCNames[F.CallConvention]);
  FWriter.Key('return');
  if F.ReturnType = nil then
    FWriter.Value('void')
  else
    FWriter.Value(F.ReturnType.FullName);

  FWriter.Key('args');
  FWriter.BeginArray;
  for i := 0 to F.ParamCount - 1 do
    Self.DumpParam(F.Params[i]);
  FWriter.EndArray;

  FWriter.EndObject;
end;

procedure TDump.DumpIntfProp(P: TIntfProperty);
var
  i: Integer;
begin
  FWriter.BeginObject;
  DumpSymBase(P);
  FWriter.Key('propattr').Value(IntfPropAttrStr(P.PropAttr));
  FWriter.Key('typ').Value(p.PropType.FullName);
  if P.Getter = nil then
    FWriter.Key('getter').NullValue
  else
    FWriter.Key('getter').Value(P.Getter.Name);

  if P.Setter = nil then
    FWriter.Key('setter').NullValue
  else
    FWriter.Key('setter').Value(P.Setter.Name);

  FWriter.Key('dispid').Value(P.DispID);

  FWriter.Key('args');
  FWriter.BeginArray;
  for i := 0 to P.ParamCount - 1 do
    DumpParam(P.Params[i]);
  FWriter.EndArray;

  FWriter.EndObject;
end;

procedure TDump.DumpLabel(Lab: TStmtLabel);
begin
  FWriter.BeginObject;
  DumpSymBase(Lab);
  FWriter.EndObject;
end;

procedure TDump.DumpMR(MR: TMethodResolution);
begin
  FWriter.BeginObject;
  DumpSymBase(MR);
  FWriter.Key('intfmeth');
  DumpSym(MR.InterfaceMethod);
  FWriter.Key('implmeth');
  DumpSym(MR.ImplementingMethod);

  FWriter.EndObject;
end;

procedure TDump.DumpParam(A: TFuncParam);
begin
  FWriter.BeginObject;
  DumpSymBase(A);
  FWriter.Key('level').Value(A.Level);
  FWriter.Key('index').Value(A.Index);
  FWriter.Key('typ').Value(A.ParamType.FullName);
//  FWriter.Key('modifier');
//  FWriter.Key('states');
  FWriter.Key('def');
  DumpVal(A.DefaultValue);
  FWriter.EndObject;
end;

procedure TDump.DumpProp(P: TProperty);
var
  i: Integer;
begin
  FWriter.BeginObject;
  DumpSymBase(P);
  FWriter.Key('propattr').Value(PropAttrStr(P.PropAttr));
  FWriter.Key('typ').Value(p.PropType.FullName);
  if P.Getter = nil then
    FWriter.Key('getter').NullValue
  else
    FWriter.Key('getter').Value(P.Getter.Name);

  if P.Setter = nil then
    FWriter.Key('setter').NullValue
  else
    FWriter.Key('setter').Value(P.Setter.Name);

  if P.Stored = nil then
    FWriter.Key('stored').NullValue
  else
    FWriter.Key('stored').Value(P.Setter.Name);

  FWriter.Key('index').Value(P.Index);

  FWriter.Key('def');
  DumpVal(P.DefaultValue);

  FWriter.Key('args');
  FWriter.BeginArray;
  for i := 0 to P.ParamCount - 1 do
    DumpParam(P.Params[i]);
  FWriter.EndArray;

  FWriter.EndObject;
end;

procedure TDump.DumpSym(s: TSymbol);
  procedure DumpOvrldStub(F: TFunctionDecl);
  begin
    FWriter.BeginObject;
    FWriter.Key('name').Value(F.Name);
    FWriter.Key('ovrldindex').Value(F.ID);
    FWriter.EndObject;
  end;

begin
  case s.NodeKind of
    nkType:
      if doDumpType in FOptions then DumpType(TType(S));
    nkVariable:
      if doDumpVar in FOptions then DumpVar(TVariable(S));
    nkConstant:
      if doDumpConst in FOptions then DumpConst(TConstant(S));
    nkField:
      if doDumpField in FOptions then DumpField(TField(S));
    nkProperty:
      if doDumpProp in FOptions then DumpProp(TProperty(S));
    nkIntfProperty:
      if doDumpProp in FOptions then DumpIntfProp(TIntfProperty(S));
    nkMethod, nkFunc, nkExternalFunc:
      if doDumpFunc in FOptions then
      begin
        if fmOvrldFlag in TFunctionDecl(S).Modifiers then
          DumpOvrldStub(TFunctionDecl(S))
        else
          DumpFunc(TFunctionDecl(S));
      end;
    nkEnumElement: begin end;
    nkLabel:
      if doDumpLabel in FOptions then DumpLabel(TStmtLabel(S));
    nkMethodResolution:
      if doDumpMR in FOptions then DumpMR(TMethodResolution(S));
    nkAccessor:
      DumpAccessor(TMultiAccessor(S));
    nkBuiltinFunc: begin end;
    nkModule: begin end;
  else
    Assert(False, 'DumpSym: ' + s.Name + ',' + s.ClassName);
  end;
end;

procedure TDump.DumpSymBase(S: TSymbol);
const
  VisStr: array[TMemberVisibility] of string = (
    'Default', 'StrictPrivate', 'StrictProtected',
    'Private', 'Protected', 'Public', 'Published', 'Automated'
  );
begin
  if doDumpLoc in FOptions then
    FWriter.Key('loc').Value(Format('%d,%d', [S.Coord.Row, S.Coord.Col]));
  FWriter.Key('nk').Value(NodeKNames[S.NodeKind]);

  FWriter.Key('name').Value(S.Name);
  FWriter.Key('attr').Value(SymbolAttrStr(S.Attr));
  FWriter.Key('vis').Value(VisStr[S.Visibility]);
end;

procedure TDump.DumpType(T: TType);
  procedure DumpRecordBody(R: TRecordBody);
  var
    i: Integer;
    sym: TSymbol;
  begin
    FWriter.BeginArray;
    for i := 0 to R.Members.Count - 1 do
    begin
      sym := TSymbol(R.Members[i]);
      DumpSym(sym);
    end;
    if R.Selector <> nil then
      DumpField(R.Selector);
    if (R is TRecordVariant) and (TRecordVariant(R).Next <> nil) then
    begin
      DumpRecordBody(TRecordVariant(R).Next);
    end
    else if R.Variants <> nil then
      DumpRecordBody(R.Variants);
    FWriter.EndArray;
  end;

  procedure DumpRecordType(T: TRecordType);
  begin
    FWriter.Key('align').Value(T.GlobalAlignSize);
    FWriter.Key('recattr').Value(StructAttrStr(T.RecordAttr));
    FWriter.Key('body');
    DumpRecordBody(T.Body);
  end;

  procedure DumpClassType(T: TClassType);
  var
    i: Integer;
  begin
    FWriter.Key('align').Value(T.GlobalAlignSize);
    FWriter.Key('classattr').Value(ClassAttrStr(T.ClassAttr));
    if T.Base = nil then
      FWriter.Key('base').NullValue
    else
      FWriter.Key('base').Value(T.Base.FullName);
    FWriter.Key('objsize').Value(T.ObjectSize);
    if T.DefaultProp = nil then
      FWriter.Key('defprop').NullValue
    else
      FWriter.Key('defprop').Value(T.DefaultProp.Name);
    FWriter.Key('interfaces');
    if T.InterfaceCount = 0 then
      FWriter.NullValue
    else begin
      FWriter.BeginArray;
      for i := 0 to T.InterfaceCount - 1 do
      begin
        FWriter.Value(T.Interfaces[i].FullName);
      end;
      FWriter.EndArray;
    end;
    //FWriter.Key('vmt');

    FWriter.Key('members');
    FWriter.BeginArray;
    for i := 0 to T.Symbols.Count-1 do
    begin
      DumpSym(T.Symbols[i]);
    end;
    FWriter.EndArray;
  end;

  procedure DumpObjectType(T: TObjectType);
  var
    i: Integer;
  begin
    FWriter.Key('align').Value(T.GlobalAlignSize);
    FWriter.Key('objattr').Value(ObjectAttrStr(T.ObjectAttr));
    if T.Base = nil then
      FWriter.Key('base').NullValue
    else
      FWriter.Key('base').Value(T.Base.FullName);
    FWriter.Key('vmtentrycount').Value(T.VmtEntryCount);
    FWriter.Key('vmtoffset').Value(T.VmtOffset);

    FWriter.Key('members');
    FWriter.BeginArray;
    for i := 0 to T.Symbols.Count-1 do
    begin
      DumpSym(T.Symbols[i]);
    end;
    FWriter.EndArray;
  end;

  procedure DumpInterfaceType(T: TInterfaceType);
  var
    i: Integer;
  begin
    FWriter.Key('guid').Value(GuidToString(T.Guid));
    FWriter.Key('isdisp').Value(T.IsDisp);
    if T.Base = nil then
      FWriter.Key('base').NullValue
    else
      FWriter.Key('base').Value(T.Base.FullName);
    FWriter.Key('vmtentrycount').Value(T.VmtEntryCount);

    FWriter.Key('members');
    FWriter.BeginArray;
    for i := 0 to T.Symbols.Count-1 do
    begin
      DumpSym(T.Symbols[i]);
    end;
    FWriter.EndArray;
  end;

  procedure DumpEnumValue(E: TEnumValue);
  begin
    FWriter.BeginObject;
    DumpSymBase(E);
    FWriter.Key('value').Value(E.Value);
    FWriter.EndObject;
  end;

  procedure DumpEnumType(T: TEnumType);
  var
    i: Integer;
  begin
    FWriter.Key('minsize').Value(T.MinEnumSize);
    FWriter.Key('values');
    FWriter.BeginArray;
    for i := 0 to T.Values.Count - 1 do
      DumpEnumValue(TEnumValue(T.Values[i]));
    FWriter.EndArray;
  end;

  procedure DumpSubrangeType(T: TSubrangeType);
  begin
    FWriter.Key('begin').Value(T.RangeBegin);
    FWriter.Key('end').Value(T.RangeEnd);
    FWriter.Key('basetype');
    if T.BaseType.Name = '' then
      DumpType(T.BaseType)
    else
      FWriter.Value(T.BaseType.FullName);
  end;

  procedure DumpSetType(T: TSetType);
  begin
    FWriter.Key('range');
    if T.RangeType = nil then
      FWriter.NullValue
    else if T.RangeType.Name = '' then
      DumpSubrangeType(T.RangeType)
    else
      FWriter.Value(T.RangeType.FullName);
  end;

  procedure DumpStringType(T: TStringType);
  begin
    FWriter.Key('strkind').Value(StringTypeNames[T.Kind]);
    FWriter.Key('charcount').Value(T.CharCount);
    FWriter.Key('codepage').Value(T.CodePage);
  end;

  procedure DumpPointerType(T: TPointerType);
  begin
    FWriter.Key('ref');
    if T.IsUntypePointer then
      FWriter.Value('untype')
    else
      FWriter.Value(T.RefType.FullName);
  end;

  procedure DumpArrayType(T: TArrayType);
  begin
    FWriter.Key('elem');
    if T.ElementType.Name = '' then
      DumpType(T.ElementType)
    else
      FWriter.Value(T.ElementType.FullName);
    if T.Range.Name = '' then
      DumpSubrangeType(T.Range)
    else
      FWriter.Value(T.Range.FullName);
    FWriter.Key('arrayattr').Value(StructAttrStr(T.ArrayAttr));
  end;

  procedure DumpDynamicArrayType(T: TDynamicArrayType);
  begin
    FWriter.Key('elem');
    if T.ElementType.Name = '' then
      DumpType(T.ElementType)
    else
      FWriter.Value(T.ElementType.FullName);
  end;

  procedure DumpOpenArrayType(T: TOpenArrayType);
  begin
    FWriter.Key('elem');
    if T.ElementType.Name = '' then
      DumpType(T.ElementType)
    else
      FWriter.Value(T.ElementType.FullName);
    FWriter.Key('count').Value(T.ElementCount);
  end;

  procedure DumpAliasType(T: TAliasType);
  begin
    FWriter.Key('ref').Value(T.RefType.FullName);
  end;

  procedure DumpFileType(T: TFileType);
  begin
    FWriter.Key('elem');
    if T.ElementType = nil then
      FWriter.NullValue
    else if T.ElementType.Name = '' then
      DumpType(T.ElementType)
    else
      fWriter.Value(T.ElementType.FullName);
  end;

  procedure DumpProcType(T: TProceduralType);
  var
    i: Integer;
  begin
    FWriter.Key('cc').Value(CCNames[T.CallConvention]);
    FWriter.Key('mk').Value(MethKNames[T.MethodKind]);
    FWriter.Key('ok').Value(ObjKNames[T.ObjectKind]);
    FWriter.Key('ismeth').Value(T.IsMethodPointer);
    FWriter.Key('return');
    if T.ReturnType = nil then
      FWriter.Value('void')
    else
      FWriter.Value(T.ReturnType.FullName);

    FWriter.Key('args');
    FWriter.BeginArray;
    for i := 0 to T.ParamCount - 1 do
      Self.DumpParam(T.Params[i]);
    FWriter.EndArray;
  end;

begin
  FWriter.BeginObject;
  DumpSymBase(T);
  FWriter.Key('typecode').Value(TypeNames[T.TypeCode]);
  FWriter.Key('size').Value(T.Size);
  case T.TypeCode of
    typUnknown: begin end;
    typUntype: begin end;
    typInt: FWriter.Key('intkind').Value(IntTypeNames[TIntType(T).Kind]);
    typNumeric: FWriter.Key('numkind').Value(NumericTypeNames[TNumericType(T).Kind]);
    typBool: FWriter.Key('boolkind').Value(BoolTypeNames[TBoolType(T).Kind]);
    typChar: FWriter.Key('charkind').Value(CharTypeNames[TCharType(T).Kind]);
    typString: DumpStringType(TStringType(T));
    typVariant: FWriter.Key('isole').Value(TVariantType(T).IsOle);

    typPAnsiChar, typPWideChar: begin end;

    typPointer: DumpPointerType(TPointerType(T));

    typFile: DumpFileType(TFileType(T));
    typText: begin end;
    typProcedural: DumpProcType(TProceduralType(T));

    typRecord: DumpRecordType(TRecordType(T));
    typClass: DumpClassType(TClassType(T));
    typObject: DumpObjectType(TObjectType(T));
    typInterface: DumpInterfaceType(TInterfaceType(T));
    typClassRef: FWriter.Key('ref').Value(TClassRefType(T).RefType.FullName);

    typEnum: DumpEnumType(TEnumType(T));
    typSet: DumpSetType(TSetType(T));
    typSubrange: DumpSubrangeType(TSubrangeType(T));
    typArray: DumpArrayType(TArrayType(T));
    typDynamicArray: DumpDynamicArrayType(TDynamicArrayType(T));
    typOpenArray: DumpOpenArrayType(TOpenArrayType(T));
    typSymbol: FWriter.Key('ref').Value(TSymbolType(T).Reference.FullName);
    typAlias, typClonedType: DumpAliasType(TAliasType(T));
  end;
  FWriter.EndObject;
end;

procedure TDump.DumpVal(const V: TValueRec);
const
  StrOfVT: array[TValueType] of string = (
    'Empty', 'Int', 'Int64', 'Real', 'Curr', 'Set', 'Bool', 'Str',
    'WStr', 'AChr', 'WChr', 'Ptr', 'Array', 'Record', 'Symbol',
    'AddrOf', 'AddrOffset', 'IID', 'IIDStr'
  );
var
  I: Integer;
  Sym: TSymbol;
begin
  FWriter.BeginObject;
  FWriter.Key('vt').Value(StrOfVT[V.VT]);
  case V.VT of
    vtEmpty: begin end;
    vtInt: FWriter.Key('value').Value(V.VInt);
    vtInt64: FWriter.Key('value').Value(V.VInt64);
    vtReal: FWriter.Key('value').Value(V.VReal);
    vtCurr: FWriter.Key('value').Value(V.VCurr);
    vtBool: FWriter.Key('value').Value(V.VBool);
    vtStr, vtWStr, vtPtr:
      FWriter.Key('value').Value(ValToStr(V));
    vtAChr:
      if V.VAChr < #32 then
        FWriter.Key('value').Value('#' + IntToStr(Ord(V.VAChr)))
      else
        FWriter.Key('value').Value(ValToStr(V));
    vtWChr:
      if V.VWChr < 32 then
        FWriter.Key('value').Value('#' + IntToStr(V.VWChr))
      else
        FWriter.Key('value').Value(ValToStr(V));
    vtSet:
      if V.VSet = nil then
        FWriter.Key('value').Value('')
      else
        FWriter.Key('value').Value(TSetValue(V.VSet).AsString);
    vtArray, vtRecord: begin
      FWriter.Key('value').Value('array/record')
    end;

    vtSymbol: begin
      FWriter.Key('value');
      if V.VSymbol = nil then
        FWriter.NullValue
      else
        FWriter.Value(V.VSymbol.FullName);
    end;
    vtAddrOfSymbol: begin
      FWriter.Key('value');
      if V.VAddr = nil then
        FWriter.NullValue
      else
        FWriter.Value(V.VAddr.FullName);
    end;
    vtAddrOffset: begin
      FWriter.Key('value');
      I := ValGetAddrOffset(V, Sym);
      if Sym = nil then
        FWriter.NullValue
      else
        FWriter.Value(V.VSymbol.FullName);
      FWriter.Key('offset').Value(I);
    end;
  end;
  FWriter.EndObject;
end;

procedure TDump.DumpVar(V: TVariable);
begin
  FWriter.BeginObject;
  DumpSymBase(V);
  FWriter.Key('level').Value(V.Level);
  FWriter.Key('index').Value(V.Index);
  FWriter.Key('varattr').Value(VarAttrStr(V.VarAttr));
  FWriter.Key('state').Value(VarStateStr(V.States));
  FWriter.Key('typ');
  if V.VarType.Name = '' then
    DumpType(V.VarType)
  else
    FWriter.Value(V.VarType.FullName);
  FWriter.Key('absvar');
  if V.AbsVar <> nil then
    FWriter.Value(V.AbsVar.Name)
  else
    FWriter.NullValue;
  FWriter.Key('initval');
  DumpVal(V.Value);
  FWriter.EndObject;
end;

function TDump.ExprAttrStr(attr: TExprAttributes): string;
const
  AttrStr: array[TExprAttribute] of string = (
    'Verified', 'Invalid', 'Delayed', 'Call', 'ArgList',
    'OverloadRestrict', 'ArrayProp', 'Inherited', 'Const',
    'VarCast', 'StrOp', 'VarOp', 'SetOp', 'Res2', 'Res3', 'Res4'
  );
var
  i: TExprAttribute;
begin
  Result := '';
  for i := Low(TExprAttribute) to High(TExprAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.IntfPropAttrStr(attr: TIntfPropertyAttributes): string;
const
  AttrStr: array[TIntfPropertyAttribute] of string = (
    'NoUsed', 'DefaultProp', 'ReadOnly',
    'WriteOnly', 'HasDispID'
  );
var
  i: TIntfPropertyAttribute;
begin
  Result := '';
  for i := Low(TIntfPropertyAttribute) to High(TIntfPropertyAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.ObjectAttrStr(attr: TObjectAttributes): string;
const
  AttrStr: array[TObjectAttribute] of string = (
    'hasvirtual', 'hasvmt', 'beginvmt'
  );
var
  i: TObjectAttribute;
begin
  Result := '';
  for i := Low(TObjectAttribute) to High(TObjectAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.PropAttrStr(attr: TPropertyAttributes): string;
const
  AttrStr: array[TPropertyAttribute] of string = (
    'NoDefault', 'NoStored', 'DefaultProp'
  );
var
  i: TPropertyAttribute;
begin
  Result := '';
  for i := Low(TPropertyAttribute) to High(TPropertyAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);

end;

function TDump.StructAttrStr(attr: TStructAttributes): string;
const
  AttrStr: array[TStructAttribute] of string = (
    'NeedInit', 'NeedFree'
  );
var
  i: TStructAttribute;
begin
  Result := '';
  for i := Low(TStructAttribute) to High(TStructAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.SymbolAttrStr(attr: TSymbolAttributes): string;
const
  AttrStr: array[TSymbolAttribute] of string = (
    'Used', 'Reserved1', 'Internal', 'Forward', 'Static', 'Class', 'Primitive', 'Temp'
  );
var
  i: TSymbolAttribute;
begin
  Result := '';
  for i := Low(TSymbolAttribute) to High(TSymbolAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.VarAttrStr(attr: TVarAttributes): string;
const
  AttrStr: array[TVarAttribute] of string = (
    'Reserved1', 'Reserved2', 'ReadOnly', 'Local', 'Hidden',
    'Tls', 'Result', 'Self'
  );
var
  i: TVarAttribute;
begin
  Result := '';
  for i := Low(TVarAttribute) to High(TVarAttribute) do
    if i in attr then
      Result := Result + AttrStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

function TDump.VarStateStr(stat: TVarStates): string;
const
  StateStr: array[TVarState] of string = (
    'Init', 'NestRef', 'ResultAddr', 'NeedInit', 'NeedFree', 'Temp'
  );
var
  i: TVarState;
begin
  Result := '';
  for i := Low(TVarState) to High(TVarState) do
    if i in stat then
      Result := Result + StateStr[i] + ',';
  if Result <> '' then Delete(Result, Length(Result), 1);
end;

{ TJsonDump }

constructor TJsonDump.Create;
begin
  inherited;
  FWriter := TJsonWriter.Create;
end;

destructor TJsonDump.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

end.
