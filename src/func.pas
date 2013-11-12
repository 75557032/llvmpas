unit func;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses ast, parser;

procedure CheckFunction(Parser: TParser; Func: TFunction);

implementation

type
  TVisitorProc = procedure (Stmt: TStatement; Data: Pointer);

procedure ForEach(Stmt: TStatement; Proc: TVisitorProc; Data: Pointer);
var
  i: Integer;
begin
  if Stmt = nil then Exit;

  Proc(Stmt, Data);
  case Stmt.StmtKind of
    skCompoundStmt:
      for i := 0 to TCompoundStmt(Stmt).Statements.Count - 1 do
        ForEach(TStatement(TCompoundStmt(Stmt).Statements[i]), Proc, Data);

    skIfStmt: begin
      ForEach(TIfStmt(Stmt).TrueStmt, Proc, Data);
      ForEach(TIfStmt(Stmt).FalseStmt, Proc, Data);
    end;

    skCaseStmt:
      for i := 0 to TCaseStmt(Stmt).Count - 1 do
        ForEach(TCaseStmt(Stmt).Selectors[i].Stmt, Proc, Data);

    skForStmt:
      ForEach(TForStmt(Stmt).Stmt, Proc, Data);

    skWhileStmt:
      ForEach(TWhileStmt(Stmt).Stmt, Proc, Data);

    skRepeatStmt:
      ForEach(TRepeatStmt(Stmt).Stmt, Proc, Data);

    skTryStmt: begin
      ForEach(TTryStmt(Stmt).Stmt, Proc, Data);
      ForEach(TTryStmt(Stmt).FinallyStmt, Proc, Data);
      if TTryStmt(Stmt).ExceptBlock <> nil then
      begin
        for i := 0 to TTryStmt(Stmt).ExceptBlock.Count - 1 do
          ForEach(TTryStmt(Stmt).ExceptBlock.ExceptHandlers[i].Stmt, Proc, Data);
        ForEach(TTryStmt(Stmt).ExceptBlock.Default, Proc, Data);
      end;
    end;
  end;
end;

type
  TVarList = array of TSymbol;
  PCheckData = ^TCheckData;
  TCheckData = record
    VarList: TVarList;
    Result: TVariable;
    Parser: TParser;
  end;

// 检查表达式中的TVariable
procedure CheckVarRef(Expr: TExpr; Data: Pointer);
 (*
  procedure SetUsed(E: TSymbolExpr);
  var
    Ref: TSymbol;
  begin
    Ref := E.Reference;
    if (Ref <> nil) and (Ref.NodeKind = nkVariable) then
    begin
    // todo 1: 检查之前有没有初始化了
      if not (vaInit in TVariable(Ref).VarAttr) and
        (TVariable(Ref).VarType <> nil) and
        not (TVariable(Ref).VarType.TypeCode in AutoInitTypes) then
      begin
        Parser.DoWarning(E.Coord, 'Variable %s may not init', [Ref.Name]);
      end;

      Include(TVariable(Ref).VarAttr, vaUsed);
    end;
  end; *)
begin
  if Expr = nil then Exit;
  if Expr.OpCode = opSYMBOL then
  begin
    if TSymbolExpr(Expr).Reference <> nil then
      if TSymbolExpr(Expr).Reference.NodeKind in [nkVariable, nkArgument] then
        Include(TSymbolExpr(Expr).Reference.Attr, saUsed);
    Exit;
  end;

  if Expr.ClassType = TBinaryExpr then
  begin
    CheckVarRef(TBinaryExpr(Expr).Left, Data);
    CheckVarRef(TBinaryExpr(Expr).Right, Data);
  end
  else if Expr.ClassType = TUnaryExpr then
    CheckVarRef(TUnaryExpr(Expr).Operand, Data);
end;

procedure CheckStmtVarRef(Stmt: TStatement; Data: Pointer);
type
  TStateBuffer = array of Boolean;
var
  States, States2: TStateBuffer;

  procedure SaveStates(var States: TStateBuffer);
  var
    i: Integer;
    sym: TSymbol;
  begin
    SetLength(States, Length(PCheckData(Data)^.VarList));
    for i := 0 to Length(States) - 1 do
    begin
      sym := PCheckData(Data)^.VarList[i];
      case sym.NodeKind of
        nkVariable: States[i] := vsInit in TVariable(sym).States;
        nkArgument: States[i] := asInit in TArgument(sym).States;
      end;
    end;
  end;

  procedure UpdateStates(const s: TStateBuffer);
  var
    i: Integer;
    sym: TSymbol;
    varList: TVarList;
  begin
    varList := PCheckData(Data)^.VarList;
    for i := 0 to Length(s) - 1 do
    begin
      sym := varList[i];
      case sym.NodeKind of
        nkVariable:
          if s[i] then
            Include(TVariable(sym).States, vsInit)
          else
            Exclude(TVariable(sym).States, vsInit);
        nkArgument:
          if s[i] then
            Include(TArgument(sym).States, asInit)
          else
            Exclude(TArgument(sym).States, asInit);
      end;
    end;
  end;

  procedure OrStates(var s1: TStateBuffer); overload;
  var
    i: Integer;
    varList: TVarList;
    sym: TSymbol;
  begin
    varList := PCheckData(Data)^.VarList;
    for i := 0 to Length(s1) - 1 do
    begin
      sym := TVarList(Data)[i];
      case sym.NodeKind of
        nkVariable: s1[i] := s1[i] or (vsInit in TVariable(sym).States);
        nkArgument: s1[i] := s1[i] or (asInit in TArgument(sym).States);
      end;
    end;
  end;

  procedure OrStates(var s1, s2: TStateBuffer); overload;
  var
    i: Integer;
  begin
    for i := 0 to Length(s1) - 1 do
      s1[i] := s1[i] or s2[i];
  end;

  procedure ClearStates;
  var
    i: Integer;
    varList: TVarList;
  begin
    varList := PCheckData(Data)^.VarList;
    for i := 0 to Length(varList) - 1 do
      case VarList[i].NodeKind of
        nkVariable: Exclude(TVariable(varList[i]).States, vsInit);
        nkArgument: Exclude(TArgument(varList[i]).States, asInit);
      end;
  end;

  procedure AndStates(var s1, s2: TStateBuffer); overload;
  var
    i: Integer;
  begin
    for i := 0 to Length(s1) - 1 do
      s1[i] := s1[i] and s2[i];
  end;

  procedure AndStates(var s: TStateBuffer); overload;
  var
    i: Integer;
    varList: TVarList;
  begin
    varList := PCheckData(Data)^.VarList;
    for i := 0 to Length(s) - 1 do
      case varList[i].NodeKind of
        nkVariable: s[i] := s[i] and (vsInit in TVariable(varList[i]).States);
        nkArgument: s[i] := s[i] and (asInit in TArgument(varList[i]).States);
      end;
  end;

  function IsExitStmt(Stmt: TStatement): Boolean;
  var
    P: TSymbol;
  begin
    if Stmt.StmtKind = skCallStmt then
    begin
      P := TCallStmt(Stmt).CallExpr.GetReference;
      Result := (P <> nil) and (P.NodeKind = nkBuiltinFunc)
        and (TBuiltinFunction(P).Kind = bfExit);
    end
    else
      Result := False;
  end;

  procedure CheckCompound(Stmt: TCompoundStmt);
  var
    i: Integer;
  begin
    for i := 0 to Stmt.Statements.Count - 1 do
    begin
      if IsExitStmt(TStatement(Stmt.Statements[i])) then
      begin
        if PCheckData(Data)^.Result <> nil then
          if not (vsInit in PCheckData(Data)^.Result.States) then
            PCheckData(Data)^.Parser.DoWarning(Stmt.Coord, 'Result maybe undefined');
        Break;
      end;
      CheckStmtVarRef(TStatement(Stmt.Statements[i]), Data);
    end;
  end;

  procedure CheckAssign(Stmt: TAssignmentStmt);
  var
    Sym: TSymbol;
  begin
    Sym := Stmt.Left.GetReference;
    if Sym <> nil then
    begin
      case Sym.NodeKind of
        nkVariable: Include(TVariable(Sym).States, vsInit);
        nkArgument: Include(TArgument(Sym).States, asInit);
      end;
    end;
  end;

  procedure CheckWhile(Stmt: TWhileStmt);
  begin
    CheckVarRef(Stmt.Condition, Data);
  end;
begin
  case Stmt.StmtKind of
    skIfStmt: begin
      CheckVarRef(TIfStmt(Stmt).Value, Data);
      SaveStates(States);
      ClearStates;
      CheckStmtVarRef(TIfStmt(Stmt).TrueStmt, Data);
      if TIfStmt(Stmt).FalseStmt <> nil then
      begin
        SaveStates(States2);
        ClearStates;
        CheckStmtVarRef(TIfStmt(Stmt).FalseStmt, Data);
        AndStates(States2);
        OrStates(States, States2);
        UpdateStates(States);
      end;
    end;
    skCompoundStmt:
      CheckCompound(TCompoundStmt(Stmt));
    skAssignmentStmt:
      CheckAssign(TAssignmentStmt(Stmt));
    skWhileStmt:
      CheckWhile(TWhileStmt(Stmt));
  end;
end;

procedure CheckGoto(Stmt: TStatement; Data: Pointer);

  function GetOuterStmt(S: TStatement): TStatement;
  begin
    while S.Parent <> nil do
    begin
      if S.Parent.StmtKind = skTryStmt then
      begin
        Result := S;
        Exit;
      end;
      S := S.Parent;
    end;
    Result := nil;
  end;

begin

  if Stmt.StmtKind = skGotoStmt then
  begin
    if TGotoStmt(Stmt).StmtLabel <> nil then
    begin
      if TGotoStmt(Stmt).StmtLabel.Stmt <> nil then
      begin
        if GetOuterStmt(TGotoStmt(Stmt).StmtLabel.Stmt) <> GetOuterStmt(Stmt) then
          TParser(Data).ParseError(Stmt.Coord, 'GOTO leads into or out of TRY statement');
      end
      else
        TParser(Data).ParseError(Stmt.Coord, 'Label declared and referenced, but not set')
    end;
  end;
end;

procedure CheckFunction(Parser: TParser; Func: TFunction);

  function NeedCheckInit(T: TType): Boolean;
  begin
    Result := not (T.TypeCode in AutoInitTypes)
            and not (T.TypeCode in [typRecord, typObject, typArray]);
  end;

  function GetCheckVar(var List: TVarList; var ResultVar: TVariable): Integer;
  var
    i, c: Integer;
    Sym: TSymbol;

    procedure Add(Sym: TSymbol);
    begin
      if List <> nil then
      begin
        List[c] := Sym;
        Inc(c);
      end;
    end;
  begin
    c := 0;
    for i := 0 to Func.LocalSymbols.Count - 1 do
    begin
      Sym := Func.LocalSymbols[i];
      case Sym.NodeKind of
        nkArgument:
          if argOut = TArgument(Sym).Modifier then
            Add(Sym);
        nkVariable: begin
          if not NeedCheckInit(TVariable(Sym).VarType) then
          begin
            Add(Sym);
            if vaResult in TVariable(Sym).VarAttr then
              ResultVar := TVariable(Sym);
          end;
        end;
      end;
    end;
    Result := c;
  end;
(*var
  Data: TCheckData;
  Cnt: Integer;*)
begin
  // 检查goto语句
  ForEach(Func.StartStmt, CheckGoto, Parser);
(*  // 变量使用
  Data.Parser := Parser;
  Data.VarList := nil;
  Data.Result := nil;
  Cnt := GetCheckVar(Data.VarList, Data.Result);
  if Cnt > 0 then
  begin
    GetCheckVar(Data.VarList, Data.Result);
    ForEach(Func.StartStmt, CheckStmtVarRef, @Data);
  end;*)
end;

end.
