
procedure TParser.FindProper(E: TUnaryExpr; ProcType: TProceduralType);
  function IsProper(Func: TFunctionDecl): boolean;
  begin
    Result := ProcType.IsMethodPointer = ((Func.NodeKind = nkMethod) and not TMethod(Func).IsClassOrStatic);
    if Result then
      Result := ProcType.Equals(Func.ProceduralType);
  end;

  function CheckGroup(var Func: TFunctionDecl): Boolean;
  begin
    while Func <> nil do
    begin
      if IsProper(Func) then
      begin
        Result := True;
        Exit;
      end;
      Func := Func.NextOverload;
    end;
    Result := False;
  end;
var
  Func, Old: TFunctionDecl;
begin
  if E.OpCode <> opADDR then Exit;

  Func := E.Operand.GetFunctionSymbol;
  if (Func = nil) or not Func.IsOverload then Exit;

  if (fmOverload in Func.Modifiers) and
      not (eaOverloadRestrict in E.Operand.Attr)  then
  begin
    Old := Func;
    GetOverloadBegin(Func);

    while Func <> nil do
    begin
      if CheckGroup(Func) then
      begin
        E.Operand.SetReference(Func);
        E.Typ := Func.ProceduralType;
        E.Operand.Typ := E.Typ;
        Exit;
      end;
      Func := GetOverloadNext;
    end;
    GetOverloadEnd;

    ParseError(E.Coord, 'Can not choose a overloaded version for %s', [Old.Name]);
  end
  else if Func.NextOverload <> nil then
  begin
    Old := Func;
    if CheckGroup(Func) then
    begin
      E.Operand.SetReference(Func);
      E.Typ := Func.ProceduralType;
      E.Operand.Typ := E.Typ;
    end
    else
      ParseError(E.Coord, 'Can not choose a overloaded version for %s', [Old.Name]);
  end;
end;

function TParser.CheckExpr(var Expr: TExpr): Boolean;

const
  AddOpResult: array[typShortint..typCurrency, typShortint..typCurrency] of TTypeCode =
  (
              // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord,   typInt64, typUInt64, typComp, typReal48, typSingle, typDouble, typExtended, typCurrency
{typShortint} (typLongint, typLongint, typLongint, typLongint, typLongint, typInt64,    typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typByte    } (typLongint, typLongint, typLongint, typLongint, typLongint, typLongWord, typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typSmallint} (typLongint, typLongint, typLongint, typLongint, typLongint, typInt64,    typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typWord    } (typLongint, typLongint, typLongint, typLongint, typLongint, typLongWord, typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typLongint } (typLongint, typLongint, typLongint, typLongint, typLongint, typInt64,    typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typLongWord} (typInt64,   typLongWord, typInt64,  typLongWord, typInt64,  typLongWord, typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typInt64   } (typInt64,   typInt64,   typInt64,   typInt64,   typInt64,   typInt64,    typInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typUInt64  } (typUInt64,  typUInt64,  typUInt64,  typUInt64,  typUInt64,  typUInt64,  typUInt64, typUInt64, typDouble, typDouble, typDouble, typDouble, typDouble, typCurrency),
{typComp    } (typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble),
{typReal48  } (typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble),
{typSingle  } (typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble),
{typDouble  } (typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble),
{typExtended} (typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble,  typDouble),
{typCurrency} (typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency,typCurrency)
  );

  function CreateCastExpr(Typ: TType; Expr: TExpr): TExpr;
  var
    L: TSymbolExpr;
  begin
    L := CreateSymbolExpr(Typ.Name);
    L.Reference := Typ;
    Result := CreateBinaryExpr(opCAST, L, Expr);
  end;

  procedure IncompatibleErr(const Coord: TAstNodeCoord; t1, t2: TTypeCode);
  begin
    ParseError(Coord, 'Incompatible types: ''%s'' and ''%s''',
                [TypeNames[t1], TypeNames[t2]]);
  end;

  // 确保Expr不是指向Kind指定的Node
  // 返回true表示Expr不在Kinds指定的Node类型中
  function NotAllowNode(Kinds: TAstNodeKinds; Expr: TExpr): Boolean;
  var
    Invalid: Boolean;
  begin
    Invalid := False;
    case Expr.OpCode of
      opSYMBOL:
        if TSymbolExpr(Expr).Reference <> nil then
          Invalid := TSymbolExpr(Expr).Reference.NodeKind in Kinds;
      opMEMBER:
        begin
          Assert(TBinaryExpr(Expr).Right.OpCode = opSYMBOL, 'NotAllowNode');
          Expr := TBinaryExpr(Expr).Right;
          if TSymbolExpr(Expr).Reference <> nil then
            Invalid := TSymbolExpr(Expr).Reference.NodeKind in Kinds;
        end;
    end;

    if Invalid then
    begin
      if Expr.Typ = nil then Expr.Typ := FContext.FAnytype;
      ParseError(Expr.Coord, SErr_InvalidOperand);
    end;

    Result := not Invalid;
  end;

  function EnsureNode(Kind: TAstNodeKind; Expr: TExpr): Boolean;
  var
    Kinds: TAstNodeKinds;
  begin
    Kinds := [Low(TAstNodeKind)..High(TAstNodeKind)];
    Exclude(Kinds, Kind);
    Result := NotAllowNode(Kinds, Expr);
  end;

  procedure CheckOpenArrayConstructor(Expr: TUnaryExpr);
  var
    E: TExpr;
    Ref: TSymbol;
  begin
    E := Expr.Operand; // opLIST
    if E <> nil then
    begin
      E := TUnaryExpr(E).Operand; // first element of List expr
      while E <> nil do
      begin
        Ref := E.GetReference;
        if (Ref.NodeKind = nkType) and (TType(Ref).TypeCode <> typClass) then
          ParseError(E.Coord, SErr_InvalidOperand);
      //  NotAllowNode([nkType], E);
        if not (E.Typ.TypeCode in [typShortint..typOleVariant,
            typProcedural, typRecord, typClassRef, typSubrange,
            typArray, typDynamicArray]) then
        begin
          ParseError(E.Coord, SErr_InvalidOpenArrayEl);
        end;
        E := TExpr(E.Next);
      end;
    end;
    Expr.Typ := FContext.FVarOpenArrayType;
  end;

  function NeedSetType(E: TUnaryExpr): Boolean;
  const
    SetOps = [opADD, opSUB, opMUL, opLE, opGE, opEQ, opNE, opIN];
  begin
    if E.Parent = nil then
      Result := True
    else
      Result := E.Parent.OpCode in SetOps;
  end;

  // 检查opSET. 返回true表示可以视为Set
  function CheckSetType(Expr: TUnaryExpr): Boolean;
  var
    E: TExpr;
    T, T2: TType;
  begin
    E := Expr.Operand; // opLIST
    if E <> nil then
    begin
      E := TUnaryExpr(E).Operand; // first element of List expr
      if E <> nil then
      begin
        T := E.Typ.NormalType;

        case T.TypeCode of
          typBoolean: Expr.Typ := FContext.FBoolSetType;

          typAnsiChar, // 先假设WideChar是合法
          typWideChar: Expr.Typ := FContext.FCharSetType;

          typEnum:
            Expr.Typ := GetSetType(TEnumType(T));
        else
          if T.IsInteger then
            Expr.Typ := FContext.FByteSetType
          else begin
            Result := False;
            Exit;
          end;
        end;

        Expr.Operand.Typ := Expr.Typ;

        if T.TypeCode = typWideChar then
          T := FContext.FTypes[typAnsiChar];

        while E <> nil do
        begin
          T2 := E.Typ;
          if T2.TypeCode = typSubrange then T2 := TSubrangeType(T2).BaseType;
          if T <> T2 then
          begin
            Result := False;
            Exit;
          end;
          E := TExpr(E.Next);
        end;
      end;
    end;
    Result := True;
  end;

  procedure ResolveSetType(Expr: TUnaryExpr);
  begin
    NotAllowNode([nkType], Expr.Operand);
    if not CheckSetType(Expr) then
    begin
      if NeedSetType(Expr) then
      begin
        ParseError(Expr.Coord, SErr_InvalidOperand);
        Expr.Typ := FContext.FEmptySetType;
        if Expr.Operand <> nil then
          Expr.Operand.Typ := Expr.Typ;
      end
      else // 当成开放数组来检查
        CheckOpenArrayConstructor(Expr);
    end;
  end;

  function NeedExpand(T1, T2: TType): Boolean; forward;

  procedure CheckRelOp(bin: TBinaryExpr);
  var
    L, R: TType;
    Result: Boolean;
  begin
{
  integer, real, string, packed string, sets, typed pointer, variant, olevariant
}
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    bin.Typ := FContext.FTypes[typBoolean];
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then
      L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then
      R := TSubrangeType(L).BaseType;

    case L.TypeCode of
      typShortint..typUInt64:
        case R.TypeCode of
          typShortint..typUInt64:
            if NeedExpand(L, R) then
              DoWarning(bin.Coord, SWarn_CombiningSignedUnsigned);
        end;
    end;

    case L.TypeCode of
      typShortint..typCurrency:
        if R.TypeCode in [typShortint..typCurrency, typVariant, typOleVariant] then
          Result := True
        else
          Result := False;

      typAnsiString..typShortString:
        Result := R.TypeCode in [typAnsiString..typShortString,
            typAnsiChar, typWideChar, typPAnsiChar, typPWideChar,
            typVariant, typOleVariant];

      typBoolean..typLongBool:
        if R.TypeCode in [typBoolean..typLongBool, typVariant, typOleVariant] then
          Result := True
        else
          Result := False;

      typVariant, typOleVariant:
        Result := R.TypeCode in [typShortint..typUInt64,
                    typComp..typCurrency,
                    typAnsiChar, typWideChar,
                    typPAnsiChar, typPWideChar,
                    typBoolean..typLongBool, typEnum,
                    typAnsiString..typShortString, typVariant..typOleVariant];

      typPAnsiChar, typPWideChar:
        Result := R.IsStringCompatible or (R.TypeCode in [typPointer,
            typAnsiChar, typWideChar,
            typPAnsiChar, typPWideChar,
            typVariant, typOleVariant]);

      typAnsiChar, typWideChar:
        Result := (L.TypeCode = R.TypeCode) or
            (R.TypeCode in [typAnsiString..typShortString, typAnsiChar, typWideChar, typVariant, typOleVariant]);

      typPointer:
        case R.TypeCode of
          typInterface, typDispInterface:
            Result := bin.Left.OpCode = opNIL;
        else
          // todo 1: 如果两个指针分别指向不同的object或record,则不应该通过
          Result := R.TypeCode in [typPointer, typPAnsiChar, typPWideChar, typClass, typClassRef];
        end;

      typClass:
        if bin.OpCode in [opNE, opEQ] then
        begin
          case R.TypeCode of
            typPointer: Result := True;
            typClass:
              Result := TClassType(R).IsInheritedFrom(TClassType(L)) or
                TClassType(L).IsInheritedFrom(TClassType(R));
          else
            Result := False;
          end;
        end
        else
          Result := False;

      typClassRef:
        if bin.OpCode in [opNE, opEQ] then
        begin
          case R.TypeCode of
            typPointer: Result := True;
            typClassRef:
              Result := TClassRefType(R).IsInheritedFrom(TClassRefType(L)) or
                TClassRefType(L).IsInheritedFrom(TClassRefType(R));
          else
            Result := False;
          end;
        end
        else
          Result := False;

      typInterface:
        if bin.OpCode in [opNE, opEQ] then
        begin
          case R.TypeCode of
            typPointer: Result := bin.Right.OpCode = opNIL;
            typInterface:
              Result := TInterfaceType(R).IsInheritedFrom(TInterfaceType(L)) or
                TInterfaceType(L).IsInheritedFrom(TInterfaceType(R));
            typDispInterface:
              with FContext do
                if FIDispatchType <> nil then
                  Result := TInterfaceType(L).IsInheritedFrom(FIDispatchType)
                else
                  Result := True;
          else
            Result := False;
          end;
        end
        else
          Result := False;

      // DONE 1: 需要检查继承关系
      // dispinterface与nil,有继承关系的dispinterface,以及idispatch相容
      typDispInterface:
        if bin.OpCode in [opNE, opEQ] then
        begin
          case R.TypeCode of
            typPointer: Result := bin.Right.OpCode = opNIL;
            typInterface:
              with FContext do
                if FIDispatchType <> nil then
                  Result := TInterfaceType(R).IsInheritedFrom(FIDispatchType)
                else
                  Result := True;
            typDispInterface:
              Result := TInterfaceType(R).IsInheritedFrom(TInterfaceType(L)) or
                TInterfaceType(L).IsInheritedFrom(TInterfaceType(R));
          else
            Result := False;
          end;
        end
        else
          Result := False;

      typEnum:
        Result := (L = R) or (R.TypeCode in [typVariant, typOleVariant]);

      typSet: Result := (R.TypeCode = typSet) and
                (bin.OpCode in [opNE, opEQ, opGE, opLE]) and
                (TSetType(L).RangeType = TSetType(R).RangeType);

    else
      Result := False;
    end;

    if not Result then
      IncompatibleErr(bin.Coord, L.TypeCode, R.TypeCode);

    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  procedure CheckIsOp(bin: TBinaryExpr);
  begin
    bin.Typ := FContext.FTypes[typBoolean];
    if bin.Left.Typ.TypeCode = typClass then
    begin
      if bin.Right.Typ.TypeCode <> typClassRef then
        ParseError(bin.Coord, SErr_InvalidOperand);
    end
    else
      ParseError(bin.Coord, SErr_InvalidOperand);
  end;

  procedure CheckInOp(bin: TBinaryExpr);
  var
    hasErr, notEmpty: Boolean;
  begin
    // 左: integer, char, enum
    // 右: set
    bin.Typ := FContext.FTypes[typBoolean];
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);

    if bin.Right.Typ.TypeCode <> typSet then
      hasErr := True
    else begin
      notEmpty := TSetType(bin.Right.Typ).RangeType <> nil;
      case bin.Left.Typ.TypeCode of
        typShortint..typUInt64:
          hasErr := notEmpty and (TSetType(bin.Right.Typ).RangeType <> FContext.FByteRangeType);

        typBoolean..typLongBool:
          hasErr := notEmpty and (TSetType(bin.Right.Typ).RangeType <> FContext.FBoolRangeType);

        typAnsiChar..typWideChar:
          hasErr := notEmpty and (TSetType(bin.Right.Typ).RangeType <> FContext.FCharRangeType);

        typEnum:
          hasErr := notEmpty and (TSetType(bin.Right.Typ).RangeType.BaseType <> TEnumType(bin.Left.Typ));

        else
          hasErr := True;
      end;
    end;

    if hasErr then
      ParseError(bin.Coord, SErr_InvalidOperand);

    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  procedure CheckAsOp(bin: TBinaryExpr);
  begin
    NotAllowNode([nkType], bin.Left);
    // 左边是class, interface型的表达式
    // 右边是类引用表达式, interface型的符号
    if bin.Left.Typ.TypeCode in [typClass, typInterface, typDispInterface] then
    begin
      case bin.Right.Typ.TypeCode of
        typClassRef: bin.Typ := TClassRefType(bin.Right.Typ).RefType;
        typInterface, typDispInterface: begin
            bin.Typ := bin.Right.Typ;
            if bin.Right.OpCode <> opSYMBOL then
              ParseError(bin.Coord, SErr_InvalidOperand);
          end;

      else
        bin.Typ := bin.Left.Typ;
        ParseError(bin.Coord, SErr_InvalidOperand);
      end;
    end
    else begin
      bin.Left.Typ := FContext.FTObjectType;
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;
  end;

const
  SignOf: array[typShortint..typUInt64] of Boolean = (
      True, False, True, False, True, False, True, False
    );

  function NeedExpand(t1, t2: TType): Boolean;
  begin
    // LongWord和其它有符号数的运算, 需要扩展为Int64, 在性能有所损失
    Result := ((t1.TypeCode = typLongWord) and (t2.TypeCode in [typShortint, typSmallint, typLongint]))
            or
              ((t2.TypeCode = typLongWord) and (t1.TypeCode in [typShortint, typSmallint, typLongint]));
  end;

  procedure CheckDivModOp(bin: TBinaryExpr);
  var
    L, R: TType;
  begin
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    if (L.TypeCode in [typShortint..typUInt64]) and
       (R.TypeCode in [typShortint..typUInt64]) then
    begin
      if (L.TypeCode in [typLongWord..typUInt64]) or
         (R.TypeCode in [typLongWord..typUInt64]) then
      begin
        if NeedExpand(L, R) then
          DoWarning(bin.Coord, SWarn_CombiningSignedUnsigned);

        if SignOf[L.TypeCode] or SignOf[R.TypeCode] then
          bin.Typ := FContext.FTypes[typInt64]
        else
          bin.Typ := FContext.FTypes[typUInt64];
      end
      else
        bin.Typ := FContext.FTypes[typLongint];
    end
    else if (L.TypeCode in [typVariant, typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(R) then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := FContext.FTypes[typVariant];
    end
    else if (R.TypeCode in [typVariant, typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(L) then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := FContext.FTypes[typVariant];
    end
    else begin
      bin.Typ := FContext.FTypes[typLongint];
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;

    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  procedure CheckDivOp(bin: TBinaryExpr);
  var
    L, R: TType;
  begin
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    if (L.TypeCode in [typShortint..typCurrency]) and
      (R.TypeCode in [typShortint..typCurrency]) then
    begin
      bin.Typ := FContext.FTypes[typDouble];
    end
    else if (L.TypeCode in [typVariant, typOleVariant]) or
      (R.TypeCode in [typVariant, typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(L) or
          not TypIsVariantArithCompatible(R) then
        ParseError(bin.Coord, SErr_InvalidOperand);

      bin.Typ := FContext.FTypes[typVariant];
    end
    else begin
      bin.Typ := FContext.FTypes[typLongint];
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;
    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  procedure CheckNegOp(un: TUnaryExpr);
  var
    T: TType;
  begin
    NotAllowNode([nkType], un.Operand);

    T := un.Operand.Typ;
    if T.TypeCode = typSubrange then T := TSubrangeType(T).BaseType;

    case T.TypeCode of
      typShortint..typUInt64:
        un.Typ := FContext.FTypes[T.TypeCode];

      typComp..typCurrency:
        un.Typ := FContext.FTypes[un.Operand.Typ.TypeCode];

      typVariant..typOleVariant:
        un.Typ := FContext.FTypes[un.Operand.Typ.TypeCode];
    else
      un.Typ := FContext.FTypes[typLongint];
      ParseError(SErr_InvalidOperand);
    end;

    if eaConst in un.Operand.Attr then Include(un.Attr, eaConst);
  end;

  // opADD opSUB opMUL
  procedure CheckAddSubMulOp(bin: TBinaryExpr);
  var
    t1, t2, t: TTypeCode;
    L, R: TType;
  begin
{
  integer, real, string, packed string, sets, typed pointer, variant, olevariant
}
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    // todo 1 : 常量必须要更改类型，以避免扩展到64位运算

    t1 := L.TypeCode;
    t2 := R.TypeCode;
    if (t1 in [typShortint..typCurrency]) and
        (t2 in [typShortint..typCurrency]) then
    begin
      t := AddOpResult[L.TypeCode, R.TypeCode];
      bin.Typ := FContext.FTypes[t];
      if NeedExpand(L, R) then
        DoWarning(bin.Coord, SWarn_CombiningSignedUnsigned);
    end
    else if (t1 in [typVariant..typOleVariant]) or
        (t2 in [typVariant..typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(L) or
          not TypIsVariantArithCompatible(R) then
        ParseError(bin.Coord, SErr_InvalidOperand);

      bin.Typ := FContext.FTypes[typVariant];
    end
    // 字符串
    else if (bin.OpCode = opADD) and TypIsStringArithCompatible(L)
        and TypIsStringArithCompatible(R) then
    begin
      if (t1 = typUnicodeString) or (t2 = typUnicodeString) then
        bin.Typ := FContext.FTypes[typUnicodeString]
      else if (t1 = typWideString) or (t2 = typWideString) then
        bin.Typ := FContext.FTypes[typWideString]
      else
        bin.Typ := FContext.FStringType;
    end
    else if (bin.OpCode = opSUB)
      and (t1 in [typPointer, typPAnsiChar, typPWideChar])
      and (t2 in [typPointer, typPAnsiChar, typPWideChar]) then
    begin
      bin.Typ := FContext.FNativeIntType;
    end
    // 指针运算
    // p := 1 + p : ok
    // p := 1 - p : error
    else if (bin.OpCode in [opADD, opSUB])
      and (t1 in [typPointer, typPAnsiChar, typPWideChar])
      and (t2 in [typShortint..typInt64]) then
    begin
      if (t1 = typPointer) and TPointerType(L).IsUntype then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := L;
    end
    else if (bin.OpCode in [opADD])
      and (t2 in [typPointer, typPAnsiChar, typPWideChar])
      and (t1 in [typShortint..typInt64]) then
    begin
      if (t2 = typPointer) and TPointerType(R).IsUntype then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := R;
    end
    // 集合运算
    else if (t1 = typSet) and (t2 = typSet) then
    begin
      if TSetType(L).RangeType <> TSetType(R).RangeType then
//        IncompatibleErr(bin.Coord, t1, t2);
        ParseError(bin.Coord, 'Incompatible types');
      bin.Typ := L;
    end
    else
    begin
      bin.Typ := FContext.FTypes[typLongint];
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;

    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  // shl, shr
  procedure CheckShiftOp(bin: TBinaryExpr);
  var
    L, R: TType;
  begin
    {
      shl: 如果左操作数小于Integer, 则结果是Integer类型. 否则结果是左操作数的类型
      shr 结果类型推测为左操作数的类型
    }
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    if (L.TypeCode in [typShortint..typUInt64]) and
        (R.TypeCode in [typShortint..typUInt64]) then
    begin
      if (L.TypeCode < typLongint) and (bin.OpCode = opSHL) then
        bin.Typ := FContext.FIntegerType
      else
        bin.Typ := L;
    end
    else if (L.TypeCode in [typVariant, typOleVariant]) or
        (R.TypeCode in [typVariant, typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(L) or
          not TypIsVariantArithCompatible(R) then
        ParseError(bin.Coord, SErr_InvalidOperand);

      bin.Typ := FContext.FTypes[typVariant];
    end
    else
    begin
      bin.Typ := FContext.FTypes[typLongint];
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;
    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  // and xor or
  procedure CheckBitwiseOp(bin: TBinaryExpr);
  var
    t1, t2: TTypeCode;
    L, R: TType;
  begin
{
  variant, olevariant, integer, boolean
}
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);
    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    {
      and or xor 结果的符号推测:
        unsigned op unsigned: unsigned
        unsigned op signed: -|
        signed op unsigned: -| unsigned
        signed op signed: signed

        0..$7fffffff的常量可以更改类型。除此之外，不能。
    }
    t1 := L.TypeCode;
    t2 := R.TypeCode;
    if (t1 in [typShortint..typUInt64]) and
        (t2 in [typShortint..typUInt64]) then
    begin
      if L.Size = R.Size then
      begin
        if R.IsSignedInt then
          bin.Typ := L
        else
          bin.Typ := R;
      end
      else if L.Size < R.Size then
        bin.Typ := R
      else
        bin.Typ := L;
{
      if (t1 in [typInt64..typUInt64]) or (t2 in [typInt64..typUInt64]) then
        t := typUInt64
      else if (t1 in [typShortint, typSmallint, typLongint]) and
              (t2 in [typShortint, typSmallint, typLongint]) then
        t := typLongint
      else
        t := typLongWord;

      bin.Typ := Parser.FContext.FTypes[t]; }
    end
    else if (t1 in [typVariant..typOleVariant]) or
        (t2 in [typVariant..typOleVariant]) then
    begin
      if not TypIsVariantArithCompatible(L) or
          not TypIsVariantArithCompatible(R) then
        ParseError(bin.Coord, SErr_InvalidOperand);

      bin.Typ := FContext.FTypes[typVariant];
    end
    else if (bin.OpCode in [opAND, opOR, opXOR]) and
        (t1 in [typBoolean..typLongBool]) and
        (t2 in [typBoolean..typLongBool]) then
    begin
      bin.Typ := FContext.FTypes[typBoolean];
    end
    else
    begin
      bin.Typ := FContext.FTypes[typLongint];
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;

    if (eaConst in bin.Left.Attr) and (eaConst in bin.Right.Attr) then
      Include(bin.Attr, eaConst);
  end;

  procedure CheckNotOp(un: TUnaryExpr);
  var
    T: TType;
  begin
    NotAllowNode([nkType], un.Operand);
    T := un.Operand.Typ;
    if T.TypeCode = typSubrange then T := TSubrangeType(T).BaseType;

    case T.TypeCode of
      typShortint..typUInt64:
        un.Typ := FContext.FTypes[T.TypeCode];

      typBoolean..typLongBool:
      //  un.Typ := FContext.FTypes[typBoolean];
        un.Typ := FContext.FTypes[T.TypeCode];

      typVariant..typOleVariant:
        un.Typ := FContext.FTypes[un.Operand.Typ.TypeCode];
    else
      un.Typ := FContext.FTypes[typBoolean];
      ParseError(SErr_InvalidOperand);
    end;
    if eaConst in un.Operand.Attr then Include(un.Attr, eaConst);
  end;

  function CreateCallExpr(Sym: TExpr): TBinaryExpr;
  var
    P: TExpr;
    Left: TExpr;
  begin
    Left := Sym.Parent;
    //if (Left = nil) or (Left.OpCode = opLIST) then Left := Sym;
    if (Left = nil) or (Left.OpCode <> opMEMBER) then Left := Sym;

    P := Left.Parent;
    Result := CreateBinaryExpr(opCALL, Left, nil);
    Result.Attr := Left.Attr;
    Result.Coord := Left.Coord;
    Result.Switches := Left.Switches;
    Result.Next := Left.Next;
    Left.Next := nil;

    if P <> nil then
    begin
      if P.ClassType = TBinaryExpr then
      begin
        if TBinaryExpr(P).Left = Sym then
          TBinaryExpr(P).Left := Result
        else
          TBinaryExpr(P).Right := Result;
      end
      else if P.ClassType = TUnaryExpr then
        if P.OpCode = opLIST then
          Result.Parent := P
        else
          TUnaryExpr(P).Operand := Result;
    end;
  end;

  function CountOfArgs(Args: TUnaryExpr): Integer;
  var
    E: TExpr;
  begin
    Result := 0;
    if Args = nil then Exit;

    Assert(Args.OpCode = opLIST);
    E := Args.Operand;
    while E <> nil do
    begin
      E := TExpr(E.Next);
      Inc(Result);
    end;
  end;

  function GetProcType(Sym: TSymbol): TProceduralType;
    function GetProc(T: TType): TProceduralType;
    begin
      if T.TypeCode = typProcedural then
        Result := TProceduralType(T) else
        Result := nil;
    end;
  begin
    case Sym.NodeKind of
      nkFunc, nkMethod, nkExternalFunc:
        Result := TFunctionDecl(Sym).ProceduralType;
      nkVariable: Result := GetProc(TVariable(Sym).VarType);
      nkField: Result := GetProc(TField(Sym).FieldType);
      nkProperty: Result := GetProc(TProperty(Sym).PropType);
      nkIntfProperty: Result := GetProc(TIntfProperty(Sym).PropType);
      nkArgument: Result := GetProc(TArgument(Sym).ArgType);
    else
      Result := nil;
    end;
  end;

  function GetReturnType(Sym: TSymbol): TType;
    function GetRet(T: TType): TType;
    begin
      if T.TypeCode = typProcedural then
        Result := TProceduralType(T).ReturnType
      else
        Result := nil;
    end;
  begin
    case Sym.NodeKind of
      nkFunc, nkMethod, nkExternalFunc:
        Result := TFunctionDecl(Sym).ReturnType;
      nkVariable: Result := GetRet(TVariable(Sym).VarType);
      nkField: Result := GetRet(TField(Sym).FieldType);
      nkProperty: Result := GetRet(TProperty(Sym).PropType);
      nkIntfProperty: Result := GetRet(TIntfProperty(Sym).PropType);
      nkArgument: Result := GetRet(TArgument(Sym).ArgType);
    else
      Result := nil;
    end;
  end;

  function MinOfProcArgs(Sym: TSymbol): Integer;
  begin
    case Sym.NodeKind of
      nkFunc, nkMethod, nkExternalFunc:
        Result := TFunctionDecl(Sym).MinOfArgs
    else
      Sym := GetProcType(Sym);
      Result := TProceduralType(Sym).MinOfArgs;
    end;
  end;
type
  TCheckArgsResult = (caMismatched, caOk, caCompatible);
  TCheckOverloadResult = (coNonMatched, coMatched, coMultiMatched,
                          coCompatible, coMultiCompatible);

  // 只有在IsOverload=True之时，才有可能返回caOk
  function CheckArgs(FormalArgs: TList; CallExpr: TBinaryExpr;
                     ErrReport: Boolean = True;
                     IsOverload: Boolean = False; // 位于Overload 中
                     DoChange: Boolean = True     // 如果实参是Overload 的符号,可以调整它指向的TFunctionDecl
                    ): TCheckArgsResult; overload;

    procedure ShowError(const Coord: TAstNodeCoord; const Msg: string);
    begin
      if ErrReport then
        ParseError(Coord, Msg);
    end;

    procedure ChangeType(ActualArg: TExpr; ArgTyp: TType);
    begin
        // 更改类型
        if DoChange and (ActualArg.Typ.TypeCode = typSet) then
        begin
          ActualArg.Typ := ArgTyp;
          if TUnaryExpr(ActualArg).Operand <> nil then
            TUnaryExpr(ActualArg).Operand.Typ := ActualArg.Typ;
        end;
    end;

    // 检查typ是否是开放数组兼容
    function IsOpenArrayCompatibility(typ, el: TType): Boolean;
    begin
      case typ.TypeCode of
        typOpenArray: Result := (el = FContext.FVarRecType);
        typDynamicArray:
          Result := TDynamicArrayType(typ).ElementType.Equals(el);
        typArray:
          Result := TArrayType(typ).ElementType.Equals(el);
        typSet:
          Result := True;
      else
        Result := False;
      end;
    end;

    // check variant open array
    procedure CheckArrayOfConst(ActualArg: TExpr);
    begin
      if not IsOpenArrayCompatibility(ActualArg.Typ, FContext.FVarRecType) then
        ShowError(ActualArg.Coord, SErr_VarArgMustIdentical);
      ChangeType(ActualArg, FContext.FVarOpenArrayType);
    end;

    procedure CheckArrayOfType(ActualArg: TExpr; ArgDef: TArgument);
    var
      E: TExpr;
    begin
      if ActualArg.OpCode = opSET then
      begin
        E := TUnaryExpr(ActualArg).Operand;
        if E <> nil then
          E := TUnaryExpr(E).Operand;
        while E <> nil do
        begin
          if not CheckAssignmentCompatibility(E.Typ, TOpenArrayType(ArgDef.ArgType).ElementType) then
            ShowError(E.Coord, SErr_IncompatibleTypes);
          E := TExpr(E.Next);
        end;
        // 更改类型
        ChangeType(ActualArg, ArgDef.ArgType);
      end
      else
      begin
        if not IsOpenArrayCompatibility(ActualArg.Typ, ArgDef.ArgType) then
          ShowError(ActualArg.Coord, SErr_IncompatibleTypes);
        ChangeType(ActualArg, ArgDef.ArgType);
      end;
    end;

    function MinCountOfArgs(Args: TList): Integer;
    var
      i: Integer;
    begin
      Result := 0;
      for i := 0 to Args.Count - 1 do
        if TArgument(Args[i]).DefaultValue.VT = vtEmpty then
          Inc(Result);
    end;

    function CheckTypeCompatibility(T1, T2: TType): Boolean;
    begin
      if (T1 <> nil) and (T2 <> nil) then
        Result := CheckAssignmentCompatibility(T1, T2)
      else
        Result := False;
    end;

    function CheckProcCompatibility(T1, T2: TType): Boolean;
    begin
      if (T1 <> nil) and (T2 <> nil) then
      begin
      // 这里使函数指针可以传给无类型指针
        if ((T2.TypeCode = typProcedural) and not TProceduralType(T2).IsMethodPointer)
            and T1.IsUntypePointer then
          Result := True
        else
          Result := CheckAssignmentCompatibility(T1, T2)
      end
      else
        Result := False;
    end;

    // 检查形参与实参是否兼容
    function CheckExprCompatibility(Arg: TArgument; E: TExpr): TCheckArgsResult;
    var
      Typ: TProceduralType;
      AlterTyp: TType;
      Ok: Boolean;
    begin
      if E.Typ.TypeCode = typProcedural then
        Typ := TProceduralType(E.Typ)
      else
        Typ := nil;
      if Typ <> nil then
      begin
        AlterTyp := Typ;
        Ok := CheckTypeCompatibility(Arg.ArgType, AlterTyp);

        if not Ok and (Typ.MinOfArgs = 0) then
        begin
          AlterTyp := Typ.ReturnType;
          if AlterTyp <> nil then
          begin
            Ok := CheckTypeCompatibility(Arg.ArgType, AlterTyp);
            if Ok and DoChange then
              Include(E.Attr, eaCall);
          end;
        end;

        if not Ok then
          Result := caMismatched
        else if IsOverload and Arg.ArgType.Equals(AlterTyp) then
          Result := caOk
        else
          Result := caCompatible;
      end
      else
        Result := caMismatched;
    end;

    // 检查形参与实参的类型是否兼容。Ref是E指向的符号
    function CheckSingleCompatibility(Arg: TArgument; E: TExpr; Ref: TSymbol): TCheckArgsResult;
    var
      AlterTyp: TType;
      Ok: Boolean;
    begin
      if E.OpCode = opADDR then
      begin
        AlterTyp := GetProcType(Ref);
        if AlterTyp <> nil then
          Ok := CheckProcCompatibility(Arg.ArgType, AlterTyp)
        else
          Ok := False;
        if Ok and DoChange then
          TUnaryExpr(E).Operand.SetReference(Ref);
      end
      else
      begin
        //AlterTyp := E.Typ;
        AlterTyp := GetProcType(Ref);
        Ok := CheckTypeCompatibility(Arg.ArgType, AlterTyp);

        if not Ok and (MinOfProcArgs(Ref) = 0) then
        begin
          //AlterTyp := Ref.ReturnType;
          AlterTyp := GetReturnType(Ref);
          if AlterTyp <> nil then
          begin
            Ok := CheckTypeCompatibility(Arg.ArgType, AlterTyp);
            if Ok and DoChange then
            begin
              Include(E.Attr, eaCall);
              E.SetReference(Ref);
            end;
          end;
        end;
      end;

      if not Ok then
        Result := caMismatched
      else if IsOverload and Arg.ArgType.Equals(AlterTyp) then
        Result := caOk
      else
        Result := caCompatible;
    end;

    function CheckGroupCompatibility(Arg: TArgument; E: TExpr; Func: TFunctionDecl): TCheckOverloadResult;
    var
      ChkResult: TCheckArgsResult;
      F: TFunctionDecl;
    begin
      Result := coNonMatched;
      F := Func;
      while F <> nil do
      begin
        ChkResult := CheckSingleCompatibility(Arg, E, F);

        if ChkResult = caOk then
        begin
          Result := coMatched;
          Break;
        end
        else if ChkResult = caCompatible then
        begin
          Result := coCompatible;
          Break;
        end;
        F := F.NextOverload;
      end;

    end;

    function CheckMultiCompatibility(Arg: TArgument; E: TExpr; Func: TFunctionDecl): TCheckArgsResult;
    var
      ChkResult: TCheckOverloadResult;
    begin
      Result := caMismatched;
      if not (eaOverloadRestrict in E.Attr) and (fmOverload in Func.Modifiers) then
      begin
        GetOverloadBegin(Func);

        while Func <> nil do
        begin
          ChkResult := CheckGroupCompatibility(Arg, E, Func);
          case ChkResult of
            coCompatible: Result := caCompatible;
            coMatched: begin
              Result := caOk;
              Exit;
              end;
          end;
          Func := GetOverloadNext;
        end;
        GetOverloadEnd;
      end
      else if Func.NextOverload <> nil then
      begin
        ChkResult := CheckGroupCompatibility(Arg, E, Func);
        case ChkResult of
          coCompatible: Result := caCompatible;
          coMatched: begin
            Result := caOk;
            Exit;
            end;
        end;
      end
    end;

    function CheckCompatibility(Arg: TArgument; E: TExpr): TCheckArgsResult;
    var
      Ref: TSymbol;
    begin
      if eaDelayed in E.Attr then
      begin
        Ref := nil;
        case E.OpCode of
          opSYMBOL, opMEMBER: Ref := E.GetReference;
          opADDR:
            if TUnaryExpr(E).Operand <> nil then
              Ref := TUnaryExpr(E).Operand.GetReference;
        else
        // 其它返回函数指针的表达式（在CheckAdjustCall中添加）比如 aaa(TMyFunc(p));
          //Result := caMismatched;
          Result := CheckExprCompatibility(Arg, E);
          Exit;
        end;

        // Ref不一定是函数，可能是函数指针的变量
        if Ref <> nil then
        begin
          if (Ref.NodeKind in [nkFunc, nkMethod, nkExternalFunc])
            and TFunctionDecl(Ref).IsOverload then
          begin
            Result := CheckMultiCompatibility(Arg, E, TFunctionDecl(Ref))
          end
          else
            Result := CheckSingleCompatibility(Arg, E, Ref);
        end
        else
          Result := caMismatched;
      end
      else begin
        if not CheckTypeCompatibility(Arg.ArgType, E.Typ) then
          Result := caMismatched
        else if IsOverload and Arg.ArgType.Equals(E.Typ) then
          Result := caOk
        else
          Result := caCompatible;
      end;
    end;

  var
    ArgCount, FormalArgCount, MinArgCount, I: Integer;
    E: TExpr;
    Arg: TArgument;
    ActualArgs: TUnaryExpr;
    AllOk: Boolean;
  begin
    FormalArgCount := 0;
    MinArgCount := 0;
    if FormalArgs <> nil then
    begin
      FormalArgCount := FormalArgs.Count;
      MinArgCount := MinCountOfArgs(FormalArgs);
    end;

    ActualArgs := TUnaryExpr(CallExpr.Right);
    ArgCount := CountOfArgs(ActualArgs);
    Result := caMismatched;
    AllOk := True;

    if (ArgCount <= FormalArgCount) and (ArgCount >= MinArgCount) then
    begin
      if ArgCount > 0 then
      begin
        I := 0;
        E := ActualArgs.Operand;
        while E <> nil do
        begin
          Arg := TArgument(FormalArgs[I]);

          if Arg.ArgKind in [akArrayOfType, akArrayOfConst] then
          begin
          // 检查开放数组参数
            if TOpenArrayType(Arg.ArgType).ElementType.TypeCode = typUntype then
              CheckArrayOfConst(E)
            else
              CheckArrayOfType(E, Arg);
            if Arg.Modifier in [argVar, argOut] then
              if not E.HasMemory then
                ShowError(E.Coord, SErr_VarArgMustIdentical);
          end
          else if Arg.ArgKind = akUntype then
          begin
            if not E.HasMemory then
              ShowError(E.Coord, SErr_IncompatibleTypes);
          end
          else if Arg.Modifier in [argVar, argOut] then
          begin
            if not E.HasMemory then
              ShowError(E.Coord, SErr_IncompatibleTypes)
            else if (Arg.ArgType.TypeCode <> typUntype) and (E.Typ <> Arg.ArgType) then
            // var/out参数必须严格相等
            begin
              ShowError(E.Coord, SErr_VarArgMustIdentical);
              Exit;
            end;
          end
          else begin
            case CheckCompatibility(Arg, E) of
              caMismatched: begin
                ShowError(E.Coord, SErr_IncompatibleTypes);
                Exit;
                end;
              caCompatible:
                AllOk := False;
            end;
          end;

          E := TExpr(E.Next);
          Inc(I);
        end;
      end;
    end
    else
    begin
      ShowError(CallExpr.Coord, SErr_ArgNotMatched);
      Exit;
    end;

    if AllOk and IsOverload then
      Result := caOk
    else
      Result := caCompatible;
  end;

  // 检查一组重载函数
  function CheckFuncs(var Func: TFunctionDecl;
                      CallExpr: TBinaryExpr): TCheckOverloadResult;
  var
    ChkResult: TCheckArgsResult;
    MatchedCount, CompatibleCount: Integer;
    F: TFunctionDecl;
  begin
    MatchedCount := 0;
    CompatibleCount := 0;
    Result := coNonMatched;
    F := Func;
    while F <> nil do
    begin
      ChkResult := CheckArgs(F.Args, CallExpr, False, True, False);
      if ChkResult = caOk then
      begin
        Inc(MatchedCount);
        if MatchedCount = 1 then Func := F;
      end;

      if ChkResult = caCompatible then
      begin
        Inc(CompatibleCount);
        if (CompatibleCount = 1) and (MatchedCount = 0) then Func := F;
      end;

      F := F.NextOverload;
    end;

    if MatchedCount > 0 then
    begin
      if MatchedCount = 1 then
        Result := coMatched
      else
        Result := coMultiMatched
    end
    else if CompatibleCount > 0 then
    begin
      if CompatibleCount = 1 then
        Result := coCompatible
      else
        Result := coMultiCompatible
    end;
  end;

  procedure ProcessDelayed(CallExpr: TBinaryExpr);
  var
    Args: TUnaryExpr;
    E: TExpr;
    NewExpr: TBinaryExpr;
    Ref: TSymbol;
  begin
    if CallExpr = nil then Exit;
    Args := TUnaryExpr(CallExpr.Right);
    if Args = nil then Exit;
    E := Args.Operand;
    while E <> nil do
    begin
      if eaCall in E.Attr then
      begin
        Exclude(E.Attr, eaDelayed);
        Exclude(E.Attr, eaCall);
        NewExpr := CreateCallExpr(E);
        Ref := E.GetReference;
        if Ref <> nil then
          NewExpr.Typ := GetReturnType(Ref);
        if NewExpr.Typ = nil then NewExpr.Typ := FContext.FUntype;
      end;
      E := TExpr(E.Next);
    end;
  end;
  procedure CheckArgs(var Func: TFunctionDecl; CallExpr: TBinaryExpr); overload;
  var
    ChkResult: TCheckOverloadResult;
    FirstOk, Old: TFunctionDecl;
  begin
    if fmOverload in Func.Modifiers then
    begin
      GetOverloadBegin(Func);
      Old := Func;
      FirstOk := nil;

      while Func <> nil do
      begin
        ChkResult := CheckFuncs(Func, CallExpr);
        case ChkResult of
          coCompatible:
            if FirstOk = nil then FirstOk := Func;
          coMatched:
            begin
              FirstOk := Func;
              Break;
            end;
          coMultiMatched:
            begin
              ParseError(CallExpr.Coord, 'Ambiguous overloaded call to %s', [Old.Name]);
              Exit;
            end;
        end;
        Func := GetOverloadNext;
      end;
      GetOverloadEnd;

      if FirstOk <> nil then
      begin
        Func := FirstOk;
        CheckArgs(Func.Args, CallExpr, True, True, True);
        ProcessDelayed(CallExpr);
        Exit;
      end;

      Func := Old;
      ParseError(CallExpr.Coord,
        'There is no overloaded version of %s that can be called with these arguments',
        [Old.Name]);
    end
    else if Func.NextOverload <> nil then
    begin
      Old := Func;
      ChkResult := CheckFuncs(Func, CallExpr);
      case ChkResult of
        coCompatible, coMatched: begin
          CheckArgs(Func.Args, CallExpr, True, True, True);
          ProcessDelayed(CallExpr);
          Exit;
        end;
        coMultiCompatible, coMultiMatched: begin
          ParseError(CallExpr.Coord, 'Ambiguous overloaded call to %s', [Old.Name]);
          Exit;
        end;
      end;
      Func := Old;
      ParseError(CallExpr.Coord,
        'There is no overloaded version of %s that can be called with these arguments',
        [Old.Name]);
    end
    else
    begin
      CheckArgs(Func.Args, CallExpr, True, False, True);
      ProcessDelayed(CallExpr);
    end;
  end;

  {// 查找一个无参有返回值函数
  procedure FindOverloadNoArgs(var Func: TFunctionDecl);
  begin
    if fmOverload in Func.Modifiers then
    begin
      Parser.GetOverloadBegin(Func);
    end
    else if Func.NextOverload <> nil then
    begin
      Old := Func;
      ChkResult := CheckFuncs(Func, CallExpr);
      case ChkResult of
        coCompatible, coMatched: Exit;
        coMultiCompatible, coMultiMatched:
          Parser.ParseError(Old.Coord, 'Ambiguous overloaded call to %s', [Old.Name]);
      end;
      Func := Old;
      Parser.ParseError(CallExpr.Coord,
        'There is no overloaded version of %s that can be called with these arguments',
        [Old.Name]);
    end;
  end;    }

//  function FindMatch
  procedure CheckBuiltinArgs(Kind: TBuiltinFunctionKind;
                             CallExpr: TBinaryExpr; var Typ: TType);
  var
    Num: Integer;
    E: TExpr;
    A1, A2, A3: TExpr;
    ActualArgs: TUnaryExpr;

    procedure CheckAbs;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.TypeCode in [typShortint..typUInt64, typComp..typCurrency]) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched2, ['abs']);
      Typ := A1.Typ;
    end;

    procedure CheckCopy;
    begin
      if Num = 3 then
      begin
      // copy string or array
        NotAllowNode([nkType], A1);
        NotAllowNode([nkType], A2);
        NotAllowNode([nkType], A3);

        if not A1.Typ.IsStringCompatible and (A1.Typ.TypeCode <> typDynamicArray) then
          ParseError(A1.Coord, SErr_IncompatibleTypes)
        else if not A2.Typ.IsInteger then
          ParseError(A2.Coord, SErr_IncompatibleTypes)
        else if not A3.Typ.IsInteger then
          ParseError(A3.Coord, SErr_IncompatibleTypes);
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched2, ['copy']);
      Typ := A1.Typ;
    end;

    procedure CheckAddr;
    begin
      if Num = 1 then begin
        if not (A1.HasMemory or A1.IsFunction) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched2, ['addr']);
      Typ := FContext.FTypes[typPointer];
    end;

    procedure CheckAssigned;
    begin
    // Assigned 的参数要求是指针
      if Num = 1 then begin
        NotAllowNode([nkType, nkFunc, nkMethod, nkExternalFunc], A1);
        if not A1.Typ.IsPointer and not A1.Typ.IsProcedural then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched2, ['assigned']);
      Typ := FContext.FTypes[typBoolean];
    end;

    // Break/Continue
    procedure CheckBrCont;
    begin
      if [psInWhileStmt, psInRepeatStmt, psInForEachStmt, psInForStmt]
          * FCurStates = [] then
        ParseError(CallExpr.Coord, SErr_OutsideOfLoop);
      if Num <> 0 then
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckChr;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not A1.Typ.IsInteger then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FCharType;
    end;

    procedure CheckDispose;
    begin
      // todo 1: 需要添加Dispose(obj, Init)这种形式
      // 必须是个指针
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not A1.Typ.IsPointer then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckExclude;
    var
      T: TType;
    begin
      if Num = 2 then begin
        if not (A1.HasMemory and (A1.Typ.TypeCode = typSet)) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
        T := A2.Typ;
        if T.TypeCode = typSubrange then
          T := TSubrangeType(T).BaseType;
//        if not ((A2.Typ.TypeCode = typSubrange) and
//                (TSubrangeType(A2).BaseType = TSetType(A1.Typ).RangeType.BaseType)) then
        if T <> TSetType(A1.Typ).RangeType.BaseType then
          ParseError(A2.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckExit;
    var
      V: TVariable;
    begin
      if not (psInFunc in FCurStates) then
        ParseError(CallExpr.Coord, SErr_IncompatibleTypes);
      if Num = 1 then begin
        V := A1.GetVariableSymbol;
        if (FCurFunction.ReturnType = nil) or
          not CheckAssignmentCompatibility(
            FCurFunction.ReturnType,
            A1.Typ) or ((V <> nil) and (vaResult in V.VarAttr)) then
        ParseError(A2.Coord, SErr_IncompatibleTypes);
      end
      else if Num <> 0 then
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);

      Typ := FContext.FAnytype;
    end;

    procedure CheckFinalize;
    begin
      if Num = 2 then
      begin
        if not (A1.HasMemory) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
        if not (A2.Typ.IsInteger) then
          ParseError(A2.Coord, SErr_IncompatibleTypes);
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckFreeMem;
    begin
      if Num in [1, 2] then
      begin
        NotAllowNode([nkType], A1);
        {if not A1.HasMemory then
          Parser.ParseError(A1.Coord, SErr_VarRequired);}
        if not A1.Typ.IsPointer then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
        if (Num = 2) and not A2.Typ.IsInteger then
          ParseError(A2.Coord, SErr_IncompatibleTypes);
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckGetMem;
    begin
      if Num = 2 then
      begin
        NotAllowNode([nkType], A2);
        if not A1.HasMemory then
          ParseError(A1.Coord, SErr_VarRequired);
        if not A1.Typ.IsPointer then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
        if not A2.Typ.IsInteger then
          ParseError(A2.Coord, SErr_IncompatibleTypes);
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckInc;
    begin
      if Num in [1, 2] then
      begin
        if Num > 1 then
          NotAllowNode([nkType], A2);
        if not A1.HasMemory then
          ParseError(A1.Coord, SErr_VarRequired);
        if A1.Typ.IsUntypePointer or not (A1.Typ.IsOrdinal or A1.Typ.IsPointer) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);

        if Num > 1 then
          if not (A2.Typ.IsInteger) then
            ParseError(A1.Coord, SErr_IncompatibleTypes);
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckHi;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.TypeCode in [typShortint..typUInt64]) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typByte];
    end;

    procedure CheckHigh;
    begin
    // high参数可以是类型名称如Shortint
      if Num = 1 then begin
        if not ((A1.Typ.TypeCode in [typArray, typDynamicArray, typShortString])
                or A1.Typ.IsOrdinal) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);

      if (A1 <> nil) and (A1.Typ.IsOrdinal) then
        Typ := A1.Typ
      else
        Typ := FContext.FIntegerType;
    end;

    procedure CheckLength;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.TypeCode in [typArray, typDynamicArray, typAnsiString..typShortString]) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FIntegerType;
    end;

    procedure CheckNew;
    begin
      // todo 1: 需要添加New(obj, Init)这种形式
      // New需要指针类型的参数
      if Num = 1 then begin
        if not A1.HasMemory then
          ParseError(A1.Coord, SErr_VarRequired);
        if not A1.Typ.IsPointer then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
//        NotAllowNode([nkType]);
//        if not (A1.HasMemory and (Typ.TypeCode = typPointer)) then
//          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckOdd;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.IsInteger) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typBoolean];
    end;

    function GetUnderlineType(T: TType): TType;
    begin
      case A1.Typ.TypeCode of
        typEnum:
          Result := FContext.TypeOfRange(
                        TEnumType(A1.Typ).LowValue,
                        TEnumType(A1.Typ).HighValue
                      );
        typShortint..typUInt64:
          Result := T;
        typBoolean, typByteBool:
          Result := FContext.FTypes[typByte];
        typWordBool:
          Result := FContext.FTypes[typWord];
        typLongBool:
          Result := FContext.FTypes[typLongWord];
        typAnsiChar:
          Result := FContext.FTypes[typByte];
        typWideChar:
          Result := FContext.FTypes[typWord];
        typSubrange:
          Result := GetUnderlineType(TSubrangeType(T).BaseType);
      else
        Result := FContext.FIntegerType;
      end;
    end;

    procedure CheckOrd;
    begin
    // ord 参数可以是enum,int
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.IsOrdinal) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := GetUnderlineType(A1.Typ);
    end;

    procedure CheckPred;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.IsOrdinal) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := A1.Typ;
    end;

    procedure CheckPtr;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.IsInteger) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typPointer];
    end;

    procedure CheckRound;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.TypeCode in [typShortint..typUInt64, typComp..typCurrency]) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typInt64];
    end;

    procedure CheckSetLength;
    begin
      if Num > 1 then
      begin
        // A1必须是变量
        if not ((A1.HasMemory) and
                (A1.Typ.TypeCode in [typAnsiString..typShortString, typDynamicArray])) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
        // 如果是DynamicArray，必须检查维数
      end
      else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FAnytype;
    end;

    procedure CheckSizeOf;
    begin
      // SizeOf参数可以为任何表达式,只根据类型计算大小
(*      if Num = 1 then begin
      {  if not (A1.NodeKind in [nkVariable, nkConstant, nkType, nkEnumElement, nkFunc, nkExpr]) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);}
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);*)
      if Num <> 1 then
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FIntegerType;
    end;

    procedure CheckSwap;
    begin
      if Num = 1 then begin
        NotAllowNode([nkType], A1);
        if not (A1.Typ.IsInteger) then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typWord];
    end;

    procedure CheckTypeInfo;
    begin
      if Num = 1 then begin
        if not A1.IsTypeSymbol then
          ParseError(A1.Coord, SErr_IncompatibleTypes);
      end else
        ParseError(CallExpr.Coord, SErr_ArgNotMatched);
      Typ := FContext.FTypes[typPointer];
    end;

    function createConst(Value: Integer): TConstExpr;
    begin
      Result := CreateConstExpr(opINTCONST);
      Result.Value := ValFromInt(Value);
    end;
  begin
    ActualArgs := TUnaryExpr(CallExpr.Right);
    if ActualArgs <> nil then
    begin
      Assert(ActualArgs.OpCode = opLIST);
      Num := 0; A1 := nil; A2 := nil; A3 := nil;
      E := ActualArgs.Operand;
      while E <> nil do
      begin
        case Num of
          0: A1 := E;
          1: A2 := E;
          2: A3 := E;
          else Break;
        end;
        Inc(Num);
        E := TExpr(E.Next);
      end;

 {     // 补充缺省参数
      case Kind of
        bfInc, bfDec, bfInitialize, bfFinalize:
          if Num = 1 then
          begin
            A2 := createConst(1);
            A1.Next := A2;
          end;
      end;}
    end
    else
      Num := 0;

    case Kind of
      bfAbs:          CheckAbs;
      bfAddr:         CheckAddr;
      bfAssigned:     CheckAssigned;
      bfBreak,
      bfContinue:     CheckBrCont;
      bfChr:          CheckChr;
      bfCopy:         CheckCopy;
      bfDispose:      CheckDispose;
      bfDec, bfInc:   CheckInc;
      bfExclude,
      bfInclude:      CheckExclude;
      bfExit:         CheckExit;
      bfFinalize,
      bfInitialize:   CheckFinalize;
      bfFreeMem:      CheckFreeMem;
      bfGetMem:       CheckGetMem;
      bfHi, bfLo:     CheckHi;
      bfHigh, bfLow:  CheckHigh;
      bfLength:       CheckLength;
      bfNew:          CheckNew;
      bfOdd:          CheckOdd;
      bfOrd:          CheckOrd;
      bfPred, bfSucc: CheckPred;
      bfPtr:          CheckPtr;
      bfRound,
      bfTrunc:        CheckRound;
      bfSetLength:    CheckSetLength;
      bfSizeOf:       CheckSizeOf;
      bfSwap:         CheckSwap;
      bfTypeInfo:     CheckTypeInfo;
    else
      ParseError(CallExpr.Coord, 'Invalid builtin routine', True); // 不会到此
    end;
    if (Typ = FContext.FAnytype) and (CallExpr.Parent = nil) then
      Typ := FContext.FUntype;

    // todo 999: 如果增加了内置函数，则需要检查这里
    if Kind in [bfChr, bfHi, bfHigh, bfLength, bfLo, bfLow, bfOdd,
                bfOrd, bfPred, bfRound, bfSucc, bfSwap, bfTrunc] then
    begin
      if (A1 <> nil) and (eaConst in A1.Attr) then
        Include(CallExpr.Attr, eaConst);
    end
    else if Kind = bfSizeOf then
      Include(CallExpr.Attr, eaConst);
  end;

  function ValueCastOk(typ: TType): Boolean;
  begin
    Result := typ.IsOrdinal or (typ.TypeCode in [typPointer, typPAnsiChar,
                typPWideChar, typProcedural, typClass, typClassRef,
                typInterface, typDispInterface]);
  end;

  function CheckValueCast(L, R: TType): Boolean;
  begin
    if L.TypeCode in [typVariant, typOleVariant] then
      Result := R.IsOrdinal or R.IsReal or (R.TypeCode in [typInterface, typDispInterface])
    else if R.TypeCode in [typVariant, typOleVariant] then
      Result := L.IsOrdinal or L.IsReal or (L.TypeCode in [typInterface, typDispInterface])
    else if L.IsReal then
      Result := R.IsOrdinal or R.IsReal
    else
      Result := ValueCastOk(L) and ValueCastOk(R);
  end;

  function CheckAdjustCall(E: TExpr): TExpr; forward;

  function CheckCast(bin: TBinaryExpr): TExpr;
  var
    L, R: TType;
    E: TExpr;
    HasMem, Ok: Boolean;
  begin
    Result := nil;
    L := bin.Left.Typ.NormalType;
    E := bin.Right;
    bin.Typ := L;
    if CountOfArgs(TUnaryExpr(E)) <> 1 then
    begin
      ParseError(bin.Coord, SErr_InvalidCast);
      Exit;
    end;

    EnsureNode(nkType, bin.Left);
    NotAllowNode([nkType], bin.Right);
    R := TUnaryExpr(E).Operand.Typ.NormalType;

    HasMem := TUnaryExpr(E).Operand.HasMemory;

  // see http://docwiki.embarcadero.com/RADStudio/en/Expressions_(Delphi)#Typecasts

    // todo 5: cast需要再深入研究一下
    {
    ordinal type => ordinal type, pointer, variant, (set, array or record with memory of equal size)
    pointer type => ordinal type, pointer, string, class, classref, interface
    variant => ordinal, real, interface
     (set, array or record) => ordinal type , pointer with memory of equal size
    }
    { value cast    : 必须都是指针或有序类型
      variable cast : 被转换的必须有内存，并且是无类型或者与目的类型尺寸相等
    }
    if HasMem then
    begin
      Ok := not (L.IsInteger and R.IsReal);
      if Ok then
      begin
        Ok := (L.Size = R.Size) or (R.TypeCode = typUntype);
        if not Ok then
          Ok := CheckValueCast(L, R)
        else
          Include(bin.Attr, eaVarCast);
      end;
    end
    else
    begin
      Ok := CheckValueCast(L, R);
      {Ok := not (psInLeftVal in FCurStates);
      if Ok then
        Ok := CheckValueCast(L, R);}
    end;
(*
    // 如果右边是无类型,则转换总是成功的
    if R = Parser.FContext.FUntype then
    begin
      if bin.Typ.TypeCode = typProcedural then
        Result := CheckAdjustCall(bin);
      Exit;
    end;

    if L.IsOrdinal then
    begin
      Ok := not (R.TypeCode in [typComp..typCurrency]);
      if Ok and (R.TypeCode in [typRecord, typSet, typArray]) then
        Ok := R.Size = L.Size;
      // todo 2: 此处有问题，比如
      // var f: textfile;  integer(f);
      // 另外, i := byte(['a']);
    end
    else
    begin
      if L.TypeCode in [typRecord, typSet] then
        Ok := L.Size = R.Size
      else if L.TypeCode in [typVariant, typOleVariant] then
        Ok := R.IsOrdinal or R.IsReal or (R.TypeCode in [typInterface, typDispInterface])
      else if R.TypeCode in [typVariant, typOleVariant] then
        Ok := L.IsOrdinal or L.IsReal or (L.TypeCode in [typInterface, typDispInterface])
      else
        Ok := ValueCastOk(L) and ValueCastOk(R);
    end;  *)

    if not Ok then
      ParseError(bin.Coord, SErr_InvalidCast)
    else begin
      // 最终结果如果是函数,应该检查是否进行函数调用
      if bin.Typ.TypeCode = typProcedural then
        Result := CheckAdjustCall(bin);
    end;
  end;

  function CheckCall(bin: TBinaryExpr): TExpr;
  var
    L: TExpr;
    Typ: TType;
    Ref: ^TSymbol;
  begin
  {
    Call的左节点有两种情况：
    1. 符号
    2. 返回函数指针的表达式
  }
    Result := nil;
    L := bin.Left;
    if L.OpCode = opMEMBER then
    begin
      ASSERT(TBinaryExpr(L).Right.OpCode = opSYMBOL, 'CheckCall');
      Ref := @TSymbolExpr(TBinaryExpr(L).Right).Reference;
    end
    else if L.OpCode = opSYMBOL then
      Ref := @TSymbolExpr(L).Reference
    else
      Ref := nil;

//    if (Ref = nil) then Exit;

    if (Ref <> nil) and (Ref^ <> nil) and (Ref^.NodeKind in [nkType, nkFunc,
                              nkMethod, nkExternalFunc, nkBuiltinFunc]) then
    begin
      case Ref^.NodeKind of
        nkType: begin
          bin.OpCode := opCAST;       // check type cast?
          Result := CheckCast(bin);
        end;

        nkFunc, nkMethod, nkExternalFunc: begin
          CheckArgs(TFunctionDecl(Ref^), bin);
          bin.Typ := TFunctionDecl(Ref^).ReturnType;
          if bin.Typ = nil then
            if bin.Parent <> nil then
              bin.Typ := FContext.FAnytype
            else
              bin.Typ := FContext.FUntype;
        end;

        nkBuiltinFunc: begin
          Typ := nil;
          CheckBuiltinArgs(TBuiltinFunction(Ref^).Kind, bin, Typ);
          bin.Typ := Typ;
          TSymbolExpr(L).Typ := Typ;
        end;
      else
        ParseError(bin.Coord, SErr_InvalidOperand);
      end;
    end
    else if (L.Typ.IsProcedural) then
    begin  // variable of procedural?
      NotAllowNode([nkType], L);
      CheckArgs(TProceduralType(L.Typ).Args, bin);
      bin.Typ := TProceduralType(L.Typ).ReturnType;
      if bin.Typ = nil then
      begin
        if bin.Parent = nil then
          bin.Typ := FContext.FUntype
        else
          bin.Typ := FContext.FAnytype;
      end;
    end
    else
      ParseError(bin.Coord, SErr_InvalidOperand);
  end;

  // 检查是否有个@运算置于表达式之前
  function AddrOpPrefix(E: TExpr): Boolean;
  begin
    Result := True;
    E := E.Parent;
    while E <> nil do
    begin
      if E.OpCode = opADDR then Exit;
      E := E.Parent;
    end;
    Result := False;
  end;

  function IsCallNeed(E: TExpr; Ref: TSymbol; FuncT: TProceduralType): Boolean;
  var
    T: TType;
  begin
    if psInAccessor in FCurStates then
    begin
      Result := False;
      Exit;
    end;

    if (E.Parent <> nil) and (E.Parent.OpCode = opMEMBER) then
      E := E.Parent;

    if E.Parent = nil then
    begin
      if psInLeftVal in FCurStates then
      begin
        if (Ref <> nil) and (Ref.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
          Result := True   // 函数不能被赋值
        else
          Result := False  // 这时无法决定是否是函数调用。要等进一步分析
      end
      else if FExpectedProcType then
        Result := False
      else
        Result := True;
    end
    else if E.Parent.OpCode = opCALL then
      Result := False      // 已经是Call
    else if AddrOpPrefix(E) then
      Result := False      // 取地址
    else
    begin
      if (Ref <> nil) and (Ref.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
        T := TFunctionDecl(Ref).ReturnType
      else if FuncT <> nil then
        T := FuncT.ReturnType
      else
        T := nil;

      // 1.有返回值 2.父级是参数列表
      if (T <> nil) and (eaArgList in E.Parent.Attr) then
      begin
        Include(E.Attr, eaDelayed); // 到父级再决定是否调用
        Result := False;
      end
      else
        Result := T <> nil;//False;
    end;
  end;

  // 检查并创建opCALL表达式
  // 参数E 是返回procedural type的表达式
  function CheckAdjustCall(E: TExpr): TExpr;
  begin
    Result := nil;
    if E.Typ.TypeCode <> typProcedural then Exit;

    if IsCallNeed(E, nil, TProceduralType(E.Typ)) then
    begin
      Result := CreateCallExpr(E);
      CheckCall(TBinaryExpr(Result));
    end;
  end;

  procedure InsertExpr(P, Child, OldChild: TExpr);
  begin
    if P <> nil then
    begin
      if P.ClassType = TBinaryExpr then
      begin
        if TBinaryExpr(P).Left = OldChild then
          TBinaryExpr(P).Left := Child
        else
          TBinaryExpr(P).Right := Child;
      end
      else if P.ClassType = TUnaryExpr then
      begin
        if P.OpCode = opLIST then
          Child.Parent := P
        else
          TUnaryExpr(P).Operand := Child;
      end;
    end;
  end;

  procedure CheckVisibility(Sym: TSymbol);
  var
    IsVis: Boolean;
    F: TSymbol;
  begin
    if FCurFunction <> nil then
    begin
    (*// 查找顶级函数
      F := Parser.FCurFunction;
      while (F.Parent <> nil) and
        (F.Parent.NodeKind in [nkFunc, nkMethod]) do
      begin
        F := TFunction(F.Parent);
      end; *)
      F := FTopFunction;
      IsVis := Self.IsVisible(F, Sym)
    end
    else
      IsVis := Self.IsVisible(FCurParent, Sym);
    if not IsVis then
      ParseError(SErr_SymbolNotAccess, [Sym.Name]);
  end;

  // 检查符号, 如果这个符号是个函数, 可能会创建一个opCALl的表达式
  function CheckSymbol(sym: TSymbolExpr): TExpr;
  var
    Ref: TSymbol;
  begin
    Result := nil;
    if sym.Reference <> nil then
      Ref := sym.Reference
    else
      Ref := FindSymbol(sym.Name);

    if Ref = nil then
      ParseError(sym.Coord, SErr_UndeclaredIdent, [sym.Name])
    else begin
      Include (Ref.Attr, saUsed);
      if (Ref.Parent = nil) and (Ref.NodeKind <> nkModule) then
        ParseError(SErr_InternalError, ['CheckSymbol:Parent=nil'], True);
      CheckVisibility(Ref);
      sym.Reference := Ref;
      case Ref.NodeKind of
//        nkNameScope: sym.Typ := TNameScope(E).GetType;
//        nkModule: sym.Typ := TModule(E).GetType;
        // 因为NameScope和Module已经在ParseExpr中去掉了,因此这里不会出现
        nkNameScope, nkModule:
          Assert(False, 'NameScope and Module should not be occurred');
        nkType:
          sym.Typ := TType(Ref);
        // Var,const, arg, field, 如果类型是procedure,则需要检查是函数调用还是当成变量
        nkVariable, nkArgument, nkConstant, nkField, nkProperty, nkIntfProperty: begin
          case Ref.NodeKind of
            nkVariable:     Sym.Typ := TVariable(Ref).VarType;
            nkArgument:     Sym.Typ := TArgument(Ref).ArgType;
            nkConstant:     Sym.Typ := TConstant(Ref).ConstType;
            nkField:        Sym.Typ := TField(Ref).FieldType;
            nkProperty:     Sym.Typ := TProperty(Ref).PropType;
            nkIntfProperty: Sym.Typ := TIntfProperty(Ref).PropType;
          end;

          if Sym.Typ = nil then Sym.Typ := FContext.FUntype;

          if FCurFunction <> nil then
          begin
            case Ref.NodeKind of
              nkVariable:
                if (FCurFunction.Level > TVariable(Ref).Level)
                    and (vaLocal in TVariable(Ref).VarAttr)
                    {and (TVariable(Ref).Level > 0)} then
                begin
                  Include(TVariable(Ref).States, vsNestRef);
                  Include(FCurFunction.FuncAttr, faNeedFPArg);
                end;
              nkArgument:
                if (FCurFunction.Level > TArgument(Ref).Level)
                    {and (TArgument(Ref).Level > 0)} then
                begin
                  Include(TArgument(Ref).States, asNestRef);
                  Include(FCurFunction.FuncAttr, faNeedFPArg);
                end;
            end;
          end;

          if Sym.Typ.TypeCode = typProcedural then
            if IsCallNeed(Sym, Ref, TProceduralType(Sym.Typ)) then
            begin
              Result := CreateCallExpr(Sym);
              CheckCall(TBinaryExpr(Result));
            end;
        end;

        nkEnumElement: sym.Typ := TEnumValue(Ref).EnumType;

        nkExternalFunc, nkFunc, nkMethod: begin
          Sym.Typ := FContext.FUntype;
          if IsCallNeed(Sym, Ref, nil) then
          begin
            Result := CreateCallExpr(Sym);
            CheckCall(TBinaryExpr(Result));

            if (FCurFunction <> nil) and (Ref.NodeKind = nkFunc)
              and (TFunction(Ref).Level > 0)
              and (TFunction(Ref).Level <= FCurFunction.Level) then
            begin
              Include(FCurFunction.FuncAttr, faNeedFPArg);
            end;
          end
          else
            Sym.Typ := TFunctionDecl(Ref).ProceduralType;
        end;

        nkBuiltinFunc: begin
          Sym.Typ := FContext.FUntype;
          if (Sym.Parent = nil) or (Sym.Parent.OpCode <> opCALL) then
          begin
            Result := CreateCallExpr(Sym);
            CheckCall(TBinaryExpr(Result));
          end;
        end;
        else
          ParseError(Sym.Coord, 'Unknown type of Identifier', True);
      end;
    end;
    if Sym.Typ = nil then Sym.Typ := FContext.FAnytype;
    if (Result <> nil) and (Result.Typ = nil) then Result.Typ := Sym.Typ;
  end;

  function CheckSubSymbol(bin: TBinaryExpr): TExpr;  // opMEMBER

    function CheckRecPtr(typ: TPointerType; sym: TSymbolExpr): TSymbol;
    begin
      if typ.RefType <> nil then
        case typ.RefType.TypeCode of
          typRecord: begin
            Result := TRecordType(typ.RefType).FindSymbol(sym.Name);
            Exit;
          end;
          typObject: begin
            Result := TObjectType(typ.RefType).FindSymbol(Sym.Name);
            Exit;
          end;
        end;

      ParseError(bin.Coord, SErr_InvalidOperand);
      Result := nil;
    end;

    function IsConstructor(Ref: TSymbol): Boolean;
    begin
      Result := (Ref.NodeKind = nkMethod) and (TMethod(Ref).MethodKind = mkConstructor);
    end;
  var
    Ref: TSymbol;
    Sym: TSymbolExpr;
  begin
    Result := nil;
    if bin.Right.OpCode <> opSYMBOL then
    begin
      ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := FContext.FAnytype;
      Exit;
    end;

    Sym := TSymbolExpr(bin.Right);
    Ref := Sym.Reference;
    if Ref = nil then
    begin
      case bin.Left.Typ.TypeCode of
        // 因为NameScope和Module已经在ParseExpr中去掉了,因此这里不会出现
      {  typSymbol: begin
          Ref := TSymbolExpr(bin.Left).Reference;
          if Ref.NodeKind = nkNameScope then
            Ref := TNameScope(Ref).FindSymbol(Sym.Name)
          else if Ref.NodeKind = nkModule then
            Ref := TModule(Ref).FindSymbol(Sym.Name)
          else
            Ref := nil;
        end;}

        typPointer: begin
          NotAllowNode([nkType], bin.Left);
          Ref := CheckRecPtr(TPointerType(bin.Left.Typ), Sym);
        end;

        typRecord: begin
          Ref := TRecordType(bin.Left.Typ).FindSymbol(Sym.Name);
          if bin.Left.IsTypeSymbol then
            if ([saStatic, saClass] * Ref.Attr = []) and (Ref.NodeKind <> nkConstant) then
              ParseError(bin.Coord, SErr_SymbolNotClassOrStatic, [Sym.Name]);
        end;

        typClass: begin
          Ref := TClassType(bin.Left.Typ).FindSymbol(Sym.Name);
          if bin.Left.IsTypeSymbol then
            if ([saStatic, saClass] * Ref.Attr = []) and (Ref.NodeKind <> nkConstant) and not IsConstructor(Ref) then
              ParseError(bin.Coord, SErr_SymbolNotClassOrStatic, [Sym.Name]);
        end;

        typClassRef: begin
          Ref := TClassRefType(bin.Left.Typ).RefType.FindSymbol(Sym.Name);
            if ([saStatic, saClass] * Ref.Attr = []) and (Ref.NodeKind <> nkConstant) and not IsConstructor(Ref) then
              ParseError(bin.Coord, SErr_SymbolNotClassOrStatic, [Sym.Name]);
        end;

        typObject: begin
          Ref := TObjectType(bin.Left.Typ).FindSymbol(Sym.Name);
          if bin.Left.IsTypeSymbol then
            if ([saStatic, saClass] * Ref.Attr = []) and (Ref.NodeKind <> nkConstant) then
              ParseError(bin.Coord, SErr_SymbolNotClassOrStatic, [Sym.Name]);
        end;

        typInterface, typDispInterface:
          Ref := TInterfaceType(bin.Left.Typ).FindSymbol(Sym.Name);
        // todo 5: 以后要加helper类
      else
        ParseError(bin.Coord, SErr_InvalidOperand);
      end;
    end;

    if Ref = nil then
    begin
      ParseError(bin.Coord, SErr_UndeclaredIdent, [Sym.Name]);
      Sym.Typ := FContext.FAnytype;
    end
    else
    begin
      Sym.Reference := Ref;
      // 因为NameScope和Module已经在ParseExpr中去掉了,因此这里不会出现
      if Ref.NodeKind in [nkNameScope, nkModule] then
        ParseError(bin.Coord, SErr_InvalidIdent, [Ref.Name]);

      CheckVisibility(Ref);

      case Ref.NodeKind of
        nkType: Sym.Typ := TType(Ref);
        nkVariable, nkArgument, nkConstant,
        nkField, nkProperty, nkIntfProperty: begin
          case Ref.NodeKind of
            nkVariable: Sym.Typ := TVariable(Ref).VarType;
            nkArgument: Sym.Typ := TArgument(Ref).ArgType;
            nkConstant: Sym.Typ := TConstant(Ref).ConstType;
            nkField: Sym.Typ := TField(Ref).FieldType;
            nkProperty: Sym.Typ := TProperty(Ref).PropType;
            nkIntfProperty: Sym.Typ := TIntfProperty(Ref).PropType;
          end;

          if Sym.Typ.TypeCode = typProcedural then
            if IsCallNeed(Sym, Ref, TProceduralType(Sym.Typ)) then
            begin
              Result := CreateCallExpr(Sym);
              CheckCall(TBinaryExpr(Result));
            end;
        end;

        nkEnumElement: Sym.Typ := TEnumValue(Ref).EnumType;

        nkFunc, nkMethod, nkExternalFunc: begin
          Sym.Typ := FContext.FUntype;
          if IsCallNeed(Sym, Ref, nil) then
          begin
            Result := CreateCallExpr(Sym);
            CheckCall(TBinaryExpr(Result));
          end
          else
            Sym.Typ := TFunctionDecl(Ref).ProceduralType;
        end;

        nkBuiltinFunc: begin
          Sym.Typ := FContext.FUntype;
          if Sym.Parent.OpCode <> opCALL then
            CheckCall(CreateCallExpr(Sym));
        end;
      else
        ParseError(Sym.Coord, 'Unknown type of Identifier');
      end;
      if Sym.Typ = nil then
        Sym.Typ := FContext.FAnytype;
    end;

    if bin <> nil then
      bin.Typ := Sym.Typ;
  end;

  function CheckInst(un: TUnaryExpr): TExpr;
  var
    Typ: TType;
  begin
    Result := nil;
    NotAllowNode([nkType], un.Operand);
    Typ := un.Operand.Typ;
    case Typ.TypeCode of
      typPointer: begin
        if TPointerType(Typ).RefType <> nil then
          un.Typ := TPointerType(Typ).RefType
        else
          un.Typ := FContext.FUntype;
        if un.Typ.TypeCode = typProcedural then
          Result := CheckAdjustCall(un);
      end;
      typPAnsiChar:
        un.Typ := FContext.FTypes[typAnsiChar];
      typPWideChar:
        un.Typ := FContext.FTypes[typWideChar];
    else
      ParseError(un.Coord, SErr_InvalidOperand);
    end;
  end;

  // 左边必须和右边类型一致
  procedure CheckRange(bin: TBinaryExpr);
  var
    L, R: TType;
  begin
    NotAllowNode([nkType], bin.Left);
    NotAllowNode([nkType], bin.Right);

    L := bin.Left.Typ;
    R := bin.Right.Typ;
    if L.TypeCode = typSubrange then L := TSubrangeType(L).BaseType;
    if R.TypeCode = typSubrange then R := TSubrangeType(R).BaseType;

    if L.IsInteger then
    begin
      if not R.IsInteger then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := L;
    end
    else if L.IsBoolean then
    begin
      if not R.IsBoolean then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := FContext.FTypes[typBoolean];
    end
    else if L.TypeCode in [typAnsiChar, typWideChar] then
    begin
      if not (R.TypeCode in [typAnsiChar, typWideChar]) then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := L;
    end
    else if L.TypeCode = typEnum then
    begin
      if R <> L then
        ParseError(bin.Coord, SErr_InvalidOperand);
      bin.Typ := L;
    end
    else begin
      bin.Typ := FContext.FIntegerType;
      ParseError(bin.Coord, SErr_InvalidOperand);
    end;
  end;

  procedure CheckIndexProperty(bin: TBinaryExpr; Ref: TSymbol);
  var
    Args: TList;
  begin
    case Ref.NodeKind of
      nkProperty: begin
        Args := TProperty(Ref).Args;
        bin.Typ := TProperty(Ref).PropType;
      end;
      nkIntfProperty: begin
        Args := TIntfProperty(Ref).Args;
        bin.Typ := TIntfProperty(Ref).PropType;
      end;
    else
      Args := nil;
    end;

    CheckArgs(Args, bin);
  end;

  procedure CheckDefaultProperty(bin: TBinaryExpr; Ref: TSymbol);
  var
    LTyp: TType;
    DefProp: TSymbol;
    NewL: TBinaryExpr;
    PropExpr: TSymbolExpr;
  begin
    LTyp := bin.Left.Typ;
    case LTyp.TypeCode of
      typClass:
        DefProp := TClassType(LTyp).DefaultProp;

      typInterface, typDispInterface:
        DefProp := TInterfaceType(LTyp).DefaultProp;
    else
      Exit;
    end;
    PropExpr := CreateSymbolExpr(DefProp.Name);
    PropExpr.Reference := DefProp;
    PropExpr.Coord := bin.Coord;
    NewL := CreateBinaryExpr(opMEMBER, bin.Left, PropExpr);
    bin.Left := NewL;
    NewL.Coord := bin.Coord;
    CheckSubSymbol(NewL);
    CheckIndexProperty(bin, DefProp);
  end;
  {
  function CheckIndexType(Typ: TType; E: TExpr): Boolean;

    function CheckRangeType(t1, t2: TType): Boolean;
    begin
      if t1.TypeCode = typSubrange then t1 := TSubrangeType(t1).BaseType;
      if t2.TypeCode = typSubrange then t2 := TSubrangeType(t2).BaseType;
      case t1.TypeCode of
        typEnum: Result := t1 = t2;
        typShortint..typUInt64:
          Result := t2.TypeCode in [typShortint..typUInt64, typVariant..typOleVariant];
        typVariant..typOleVariant:
          Result := True;
      else
        Result := t1.TypeCode = typ.TypeCode;
      end;
    end;
  begin
    if Typ = nil then
    begin
      Parser.ParseError(E.Coord, SErr_ArrayRequired);
      Result := False;
      Exit;
    end;

    Result := True;
    case Typ.TypeCode of
      typAnsiString..typShortString,
      typPAnsiChar, typPWideChar:
        if not E.Typ.IsInteger then
          Parser.ParseError(E.Coord, SErr_IncompatibleTypes);
      typDynamicArray:
        if not E.Typ.IsInteger then
          Parser.ParseError(E.Coord, SErr_IncompatibleTypes);
      typArray:
        if not CheckRangeType(E.Typ, TArrayType(Typ).Range) then
          Parser.ParseError(E.Coord, SErr_IncompatibleTypes);
    else
      Parser.ParseError(E.Coord, SErr_ArrayRequired);
      Result := False;
    end;
  end;  }

  // 检查此维
  function CheckIndexType(var Typ: TType; E: TExpr): Boolean;

    function CheckIndiceType(t1, t2: TType): Boolean;
    begin
      // t1: 要检查的类型, t2: 期待的类型
      if t1.TypeCode = typSubrange then t1 := TSubrangeType(t1).BaseType;
      if t2.TypeCode = typSubrange then t2 := TSubrangeType(t2).BaseType;
      case t1.TypeCode of
        typEnum: Result := t1 = t2;
        typShortint..typUInt64:
          Result := t2.TypeCode in [typShortint..typUInt64, typVariant..typOleVariant];
        typVariant..typOleVariant:
          Result := True;
      else
        Result := t1.TypeCode = typ.TypeCode;
      end;
    end;
  var
    ElemTyp: TType;
  begin
    if Typ = nil then
    begin
      ParseError(E.Coord, SErr_ArrayRequired);
      Result := False;
      Exit;
    end;

    Result := True;
    case Typ.TypeCode of
      typAnsiString, typShortString, typPAnsiChar:
        begin
          ElemTyp := FContext.FTypes[typAnsiChar];
          if not CheckIndiceType(E.Typ, FContext.FTypes[typLongint]) then
            ParseError(E.Coord, SErr_IncompatibleTypes);
        end;

      typWideString, typUnicodeString, typPWideChar:
        begin
          ElemTyp := FContext.FTypes[typWideChar];
          if not CheckIndiceType(E.Typ, FContext.FTypes[typLongint]) then
            ParseError(E.Coord, SErr_IncompatibleTypes);
        end;

      typPointer:
        begin
          if Typ.IsUntypePointer then
            ParseError(E.Coord, SErr_ArrayRequired);
          ElemTyp := TPointerType(Typ).RefType;
          if ElemTyp = nil then ElemTyp := FContext.FUntype;
          if not CheckIndiceType(E.Typ, FContext.FTypes[typLongint]) then
            ParseError(E.Coord, SErr_IncompatibleTypes);
        end;

      typDynamicArray:
        begin
          ElemTyp := TDynamicArrayType(Typ).ElementType;
          if not CheckIndiceType(E.Typ, FContext.FTypes[typLongint]) then
            ParseError(E.Coord, SErr_IncompatibleTypes);
        end;

      typArray:
        begin
          ElemTyp := TArrayType(Typ).ElementType;
          if not CheckIndiceType(E.Typ, TArrayType(Typ).Range) then
            ParseError(E.Coord, SErr_IncompatibleTypes);
        end;
    else
      ParseError(E.Coord, SErr_ArrayRequired);
      ElemTyp := nil;
      Result := False;
    end;
    if ElemTyp <> nil then Typ := ElemTyp;
  end;

  function CheckIndex(bin: TBinaryExpr): TExpr;
  var
    E: TExpr;
    Typ: TType;
    Ref: TSymbol;
    IsArrayProp, IsDefProp: Boolean;
  begin
    Result := nil;
    E := bin.Right;
    if (E = nil) or (TUnaryExpr(E).Operand = nil) then
    begin
      ParseError(bin.Coord, SErr_ExpectExpression);
      Exit;
    end;

    NotAllowNode([nkType], bin.Left);

    Ref := bin.Left.GetReference;

    { 考虑这种情况:
      obj.Symbols[i];
      如果Symbols是个Array Property,则IsArrayProp=True
      如果Symbols是个class,而且这个class有缺省属性,则IsDefProp=True
    }

    IsArrayProp := False;
    IsDefProp := False;
    if Ref <> nil then
      case Ref.NodeKind of
        nkProperty: IsArrayProp := TProperty(Ref).CountOfArgs > 0;
        nkIntfProperty: IsArrayProp := TIntfProperty(Ref).CountOfArgs > 0;
      end;

    if not IsArrayProp then
      case bin.Left.Typ.TypeCode of
        typClass: IsDefProp := TClassType(bin.Left.Typ).DefaultProp <> nil;
        typInterface, typDispInterface:
          IsDefProp := TInterfaceType(bin.Left.Typ).DefaultProp <> nil;
      end;

    if IsArrayProp or IsDefProp then
      Include(bin.Attr, eaArrayProp);

    if IsArrayProp then
    begin
      CheckIndexProperty(bin, Ref);
    end
    else if IsDefProp then
    begin
      // 转成对缺省属性的访问,并检查
      CheckDefaultProperty(bin, Ref);
    end
    else
    begin
      E := TUnaryExpr(E).Operand;
      case bin.Left.Typ.TypeCode of
        typAnsiString..typShortString,
        typPAnsiChar, typPWideChar:
          begin
            if E.Next <> nil then
              ParseError(bin.Coord, SErr_ArrayRequired);
            Typ := bin.Left.Typ;
            CheckIndexType(Typ, E);
            bin.Typ := Typ;
          end;

        typArray, typDynamicArray, typPointer:
          begin
            Typ := bin.Left.Typ;
            repeat
              if not CheckIndexType(Typ, E) then Break;
              bin.Typ := Typ;
              E := TExpr(E.Next);
            until (E = nil) or (Typ = nil);
          end;

      else
        if bin.Left.Typ.TypeCode in [typClass, typInterface, typDispInterface] then
          ParseError(bin.Coord, SErr_NoDefaultProp)
        else
          ParseError(bin.Coord, SErr_ArrayRequired);
        bin.Typ := FContext.FAnytype;
      end;
    end;

    // 如果这个Property返回一个过程变量,应该检查是否转为调用
    // 如: Ctrl.OnClick[1];
    // Ctrl.OnClick;
    // Ctrl.Procs[1];
    if bin.Typ.TypeCode = typProcedural then
      Result := CheckAdjustCall(bin);
  end;

  procedure CheckAddrOp(un: TUnaryExpr);
    function IsVarConstField(E: TExpr): Boolean;
    var
      Sym: TSymbol;
    begin
      Sym := E.GetReference;
      Result := (Sym <> nil) and (Sym.NodeKind in [nkField, nkArgument, nkVariable]);
    end;
  var
    Func: TFunctionDecl;
    Ref: TSymbol;
  begin
    // @@VarOfProcedural
    if (un.Parent <> nil) and (un.Parent.OpCode = opADDR) then
    begin
      // @@操作符必须指向变量、参数、字段
      if not (IsVarConstField(un) and un.Operand.Typ.IsProcedural) then
        ParseError(un.Coord, SErr_InvalidOperand);
      un.Typ := FContext.FTypes[typPointer];
    end
    else
    begin
      if not (un.Operand.HasMemory or un.Operand.IsFunction) then
      begin
        ParseError(un.Coord, SErr_InvalidOperand);
        un.Typ := FContext.FTypes[typPointer];
        Exit;
      end;

      Ref := un.Operand.GetReference;
      if (Ref <> nil) and (Ref.NodeKind = nkVariable)
          and (vaSelf in TVariable(Ref).VarAttr) then
      begin
        ParseError(un.Coord, SErr_InvalidOperand);
        un.Typ := FContext.FTypes[typPointer];
        Exit;
      end;

      // todo 1: 还需要好好研究
      // @proc/@procvar 不管$T的状态，都是无类型指针
      // 根据$T+状态来调整指针的类型
      if FTypedAddress then
      begin
      //  Ref := un.Operand.GetReference;
        if Assigned(Ref) and (Ref.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
          Func := TFunctionDecl(Ref)
        else
          Func := nil;

        if Func <> nil then
        begin
          un.Typ := un.Operand.Typ;
          if Func.IsOverload then Include(un.Attr, eaDelayed);
        end
        {else if Assigned(Ref) and (Ref.NodeKind in [nkVariable, nkArgument, nkField]) then
        // @procvar 这种情形, 结果都是无类型指针
          un.Typ := Parser.FContext.FTypes[typPointer]}
        else if un.Operand.Typ.TypeCode = typUntype then
          un.Typ := FContext.FTypes[typPointer]
        else begin
          un.Operand.Typ.CreatePointerType(FPointerSize);
          un.Typ := un.Operand.Typ.PointerType;
        end;
      end
      else
        un.Typ := FContext.FTypes[typPointer];

      {
      // 根据$T+状态来调整指针的类型,但函数类型不受限制
      Func := un.Operand.GetFunctionSymbol;
      if Func <> nil then
      begin
        un.Typ := un.Operand.Typ;
        if Func.IsOverload then Include(un.Attr, eaDelayed);
      end
      else if un.HasMemory then
      begin
        if un.Operand.Typ.TypeCode = typProcedural then
          un.Typ := un.Operand.Typ
        else if Parser.FTypedAddress then
        begin
          un.Operand.Typ.CreatePointerType(Parser.FCntxInfo.PointerSize);
          un.Typ := un.Operand.Typ.PointerType;
        end
        else
          un.Typ := Parser.FContext.FTypes[typPointer];
      end
      else
        Parser.ParseError(un.Coord, SErr_InvalidOperand);
}
    end;
  end;

  procedure CheckInherited(E: TExpr);
  begin
    // todo: no impl
    E.Typ := FContext.FAnyType;
  end;

  procedure CheckIntConst(E: TConstExpr);
  var
    I64: Int64;
    I32: Integer;
  begin
    Assert(E.Value.VT in [vtInt, vtInt64]);

    I64 := ValToInt64(E.Value);
    if (Int64Rec(I64).Hi = 0) and (I64 <= $7fffffff) then
    begin
      I32 := I64;
      if I32 >= 0 then
      begin
        if I32 < 128 then
          E.Typ := FContext.FTypes[typShortint]
        else if I32 < 256 then
          E.Typ := FContext.FTypes[typByte]
        else if I32 < 32768 then
          E.Typ := FContext.FTypes[typSmallint]
        else if I32 < 65536 then
          E.Typ := FContext.FTypes[typWord]
        else
          E.Typ := FContext.FTypes[typLongint];
      end
      else
      begin
        if I32 >= -128 then
          E.Typ := FContext.FTypes[typShortint]
        else if I32 >= -32768 then
          E.Typ := FContext.FTypes[typSmallint]
        else
          E.Typ := FContext.FTypes[typLongint];
      end;
    end
    else if I64 <= $ffffffff then
      E.Typ := FContext.FTypes[typLongWord]
    else if Int64Rec(I64).Hi and $80000000 = 0 then
      E.Typ := FContext.FTypes[typInt64]
    else
      E.Typ := FContext.FTypes[typUInt64];
    Include(E.Attr, eaConst);
  end;

  function AdjustExpr(Expr: TExpr): TExpr; forward;

  procedure CheckList(un: TUnaryExpr);
  var
    E: ^TExpr;
    Temp: TExpr;
  begin
    E := @un.Operand;
    while E^ <> nil do
    begin
      Temp := AdjustExpr(E^);
      if Temp <> nil then
        E^ := Temp;
      E := @(E^.Next);
    end;
  end;

  function AdjustExpr(Expr: TExpr): TExpr;
  var
    un: TUnaryExpr;
    bin: TBinaryExpr;
    OldErr: Integer;
  begin
    Result := Expr;
    if Expr = nil then Exit;
    // 对已经标记为无效的也检查

    OldErr := FErrorCount;
    un := nil;
    bin := nil;

    if Expr.ClassType = TUnaryExpr then
    begin
      un := TUnaryExpr(Expr);
      if (un.OpCode = opLIST) then
      begin // 对LIST单独处理
        CheckList(un);
        Expr := un;
        Expr.Typ := FContext.FUntype;
      end
      else
        AdjustExpr(un.Operand);
    end
    else if Expr.ClassType = TBinaryExpr then
    begin
      bin := TBinaryExpr(Expr);
      AdjustExpr(bin.Left);
      if bin.OpCode <> opMEMBER then
        AdjustExpr(bin.Right);
    end;

    if OldErr <> FErrorCount then
    begin
      Include(Expr.Attr, eaInvalid);
      Exit;
    end;

    case Expr.OpCode of
      opNE, opEQ, opLT, opLE, opGT, opGE: CheckRelOp(bin);

      opIN: CheckInOp(bin);
      opIS: CheckIsOp(bin);
      opAS: CheckAsOp(bin);

      opFDIV: CheckDivOp(bin);

      opIDIV, opMOD: CheckDivModOp(bin);

      opADD, opSUB, opMUL: CheckAddSubMulOp(bin);

      opOR, opXOR, opAND: CheckBitwiseOp(bin);

      opSHL, opSHR: CheckShiftOp(bin);

      opNOT: CheckNotOp(un);

      opNEG, opPOS: CheckNegOp(un);

      opCALL: Result := CheckCall(bin);

      opMEMBER: Result := CheckSubSymbol(bin);

      opSYMBOL: Result := CheckSymbol(TSymbolExpr(Expr));

      opRANGE: CheckRange(bin);

      opINDEX: Result := CheckIndex(bin);

      opINHERITED: CheckInherited(bin);

      opSET: ResolveSetType(un);

      opLIST: begin end;

      opADDR: CheckAddrOp(un);

      opINST: Result := CheckInst(un);

      opNIL:
        begin
          Expr.Typ := FContext.FTypes[typPointer];
          Include(Expr.Attr, eaConst);
        end;

      opINTCONST: CheckIntConst(TConstExpr(Expr));

      opBOOLCONST:
        begin
          Expr.Typ := FContext.FTypes[typBoolean];
          Include(Expr.Attr, eaConst);
        end;
      opREALCONST:
        begin
          Expr.Typ := FContext.FRealType;
          Include(Expr.Attr, eaConst);
        end;
      opSTRCONST:
        begin
          Expr.Typ := FContext.FStringType;
          Include(Expr.Attr, eaConst);
        end;
      opCHARCONST:
        begin
          Expr.Typ := FContext.FCharType;
          Include(Expr.Attr, eaConst);
        end;
    else
      Expr.Typ := FContext.FIntegerType;
      ParseError(Expr.Coord, 'Invalid opcode');
    end;

    Expr.Typ := Expr.Typ.OriginalType;
  end;

var
  OldErr: Integer;
  E: TExpr;
begin
  OldErr := FErrorCount;
  E := AdjustExpr(Expr);
  if E <> nil then Expr := E;
  Result := (OldErr = FErrorCount) and not (eaInvalid in Expr.Attr);
  if not Result then Include(Expr.Attr, eaInvalid);
//  if FErrorCount > 0 then Expr.Typ := FContext.FAnytype;
end;

