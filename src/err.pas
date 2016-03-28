unit err;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}
// ¥ÌŒÛ–≈œ¢
interface

const
  SErr_InternalError = '(1001)Internal error: %s';
  SErr_SystemUnitUsesOthers = '(1002)System unit not allow uses other units';
  SErr_CircularUnitReference = '(1003)In unit ''%s'', circular unit reference to ''%s''';
  SErr_SystemUnitNameMismatch = '(1004)Name of system unit mismatch';
  SErr_SymbolHasParent = '(1005)Symbol `%s` has parent';
  SErr_IncompatibleTypes = '(1006)Incompatible types';
  SErr_IncompatibleTypes2 = '(1007)Incompatible types: ''%s'' and ''%s''';
  SErr_NotAssign = '(1008)Left side can not assign to';
  SErr_ExprNoValue = '(1009)Expression not return a value';
  SErr_AssignIncomp = '(1010)Assignment incompatibility';
  SErr_ArgNotMatched = '(1011)Arguments not matched';
  SErr_ArgNotMatched2 = '(1012)%s: Arguments not matched';
  SErr_InvalidArgument = '(1013)Invalid argument in routine %s';
  SErr_InvalidBuiltinFunc = '(1014)Invalid builtin-routine in constant expression';
  SErr_VarArgMustIdentical = '(1015)Types of actual and formal var/out parameters must be identical';
  SErr_OutsideOfLoop = '(1016)Break or continue outside of loop';
  SErr_UndeclaredIdent = '(1020)Undeclared identifier: %s';
  SErr_UnresolvedIdent = '(1021)Unresolved identifier: %s';
  SErr_LabelExpected = '(1022)Label expected';
  SErr_RedeclaredIdent = '(1034)Redeclared identifier: %s';
  SErr_ShortStrSize = '(1035)Short string may have at most 255 characters';
  SErr_ExpectIdentifier = '(1046)Identifier expected';
  SErr_ImplMethodDiffers = '(1048)Implemented method %s is differs from interface''s';
  SErr_MethodNotImpl = '(1049)Method no implemented code: %s';
  SErr_FuncNotImpl = '(1050)Function no implemented code: %s';
  SErr_InvalidIdent = '(1054)Expect symbol, but name scope or unit name `%s` found';
  SErr_InvalidOpenArrayEl = '(1055)Bad argument type in variable type array constructor';
  SErr_SymbolNotAccess = '(1057)Symbol %s can not be access';
  SErr_NumberOfElementsMismatch = '(1061)Number of elements differs from declaration';
// type

// expr
  SErr_InvalidOperand = '(1217)Operator not applicable to this operand type';
  SErr_InvalidCast = '(1218)Invalid type cast';
  SErr_InvalidConstOp = '(1219)Invalid operator in constant expression';
  SErr_SubrangeOutOfBound = '(1223)Low bound exceeds high bound';
  SErr_VarRequired = '(1224)Variable required';
  SErr_FieldRequired = '(1225)Field required';
  SErr_StaticRequired = '(1226)Static member required';
  SErr_SymbolNotClassOrStatic = '(1227)Symbol %s not class or static';
  SErr_ArrayRequired = '(1230)Array type required';
  SErr_NoDefaultProp = '(1231)Class or interface has not default property';
  SErr_ClassRequired = '(1232)Class type required';
  SErr_SymbolNotType = '(1233)Symbol %s is not a type';
  SErr_ExpectOrdinal = '(1234)Ordinal type expected';
  SErr_ExpectStructType = '(1235)Type class, record, interface, object expected';
  SErr_ExpectBoolExpr = '(1236)Boolean expression required';
  SErr_ExpectExpression = '(1237)Expression expected';
  SErr_OrdinalRequired = '(1238)Ordinal type required';
  SErr_InheritedExpectId = '(1239)Inherited keyword expect an identifier';
  SErr_InheritedNotAllow = '(1240)Inherited not allowed in here';
  SErr_OpNotAllow = '(1241)Operator not allowed in here';
// const/var
  SErr_ExpectConstExpr = '(1301)Expect constant expression';
  SErr_InvalidConstExpr = '(1302)Invalid constant expression';
  SErr_ConstantOutOfRange = '(1303)Constant expression violates subrange bounds';
  SErr_InitAbsoluteVar = '(1328)Absolute var can not init';
  SErr_AbsoluteVarList = '(1329)Absolute var can only be associated to one variable';
  SErr_MultiVariablesInit = '(1362)Cannot initialize multiple variables';
  SErr_ThreadVarInit = '(1363)Cannot initialize thread local variables';
// class/interface/object
  SErr_ClassNotComplete = '(1401)Class %s not completely defined';
  SErr_BaseClassSealed = '(1402)Base class is sealed, can not be inherited';
  SErr_AccessorMethodArgsNotMatched = '(1403)Accessor method arguments not matched';
  SErr_InvalidBaseObject = '(1404)Invalid base object';
  SErr_InvalidBaseClass = '(1405)Invalid base class';
  SErr_InvalidBaseIntf = '(1406)Invalid base interface';
  SErr_ExpectIntfType = '(1407)Interface type expected';
  SErr_ExpectClassType = '(1408)Class typ expected';
  SErr_IntfMethodNotImpl = '(1409)Interface method %s not implemented';
  SErr_NoVirtualMethod = '(1410)No virtual method %s in base class';
  SErr_OverridedDiffers = '(1411)Overrided method %s differs from base declaration';
  SErr_DefaultPropDuplicated = '(1412)Only one default property can inside class,object,record,interface declaraction';
  SErr_ExpectProtectOrPrivate = '(1413)`protected` or `private` expected';
  SErr_ExpectMethodOrProperty = '(1414)`function`, `procedure` or `property` expected';
  SErr_ExpectProcOrFunc = '(1415)`function` or `procedure` expected';
  SErr_ExpectMethod = '(1416)Method expected';
  SErr_MessageMethodArg = '(1417)Message method can only take a var parameter';
  SErr_MessageMethodDirective = '(1418)Invalid message method directive';
// statement
  SErr_CaseLabelDuplicated = '(1501)Case label duplicated';

  SWarn_CombiningSignedUnsigned = '(5001)Combining signed and unsigned types - widened both operands';
  SWarn_SymbolThroughInstance = '(5002)Symbol %s are not suitable to access through the instance';

implementation

end.
