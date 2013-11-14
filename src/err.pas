unit err;
{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}
// ¥ÌŒÛ–≈œ¢
interface

const
  SErr_InternalError = 'Internal error: %s';
  SErr_SystemUnitUsesOthers = 'System unit not allow uses other units';
  SErr_CircularUnitReference = 'In unit ''%s'', circular unit reference to ''%s''';
  SErr_SystemUnitNameMismatch = 'Name of system unit mismatch';
  SErr_SymbolHasParent = 'Symbol `%s` has parent';
  SErr_IncompatibleTypes = 'Incompatible types';
  SErr_AssignIncomp = 'Assignment incompatibility';
  SErr_ArgNotMatched = 'Arguments not matched';
  SErr_ArgNotMatched2 = '%s: Arguments not matched';
  SErr_InvalidArgument = 'Invalid argument in routine %s';
  SErr_InvalidBuiltinFunc = 'Invalid builtin-routine in constant expression';
  SErr_VarArgMustIdentical = 'Types of actual and formal var/out parameters must be identical';
  SErr_OutsideOfLoop = 'Break or continue outside of loop';
  SErr_InvalidOperand = 'Operator not applicable to this operand type';
  SErr_InvalidCast = 'Invalid type cast';
  SErr_InvalidConstOp = 'Invalid operator in constant expression';
  SErr_UndeclaredIdent = 'Undeclared identifier: %s';
  SErr_UnresolvedIdent = 'Unresolved identifier: %s';
  SErr_LabelExpected = 'Label expected';
  SErr_SubrangeOutOfBound = 'Low bound exceeds high bound';
  SErr_UngetTokenError = 'Internal error: Cannot unget more tokens, history buffer is full';
  SErr_VarRequired = 'Variable required';
  SErr_FieldRequired = 'Field required';
  SErr_StaticRequired = 'Static member required';
  SErr_SymbolNotClassOrStatic = 'Symbol %s not class or static';
  SErr_InitAbsoluteVar = 'Absolute var can not init';
  SErr_AbsoluteVarList = 'Absolute var can only be associated to one variable';
  SErr_ArrayRequired = 'Array type required';
  SErr_NoDefaultProp = 'Class or interface has not default property';
  SErr_ClassRequired = 'Class type required';
  SErr_SymbolNotType = 'Symbol %s is not type';
  SErr_RedeclaredIdent = 'Redeclared identifier: %s';
  SErr_ShortStrSize = 'Short string may have at most 255 characters';
  SErr_ExpectOrdinal = 'Ordinal type expected';
  SErr_ExpectBoolExpr = 'Boolean expression required';
  SErr_ExpectProtectOrPrivate = '`protected` or `private` expected';
  SErr_ExpectMethodOrProperty = '`function`, `procedure` or `property` expected';
  SErr_ExpectProcOrFunc = '`function` or `procedure` expected';
  SErr_ExpectMethod = 'Method expected';
  SErr_ExpectStructType = 'Type class, record, interface, object expected';
  SErr_ExpectIntfType = 'Interface type expected';
  SErr_ExpectClassType = 'Class typ expected';
  SErr_ExpectExpression = 'Expression expected';
  SErr_IntfMethodNotImpl = 'Interface method %s not implemented';
  SErr_ImplMethodDiffers = 'Implemented method %s is differs from interface''s';
  SErr_MethodNotImpl = 'Method no implemented code: %s';
  SErr_FuncNotImpl = 'Function no implemented code: %s';
  SErr_InvalidBaseObject = 'Invalid base object';
  SErr_InvalidBaseClass = 'Invalid base class';
  SErr_InvalidBaseIntf = 'Invalid base interface';
  SErr_InvalidIdent = 'Expect symbol, but name scope or unit name `%s` found';
  SErr_InvalidOpenArrayEl = 'Bad argument type in variable type array constructor';
  SErr_OrdinalRequired = 'Ordinal type required';
  SErr_SymbolNotAccess = 'Symbol %s can not be asscess';
  SErr_BaseClassSealed = 'Base class is sealed, can not be inherited';
  SErr_AccessorMethodArgsNotMatched = 'Accessor method arguments not matched';
  SErr_IntfNotAllowCtorDtor = 'Ctor/Dtor not allowed in interface';
  SWarn_CombiningSignedUnsigned = 'Combining signed and unsigned types - widened both operands';

implementation

end.
