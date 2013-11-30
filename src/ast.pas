unit ast;

{$ifdef FPC}
{$mode delphi}{$H+}
{$endif}

interface
uses Classes, SysUtils, hashtable;

const
  ROOT_VMT_OFFSET = 8; // 这个值和TObject的设计有关, 目前TObject有8个虚函数
type
  TMemberVisibility = (
    visDefault, visStrictPrivate, visStrictProtected,
    visPrivate, visProtected, visPublic, visPublished, visAutomated
  );
  TMemberVisibilities = set of TMemberVisibility;

  TMemberHint = (hDeprecated, hLibrary, hPlatform, hExperimental, hUnimplemented);
  TMemberHints = set of TMemberHint;

  TCallingConvention = (ccDefault, ccRegister, ccPascal, ccCDecl, ccStdCall, ccSafeCall);

  TModule = class;
  TCompoundStmt = class;
  TType = class;
  TVariable = class;
  TConstant = class;
  TFunctionDecl = class;
  TFunction = class;
  TStmtLabel = class;
  TSymbolTable = class;

  TAstNodeKind = (
    nkSymbol,
    nkExpr,
    nkType,
    nkNameScope,
    nkModule,
    nkVariable,
    nkConstant,
    nkField,
    nkProperty,
    nkIntfProperty,
    nkMethod,
    nkMethodResolution,
    nkArgument,
    nkEnumElement,
    nkFunc,           // 一般的procedure / function
    nkExternalFunc,   // 外部的
    nkBuiltinFunc,
    nkAccessor,
    nkLabel,
    nkStmt
  );

  TAstNodeKinds = set of TAstNodeKind;

  EASTError = class(Exception);

  TAstNodeCoord = record
    FileName: string;
    Row, Col: Integer;
  end;

  TAstNode = class
  private
    FNodeKind: TAstNodeKind;
  public
    Coord: TAstNodeCoord;
    Next: TAstNode;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property NodeKind: TAstNodeKind read FNodeKind;
  end;

  TAstNodeClass = class of TAstNode;

  TSymbolAttribute = (
    saUsed,          // 该符号已经使用
    saReserved1,     // (已经无用，可改名使用)临时指示这个符号不需要加到符号表
    saInternal,      // 该符号不是在interface段声明
    saForward,       // 临时指示该符号是前置声明
    saStatic,        // 该符号为static成员,它必须是TMethod,TField,TProperty
    saClass,         // 该符号为class成员,它必须是TMethod.
    saPrimitive,     // 该符号是编译定义的
    saTemp           // 该符号只是临时生成, 比如@IntData, 则表达式类型是TPointerType,RefType指向Integer,这个TPointer是临时创建的
  );
  TSymbolAttributes = set of TSymbolAttribute;

  // symbol
  TSymbol = class(TAstNode)
  private
    FName: string;
    FParent: TSymbol;
    FHints: TMemberHints;
    function GetUnitName: string;
    function GetModule: TModule;
    function GetFullName: string;
    function GetSymName: string;
 //   FUnitName: string;
  protected
    procedure AddSymbol(Sym: TSymbol); virtual;
  public
    Attr: TSymbolAttributes;
    Visibility: TMemberVisibility;
    property Name: string read FName write FName;
    property UnitName: string read GetUnitName;
    property Module: TModule read GetModule;
    // 所属的符号,可能为nil,比如运算过程中产生的类型
    property Parent: TSymbol read FParent write FParent;
    property Hints: TMemberHints read FHints write FHints;
    property FullName: string read GetFullName;
    property SymName: string read GetSymName;

    procedure Add(Sym: TSymbol); virtual;
  end;

  TSymbolClass = class of TSymbol;


  TValueType = (vtEmpty, vtInt, vtInt64, vtReal, vtCurr, vtSet, vtBool, vtStr,
                vtWStr, vtAChr, vtWChr, vtPtr, vtSymbol, vtArray, vtRecord,
                vtAddrOfSymbol);

  TSetValue = class
  public
    Bits: array[0..31] of Byte;
    BitStart: Byte; // 0..255
    BitCount: Byte; // 1..256
 //   procedure TestBit(Index: Integer);

    constructor Create;
    procedure Assign(Source: TSetValue);
    // 本集合是否包含R集合
    function Include(R: TSetValue): Boolean;
    // 取某位值
    function TestBits(Index: Byte): Boolean;
    // 该集合最小字节数
    function MinSize: Integer;
    // 设置/清除某位
    procedure SetBits(Index: Byte; Value: Boolean);
    // Update bits info, BitStart, BitCount
    procedure Update;
    // 比较
    function Equal(R: TSetValue): Boolean;
    // union
    class function Add(L, R: TSetValue): TSetValue;
    // difference
    class function Sub(L, R: TSetValue): TSetValue;
    // intersection
    class function Mul(L, R: TSetValue): TSetValue;
  end;

  TArrayValue = class;
  TRecordValue = class;

  PValueRec = ^TValueRec;
  // const value
  TValueRec = record
    VT: TValueType;
    Res1: Word;
    Res2: Word;
    Res3: Word;
    case TValueType of
      vtInt: (VInt: Integer);
      vtInt64: (VInt64: Int64);
      vtReal: (VReal: Double);
      vtCurr: (VCurr: Currency);
      vtSet: (VSet: Pointer);
      vtBool: (VBool: Boolean);
      vtStr: (VStr: Pointer);
      vtWStr: (VWStr: Pointer);
      vtAChr: (VAChr: AnsiChar);
      vtWChr: (VWChr: Integer);
      vtPtr: (VPtr: Pointer);
//      vtType: (VType: TType);
//      vtModule: (VModule: TModule);
//      vtNameScope: (VModulePart: TNameScope);
      vtSymbol: (VSymbol: TSymbol);
      vtAddrOfSymbol: (VAddr: Pointer);
      vtArray: (VArray: TArrayValue);
      vtRecord: (VRecord: TRecordValue);
  end;

  TArrayValueRange = record
    Start, Stop: Integer;
  end;

  TArrayByteValues = array[0..High(Integer)-1] of Byte;
  PArrayByteValues = ^TArrayByteValues;

  TArrayValue = class
  public
    DimCount: Integer;
    Ranges: array of TArrayValueRange;
    // 总数：每个Ranges的实际大小相乘
    // 取值: 比如数组定义为array[1..4, 0..2, 2..5], 取V[3,1,2],
    // 实际索引位置在 (3-1)*4 + 1*3 + (2-2);
    //
    Items: Pointer;
    VT: TValueType;

    destructor Destroy; override;
    procedure Clear;
  end;

  TRecordValue = class
  public
    Items: array of TValueRec;
    destructor Destroy; override;
    procedure Clear;
  end;

  TFileTimeStamp = record
    Date: LongWord;
    Time: LongWord;
  end;

  TModuleKind = (mkUnit, mkProgram, mkPackage, mkDLL);
  TModuleState = (msNone, msLoading, msIntfCompiling, msImplCompiling, msSaved, msGenOk);

  // unit class
  TModule = class(TSymbol)
  private
    FType: TType;
  protected
    procedure AddSymbol(Sym: TSymbol); override;
  public
    PointerSize: Byte;
    Kind: TModuleKind;
    State: TModuleState;
    Symbols: TSymbolTable;
    InternalSymbols: TSymbolTable;
    LoadedUnits: TSymbolTable;
    TimeStamp: TFileTimeStamp;
  //  Hash: array[0..31] of Byte;

    InitializeFunc, FinalizeFunc: TFunction;
//    InitializeCode, FinalizeCode: TCompoundStmt;
    FileName: string;  // the IN filename, only written when not empty.
    Names: array of string;

    Codes: string;
    constructor Create; override;
    destructor Destroy; override;

    // 查找符号
    function FindSymbol(const S: string): TSymbol;
    // 取得伪类, 用于表达式分析
    function GetType: TType;

    procedure SetNameScope(const Scopes: array of string; Count: Integer);
    procedure Add(Sym: TSymbol); override;
  end;

  {
    unit freecode;
    unit freecode.db;
    unit freecode.db.help;
    unit freecode.script;
    如果freecode中有符号db,那么
    uses freecode,freecode.db;
    或uses freecode.db, freecode;
    则freecode.db覆盖freecode中的db符号
  }
  TNameScope = class(TSymbol)
  private
    FType: TType;
    FSubNames: TStringList;  // TNameScope or TModule
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindSymbol(const S: string): TSymbol;
    procedure Add(const S: string; Sym: TSymbol); reintroduce;
    function GetType: TType;
  end;
{
  TProgram = class(TModule)
  public
    StartStmt: TCompoundStmt;
    constructor Create; override;
  end;}

  TPackage = class(TSymbol)
  public
    Modules: TList;
  end;

  // typInt8..typText : 固有类型
  TTypeCode = (
    typUnknown, typUntype,
    typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
    typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
    typBoolean, typByteBool, typWordBool, typLongBool,
    typAnsiChar, typWideChar,
    typPointer, typPAnsiChar, typPWideChar,
    typAnsiString, typWideString, typUnicodeString, typShortString,
    typVariant, typOleVariant,
    typFile, typText,
    typProcedural,
    typRecord, typObject, typClass, typInterface, typDispInterface, typClassRef,
    typEnum, typSet, typSubrange, typArray, typDynamicArray,
    // used for unit scope resolve
    typSymbol,
    typAlias, typClonedType, typOpenArray
  );

  TTypeCodes = set of TTypeCode;

//  TTypeAttribute = (taAutoInit, taTypeInfo);
//  TTypeAttributes = set of TTypeAttribute;

  TType = class(TSymbol)
  private
    FSize: Cardinal;
    FPointerType: TType;
    FTypeCode: TTypeCode;
  protected
    function GetAlignSize: Byte; virtual;
  public
    // 如果是匿名类型, 名称可能为空
    // Name: string

    constructor Create; override;
    destructor Destroy; override;
    property TypeCode: TTypeCode read FTypeCode;
    property Size: Cardinal read FSize write FSize;
    property AlignSize: Byte read GetAlignSize; // 1..16
    property PointerType: TType read FPointerType;

    // 创建本类型的指针类型 Size: 4, 8
    procedure CreatePointerType(ASize: Cardinal);

    // 取最初的类型
    // 比如 type MyInt = Integer; MyInt2 = MyInt
    // MyInt2的最初类型就是Integer
    function OriginalType: TType;

    function NormalType: TType;

    // 是否是有序类型 int, enum, subrange, char, boolean
    function IsOrdinal: Boolean;

    // 是否是数字类型 single, double, currency, comp
    function IsReal: Boolean;

    // 是否是整型 typShortint..typUInt64
    function IsInteger: Boolean;

    function IsSignedInt: Boolean;

    function IsSigned: Boolean;

    // 是否是布尔 typBoolean..typLongBool
    function IsBoolean: Boolean;

    // 是否是typProcedural
    function IsProcedural: Boolean;
    function IsMethodPointer: Boolean;

    // 是否是字符串,或兼容于字符串
    // typAnsiString..typShortString, 一维Char/WideChar数组
    function IsStringCompatible: Boolean;

    // 是否是Variant,或兼容于Variant
    function IsVariantCompatible: Boolean;

    // 是否是指针(typPointer, typPAnsiChar, typPWideChar)
    // 其它如typClass之类,虽与指针相容,但不算
    function IsPointer: Boolean;

    function IsUntypePointer: Boolean;

    // 是否同一类型
    function Equals(typ: TType): Boolean; {$ifdef FPC}reintroduce;{$endif}
  end;

  TTypeClass = class of TType;

  // 编译器固有类型
  TPrimitiveType = class(TType)
  protected
    function GetAlignSize: Byte; override;
  public
    FAlign: Byte;
    constructor Create(ACode: TTypeCode); reintroduce; virtual;
  end;

  TUnresolvedType = class(TType)
  public
    constructor Create; override;
  end;

  // MyInt = Integer
  TAliasType = class(TType)
  private
    FRefType: TType;
  public
    constructor Create; override;
    procedure Update;
    property RefType: TType read FRefType write FRefType;
  end;

  // MyInt = type Integer;
  TClonedAliasType = class(TAliasType)
//  private
//    FRefType: TType;
  public
    constructor Create; override;
//    procedure Update;
//    property RefType: TType read FRefType write FRefType;
  end;

  TEnumType = class;

  TEnumValue = class(TSymbol)
  public
    EnumType: TEnumType;
    Value: Integer;

    constructor Create; override;
  end;

  TSubrangeType = class;

  TEnumType = class(TType)
  private
    FHighValue, FLowValue: Integer;
    procedure UpdateRange;
    procedure CalcSize(MinSize: Integer = 1);
  public
    // 最小值和最大值组成的Range类型
    SubrangeType: TSubrangeType;
    Values: TList;  // TEnumValue
    MinEnumSize: Byte; // 1..4

    constructor Create; override;
    destructor Destroy; override;
    procedure Update;

    property LowValue: Integer read FLowValue;
    property HighValue: Integer read FHighValue;
  end;

  TSetType = class;

  TSubrangeType = class(TType)
  private
    FBaseType: TType;
    procedure SetBaseType(const Value: TType);
  public
    // 对应的set类型
    SetType: TSetType;
    // TEnumType or ordinal primitive type
    RangeBegin, RangeEnd: Int64;
    constructor Create; override;
  //  destructor Destroy; override;
    property BaseType: TType read FBaseType write SetBaseType;
    // 本类型的界限是否在typ之内
    function SubSetOf(typ: TSubrangeType): Boolean;
  end;

  TSetType = class(TType)
  protected
    function GetAlignSize: Byte; override;
    procedure UpdateSize;
  public
    RangeType: TSubrangeType;           // 如果=nil, 是通用的set, 用于表示空集合
    constructor Create; override;
    function IsCommonSetType: Boolean;
    procedure Update;
  end;

  TPointerType = class(TType)
  public
    RefType: TType;

    constructor Create; override;
    function IsUntype: Boolean;
  end;

  // type tmystr = ansistring(936)
  TAnsiStringType = class(TType)
  public
    CodePage: Word; // 0 表示系统默认的代码页
    constructor Create; override;
  end;

  TStructAttribute = (staNeedInit, staNeedFree);
  TStructAttributes = set of TStructAttribute;

  TArrayDimension = record
    RefType: TType; // TSubrangeType;
//    LowBound, HighBound: Integer;
    IsPacked: Boolean;
  end;

  TArrayType = class(TType)
  private
    function GetDimensionCount: Integer;
  protected
    function GetAlignSize: Byte; override;
  public
    ElementType: TType;    // 必须不为nil
    Range: TSubrangeType;
    IsPacked: Boolean;
    ArrayAttr: TStructAttributes;

    constructor Create; override;
    procedure Update;
    // A one-dimensional, packed, static array of Char values is called a packed string
    function IsPackedString: Boolean;
    property DimensionCount: Integer read GetDimensionCount;
  end;

  TDynamicArrayType = class(TType)
  protected
    function GetAlignSize: Byte; override;
  public
    ElementType: TType;
    IsPacked: Boolean;
    constructor Create; override;
  end;

  TOpenArrayType = class(TType)
  protected
    function GetAlignSize: Byte; override;
  public
    ElementType: TType; // 元素类型.(一般不会为nil)
    constructor Create; override;
  end;

  TStructuredType = class(TType)
  end;

                    // Record.的一部分
  TFieldAttribute = (faRecVar);
  TFieldAttributes = set of TFieldAttribute;

  TField = class(TSymbol)
  public
    FieldType: TType;
    Offset: Cardinal;
  //  Index: Integer;
    FieldAttr: TFieldAttributes;

//    IsStatic: Boolean;
    constructor Create; override;
  end;

  TPropertyAttribute = (paNoDefault, paNoStored, paDefaultProp);
  TPropertyAttributes = set of TPropertyAttribute;

  TMultiAccessor = class(TSymbol)
  public
    Fields: array of TSymbol; // first is TVariable or TField, others are TField.
    FieldCount: Integer;
    constructor Create; override;
    procedure Add(Field: TSymbol); reintroduce;
    function Last: TField;
    function First: TSymbol;
  end;

  TProperty = class(TSymbol)
  public
    PropType: TType;
    Getter, Setter, Stored: TSymbol; // TMultiAccessor, TField or TMethod;
    Index: Integer;
    Args: TList;
    DefaultValue: TValueRec;
    PropAttr: TPropertyAttributes;
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateArgs;
    function CountOfArgs: Integer;
    function HasIndexSpec: Boolean;

    function GetterType: TType;
    function SetterType: TType;
  end;

  TMethod = class;

  {TRecordField = class(TField)
  public
    IsVariantPart: Boolean; // 是否是变体的一部分

    constructor Create; override;
  end;   }

  TRecordVariant = class;

  TRecordBody = class
  private
    procedure UpdateAlign;
    function RecordSize: Int64; 
  public
    Members: TList;         // TRecordField
    // Selector,Variants, Next可为nil
    Selector: TField; // 记录变体部分, Selector.Name 可以是 ''
    Variants: TRecordVariant;  // Variants.Next连接下一组变体
    MaxAlignSize: Byte;        // 成员中最大对齐

    constructor Create;
    destructor Destroy; override;
    procedure Update(GlobalAlignSize: Byte; Offset: Cardinal);
    procedure AddFields(Symbols: TSymbolTable);
  end;

  TRecordBodyClass = class of TRecordBody;

  TRecordVariant = class(TRecordBody)
  public
    Next: TRecordVariant; // Next连接下一组变体
    Offset: Cardinal;        // 此变体的偏移
    destructor Destroy; override;
  end;

  TRecordType = class(TStructuredType)
  protected
    procedure UpdateAlign;
    function GetAlignSize: Byte; override;
    procedure AddSymbol(Sym: TSymbol); override;
  public
//    Members: TList;
    Symbols: TSymbolTable;
    Body: TRecordBody;
    // 定义record 时的默认AlignSize
    GlobalAlignSize: Byte; // 1..16
    RecordAttr: TStructAttributes;

    constructor Create; override;
    destructor Destroy; override;
    function FindSymbol(const S: string): TSymbol;
    procedure Add(Sym: TSymbol); override;
    procedure Update;
  end;

  TClassRefType = class;

  TClassAttribute = (
    caSealed,
    caAbstract,
    caRtti        // true表示声明该类时$M+,false表示$M-,$M的状态将影响子类
  );
  TClassAttributes = set of TClassAttribute;

  TInterfaceType = class;
  TMethodResolution = class;

  TClassType = class(TStructuredType)
  private
    FClassRef: TClassRefType;
    FAllSymbols: TSymbolTable;
    function GetAllSymbols: TSymbolTable;
  protected
    procedure AddSymbol(Sym: TSymbol); override;
  public
    Symbols: TSymbolTable; // TType, TField, TConstant, TMethod, TFunction
    Base: TClassType;      // if nil, this is TObject
    Interfaces: TList;
    DefaultProp: TProperty;
    ObjectSize: Int64;    // Instance Size
    VmtEntries: Integer;  // vmt条目个数
    Vmt: array of TMethod; // vmt
    MR: TMethodResolution;
    GlobalAlignSize: Byte; // 1,2,4,8,16 声明该类时的Align
    ClassAttr: TClassAttributes;

    // todo 1: VMT的建立
    // todo 1: 查找最大对齐字节

    constructor Create; override;
    destructor Destroy; override;

    // 本类和基类所有符号的表，只在ParseFunction中使用,
    // 这大概能够提高查找速度，避免向上循环查找基类
    property AllSymbols: TSymbolTable read GetAllSymbols;
    // 该类或基类是否在$M+状态编译的
    function RttiEnabled: Boolean;

    // todo 1: 需要考虑实现接口的问题。
    // 在当前类及其类中查找
    function FindSymbol(const S: string): TSymbol;
    // 仅在当前类中查找
    function FindCurSymbol(const S: string): TSymbol;
    // 仅在基类中查找
    function FindBaseSymbol(const S: string): TSymbol;
    // 该类从ABase继承？
    function IsInheritedFrom(ABase: TClassType): Boolean;
    // 是否直接或间接实现该接口
    function IsImplemented(AIntf: TInterfaceType): Boolean;

    // 取该类的类引用(class reference)
    function GetClassRef: TClassRefType;
    procedure CreateInterfaces;
    // 计算字段偏移，VMT位置等
    procedure Update(PtrSize: Integer);
    procedure Add(Sym: TSymbol); override;
  end;

  TClassRefType = class(TType)
  public
    RefType: TClassType;
    constructor Create; override;
    function IsInheritedFrom(ClassRef: TClassRefType): Boolean;
    //procedure Update(const CntxInfo: TContextInfo); override;
  end;

  TIntfPropertyAttribute = (ipaNoUsed, ipaDefaultProp, ipaReadOnly,
                            ipaWriteOnly, ipaHasDispID);
  TIntfPropertyAttributes = set of TIntfPropertyAttribute;

  TIntfProperty = class(TSymbol)
  public
    PropType: TType;
    Getter, Setter: TMethod;
    Args: TList;
    DispID: Integer;
    PropAttr: TIntfPropertyAttributes;

    constructor Create; override;
    destructor Destroy; override;
    procedure CreateArgs;
    function CountOfArgs: Integer;
  end;

  // dispinterface的基类其实就是DispInterface
  TInterfaceType = class(TType)
  private
    FAllSymbols: TSymbolTable;
    function GetAllSymbols: TSymbolTable;
  protected
    procedure AddSymbol(Sym: TSymbol); override;
  public
    Guid: TGuid;
    Symbols: TSymbolTable;  // TMethod, TInterfaceProperty
    Base: TInterfaceType;   // if nil, this is IUnknown
    CountOfVmtEntry: Word;  // vmt entry number
    DefaultProp: TIntfProperty;
    IsDisp: Boolean;

    constructor Create; override;
    destructor Destroy; override;
    property AllSymbols: TSymbolTable read GetAllSymbols;
    // 在当前类及其类中查找
    function FindSymbol(const S: string): TSymbol;
    // 仅在当前类中查找
    function FindCurSymbol(const S: string): TSymbol;
    // 仅在基类中查找
    function FindBaseSymbol(const S: string): TSymbol;
    // 该类从ABase继承？
    function IsInheritedFrom(ABase: TInterfaceType): Boolean;

    procedure Add(Sym: TSymbol); override;
  end;

  TObjectAttribute = (
    oaHasVirtual,     // 该类具有虚方法
    oaHasVmt,         // 该类具有vmt(可能从父类开始)
    oaBeginVmt        // vmt从该类开始
  );
  TObjectAttributes = set of TObjectAttribute;

  TObjectType = class(TType)
  private
    FAllSymbols: TSymbolTable;
    function GetAllSymbols: TSymbolTable;
  protected
    procedure AddSymbol(Sym: TSymbol); override;
  public
    Symbols: TSymbolTable;       // TMethod, TProperty, TField, TFunction
    Base: TObjectType;
    VmtOffset: Cardinal;   // vmt表的偏移
    VmtEntries: Integer;   // vmt条目个数
    GlobalAlignSize: Byte; // 1,2,4,8,16 声明该类时的Align
    ObjectAttr: TObjectAttributes;

    constructor Create; override;
    destructor Destroy; override;
    // 当前及基类所有符号
    property AllSymbols: TSymbolTable read GetAllSymbols;
    // 在当前类及其类中查找
    function FindSymbol(const S: string): TSymbol;
    // 仅在当前类中查找
    function FindCurSymbol(const S: string): TSymbol;
    // 仅在基类中查找
    function FindBaseSymbol(const S: string): TSymbol;
    // 该类从ABase继承？
    function IsInheritedFrom(ABase: TObjectType): Boolean;

    // 计算字段偏移，VMT位置等
    procedure Update(PtrSize: Integer);
    // 添加符号
    procedure Add(Sym: TSymbol); override;
  end;

  TFileType = class(TType)
  public
    ElementType: TType;  // ElementType可能为nil

    constructor Create; override;
    function IsUntype: Boolean;
  end;

  // 带后缀的ShortString, 如Shortstring[20]
  // 不带后缀的用TPrimitiveType表示
  TShortStringType = class(TType)
  protected
    function GetAlignSize: Byte; override;
  public
    CharCount: Byte;
    constructor Create; override;
    procedure Update;
  end;

  TSymbolType = class(TType)
  public
    Reference: TSymbol; // TNameScope or TModule
    constructor Create; override;
  end;

  TArgumentModifier = (argDefault, argConst, argVar, argOut);
  TArgState = (
    asInit,       // 已经初始化
    asNestRef,    // 被嵌套函数引用
    asByRef,      // 传引用
    asStructRef,     // 传结构引用
    asStructValue,   // 传结构值
    asNeedAddRef,
    asNeedFree
  );
  TArgStates = set of TArgState;

  TArgumentKind = (akNormal, akArrayOfType, akArrayOfConst, akUntype);
  // array of const: ArgType = TOpenArrayType and TOpenArrayType.ElementType = typUntype
  // array of Type: ArgType = TOpenArrayType and TOpenArrayType.ElementType <> typUntype
  // var obj: ArgType = typUntype and Modifier = argVar
  // out obj: ArgType = typUntype and Modifier = argOut
  // const obj: ArgType = typUntype and Modifier = argConst
  TArgument = class(TSymbol)
  public
    ArgType: TType;        // 无类型则指向typUntype
    ArgKind: TArgumentKind;
    States: TArgStates;
    Modifier: TArgumentModifier;
    Level: Byte;  // 嵌套层数, 0为顶层, 不用保存
    Index: Word;  // 顺序, 不用保存
    DefaultValue: TValueRec;

    constructor Create; override;
    destructor Destroy; override;

    function IsReadOnly: Boolean;
//    property ArgKind: TArgumentKind read GetArgKind;
  end;

  TMethodKind = (
    mkNormal, mkConstructor, mkDestructor,
    mkObjCtor, mkObjDtor,  // for object
    mkRecCtor, mkRecDtor   // for record
  );

  TObjectKind = (okClass, okObject, okRecord);

  TProceduralType = class(TType)
  public
    Args: TList; // TArgument
    ReturnType: TType;
    CallConvention: TCallingConvention;
    IsMethodPointer: Boolean;
    MethodKind: TMethodKind;
    ObjectKind: TObjectKind;

    constructor Create; override;
    destructor Destroy; override;
    procedure CreateArgs;
    function CountOfArgs: Integer;
    function MinOfArgs: Integer;
  end;

  TCompilerDirective = (
    cdBoolEval,                   // $B $BOOLEVAL
    cdIOChecks,                   // $I $IOCHECKS
    cdOverflowChecks,             // $Q $OVERFLOWCHECKS
    cdRangeChecks,                // $R $RANGECHECKS
    cdSafeDivide,                 // $U $SAFEDIVIDE
    cdTypeInfo,                   // $M $TypeInfo
    cdTypedAddress,               // $T $TYPEDADDRESS
    cdWriteableConst,             // $J $WRITEABLECONST
    cdAssertions,                 // $C $ASSERTIONS
    cdOptimization,               // $O $OPTIMIZATION
    cdStackFrames,                // $W $STACKFRAMES
    cdDebugInfo,                  // $D $DEBUGINFO
    csExtendedSyntax,             // $X $ExtendedSyntax 
    csLongStrings,                // $H $LONGSTRINGS
    csOpenStrings,                // $P $OPENSTRINGS
    csVarStringChecks,            // $V $VARSTRINGCHECKS
    csRealCompatibility,          // $REALCOMPATIBILITY

    cdAlign,                      // $A $ALIGN
    cdAppType,                    // $APPTYPE
    cdMinEnumSize,                // $Z $MINENUMSIZE

    cdDefine,
    cdUndef,
    cdIf,
    cdIfOpt,
    cdIfDef,
    cdIfNDef,
    cdElse,
    cdElseIf,
    cdEndIf,
    cdIfEnd,

    cdNoDefine,     // for c++
    cdHPPEmit,      // for c++
    cdExternalSym,  // for c++

    cdResource,                   // $R $RESOURCE
    cdInclude                     // $I $INCLUDE
  );

  TCodeSwitches = set of cdBoolEval..cdSafeDivide;

  TExprOpCode = (opNONE,
    // binary opNE..opSHR 其位置不可变动
    opNE, opEQ, opLT, opLE, opGT, opGE, opIN, opIS, opAS,
    opADD, opSUB, opOR, opXOR,
    opMUL, opFDIV, opIDIV, opMOD, opAND, opSHL, opSHR,
    opMEMBER, // eg. SomeObj.A, the A is member of SomeObj
    opCAST,   // Integer(a);
    opCALL,   // function call
    opRANGE,  // .. subrange
    opINDEX,  // [] array index
    // unary
    opNOT, opNEG, opPOS,
    opINHERITED, // inherited keyword
    opSET,  // [] set / open array constructor
    opLIST, // express list, seperated by comma
    opADDR, // @ get address
    opINST, // ^ dereference 
    // other
    opNIL,
    opBOOLCONST, opINTCONST, opREALCONST, opSTRCONST, opCHARCONST, opSYMBOL
  );

  TExprAttribute = (
    eaVerified,      // expr node is verified
    eaInvalid,       // expr node is not valid
    eaDelayed,         // 表达式需要延时处理,如@proc或proc当成参数传递时
    eaCall,            // 表达式需要扩展为call
    eaArgList,         // 表达式是参数列表
    eaOverloadRestrict, // 只在函数所在单元中查找Overload .
    eaArrayProp,
    eaConst,          // expr node is const
    eaVarCast         // expr is variable cast
  //  eaWithSym        // symbol is member of 'with' statement
  );
  TExprAttributes = set of TExprAttribute;

  TExpr = class(TAstNode)
  private
    FTyp: TType;
    FParent: TExpr;
    FOpCode: TExprOpCode;
  public
    Attr: TExprAttributes;
    Switches: TCodeSwitches;
    property Typ: TType read FTyp write FTyp;
    property OpCode: TExprOpCode read FOpCode write FOpCode;
//    property Attr: TExprAttributes read FAttr write FAttr;
    property Parent: TExpr read FParent write FParent;

    constructor Create; override;

    procedure Reset; virtual;

    // 取这个表达式指向的符号
    // 表达式可能是个opSYMBOL或opMEMBER
    function GetReference: TSymbol;
    function GetFunctionSymbol: TFunctionDecl;
    function GetVariableSymbol: TVariable;
    procedure SetReference(Ref: TSymbol);
    // 是否指向类型符号
    function IsTypeSymbol: Boolean;
 {   // 是字段
    function IsField: Boolean;
    // 是否指向变量符号(有可能是opSYMBOL,opMEMBER,opINST,opINDEX,opCAST)
    function IsVariable: Boolean;
    // 是常量(true constant)
    function IsConstant: Boolean;}
    // 是类型常量
    function IsTypedConstant: Boolean;

    // 是否指向内存地址(有可能是opSYMBOL,opMEMBER,opINST,opINDEX,opCAST)
    // v1
    // guid.d2 guid^.d2
    // ptr^
    // (ptr + 1)^
    // pbyte(p^)
    // pchar(ptr)[1]
    function HasMemory: Boolean;

    // 是函数(包括方法、外部函数)符号
    function IsFunction: Boolean;
    // 是类型符号
    function IsClassType: Boolean;

    function IsNilConst: Boolean;
  end;

  TUnaryExpr = class(TExpr)
  private
    fOperand: TExpr;
    procedure SetOperand(const Value: TExpr);
  public
    // opNOT, opNEG, opPOS, opINST, opINHERITED
    // opSET: Operand 可能为nil
    // opLIST: Operand 可能为nil
    property Operand: TExpr read fOperand write SetOperand;

    procedure Reset; override;
  end;

  TBinaryExpr = class(TExpr)
  private
    FLeft: TExpr;
    FRight: TExpr;
    procedure SetLeft(const Value: TExpr);
    procedure SetRight(const Value: TExpr);
  public
    // opCALL: Left是函数表达式, Right是调用参数列表(opLIST), 如果无参则为nil
    //    opCALL的Left可以是TSymbolExpr或其它类型为TProceduralType的表达式
    // opMEMBER: Left父表达式, Right是子表达式
    // opINDEX: Left是数组表达式, Right是索引值(ExprList)
    property Left: TExpr read FLeft write SetLeft;
    property Right: TExpr read FRight write SetRight;

    procedure Reset; override;
  end;

  TSymbolExpr = class(TExpr)
  public
    Name: string;
    Reference: TSymbol;

    procedure Reset; override;
  end;

{  TNilExpr = class(TExpr)
  public
    //construct
  end;}

  TConstExpr = class(TExpr)
  public
    //Value: Variant;
    Value: TValueRec; // Enum和Char以vtInt形式存贮

    destructor Destroy; override;
    procedure Reset; override;
  end;

  TStrConstExpr = class(TExpr)
  public
    RawValue: string; // 原始未解析的值, 只限于字符串

    procedure Reset; override;
  end;

  TVarAttribute = (
  //  vaUsed,        // 有使用
    vaReserved1,   //
    vaReserved2,     //
    vaReadOnly,    // 只读
    vaLocal,       // 局部变量
    vaHidden,      // 不在代码声明
    vaTls,         // threadvar
    vaResult,      // 用来保存结果的变量
    vaSelf         // Self指针
  );
  TVarAttributes = set of TVarAttribute;

  TVarState = (
    vsInit,             // 有赋值
    vsNestRef,          // 被嵌套函数访问
    vsResultAddr,       // 是Result变量，但这个变量是个地址（由调用函数传入）
    vsNeedInit,
    vsNeedFree,
    vsTemp              // 临时变量, 不需要内存
  );
  TVarStates = set of TVarState;

  TVariable = class(TSymbol)
  public
    VarType: TType;
    Value: TValueRec;
    AbsVar: TSymbol; // V2: Byte absolute V1;
    VarAttr: TVarAttributes;
    States: TVarStates;
    Level: Byte;  // 嵌套层数, 0为顶层, 不用保存
    Index: Word;  // 顺序, 不用保存

    constructor Create; override;
    destructor Destroy; override;
  end;

  TConstant = class(TSymbol)
  public
    ConstType: TType;
    Value: TValueRec;
    IsResStr: Boolean;

//    IsTyped: Boolean;
    constructor Create; override;
    destructor Destroy; override;
  end;

//  TProperty = class(TVariable)
//  public
//    Args: TList;  // index
//    DefaultValue: Variant;
//    Getter, Setter, StoreFunc, Implements: TSymbol;
//    IsDefault, IsNoDefault: Boolean;
//  end;

  TFunctionModifier = (fmVirtual, fmDynamic, fmAbstract, fmOverride,
                       fmOverload, fmMessage, fmReintroduce, fmStatic,
                       fmInline, fmAssembler, fmVarargs, fmLocal, fmDispID,
                       fmExport, fmNear, fmFar, fmExternal, fmForward,

                       fmNoReturn     // 相当于llvm的noreturn
                       );
  TFunctionModifiers = set of TFunctionModifier;

{  TFuncAttr = (faVirtual, faDynamic, faAbstract, faOverride,
               faOverload, faMessage, faReintroduce, faStatic,
               faInline, faAssembler, faVarargs, faLocal, faDispID,
               faExport, faForward );
  TFuncAttrs = set of TFuncAttr;}

  // 函数声明
  TFunctionDecl = class(TSymbol)
  private
    FProcType: TProceduralType;
    function GetProceduralType: TProceduralType;
  protected
    procedure CreateProceduralType; virtual;
  public
    ID: Integer;     // 编号,用于区别重载方法
    Args: TList;     // TArgument
    ReturnType: TType;   // nil 表示procedure
    NextOverload: TFunctionDecl;  // overload chains
  //  Overloads: TList;
    Modifiers: TFunctionModifiers;
    CallConvention: TCallingConvention;
    Level: Byte;  // 嵌套层数 顶层为0

    destructor Destroy; override;
    procedure CreateArgs;
    function CountOfArgs: Integer;
    function MinOfArgs: Integer;
    function IsOverload: Boolean;
    procedure AddOverload(Func: TFunctionDecl);
    property ProceduralType: TProceduralType read GetProceduralType;
  end;

  TExternalFunction = class(TFunctionDecl)
  public
    FileName: string;
    RoutineName: string;   // name
    RoutineNo: Integer;    // index
    constructor Create; override;
  end;

  TFuncAttr = (
    faNeedFP,            // 需要建立堆栈桢结构
    faNeedFPArg          // 需要传入上一级堆栈桢
  );
  TFuncAttrs = set of TFuncAttr;

  TFunction = class(TFunctionDecl)
  protected
    procedure AddSymbol(Sym: TSymbol); override;
  public
    LocalSymbols: TSymbolTable;
    StartStmt: TCompoundStmt;
  //  Level: Byte;  // 嵌套层数 顶层为0
    FuncAttr: TFuncAttrs;

    constructor Create; override;
    destructor Destroy; override;
    procedure Add(Sym: TSymbol); override;
  end;

  TMethod = class(TFunction)
  protected
    procedure CreateProceduralType; override;
  public
    MethodKind: TMethodKind;
    ObjectKind: TObjectKind;
    VTIndex: Smallint;    // 在VMT中的索引, 可负
    MsgNo: Word;      // Message id.
    DispID: Integer;  // disp id.
    constructor Create; override;
    function IsClassOrStatic: Boolean;
  end;

  TMethodResolution = class(TSymbol)
  public
    ImplementingMethod: TMethod;
    InterfaceMethod: TMethod;
    constructor Create; override;
  end;

  TBuiltinFunctionKind = (
      bfAbs, bfAddr, bfAssigned, bfBreak, bfChr, bfContinue, bfCopy, bfDec,
      bfDispose, bfExclude, bfExit, bfFinalize, bfFreeMem, bfGetMem,
      bfHi, bfHigh, bfInc, bfInclude, bfInitialize, bfLength, bfLo,
      bfLow, bfNew, bfOdd, bfOrd, bfPred, bfPtr, bfRound, bfSucc, bfSetLength,
      bfSizeOf, bfSwap, bfTrunc, bfTypeInfo
      //bfFillChar, bfMove, bfVal
  );

  TBuiltinFunction = class(TSymbol)
  public
    Kind: TBuiltinFunctionKind;
    constructor Create; override;
  end;

  TStmtKind = (
    skEmptyStmt,
    skCompoundStmt,
    skIfStmt,
    skCaseStmt,
    skForStmt,
    skWhileStmt,
    skRepeatStmt,
    skTryStmt,
//    skWithStmt,
    skRaiseStmt,
    skAssignmentStmt,
    skCallStmt,
    skGotoStmt,

    skCtorInit,
    skCtorAfter,
    skDtorBefore,
    skDtorInit,
    skLocalInit,
    skLocalUninit,
    skHandleExcept,
    skStrCat,
    skStrCast,
    skVarOp,
    skVarCast,
    skIntfCast,
    skOAInit
  );

  // statement classes
  TStatement = class(TAstNode)
  private
    FStmtKind: TStmtKind;
  public
    Parent: TStatement;
    property StmtKind: TStmtKind read FStmtKind;
//    function IsParent
  end;

  TStatementClass = class of TStatement;

  TEmptyStmt = class(TStatement)
  public
    constructor Create; override;
  end;

  TCompoundStmt = class(TStatement)
  public
    Statements: TList;
    IncludeBegin: Boolean;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TCallStmt = class(TStatement)
  public
    CallExpr: TExpr;
    constructor Create; override;
  end;

  TStmtLabel = class(TSymbol)
  public
    // property Name 已经在TSymbol声明
    Stmt: TStatement;
    constructor Create; override;
  end;

//  TLabelStmt = class(TStatement)
//  public
//    Value: string;
//    Stmt: TStatement;
//    constructor Create; override;
//  end;

  TAssignmentStmt = class(TStatement)
  public
    Left, Right: TExpr;
    constructor Create; override;
  end;

  TIfStmt = class(TStatement)
  public
    Value: TExpr;
    TrueStmt: TStatement;
    FalseStmt: TStatement; // 可能为nil
    constructor Create; override;
  end;

  TForStmt = class(TStatement)
  public
    Value: TSymbol; // TVariable, TArgument
    Down: Boolean;
    Start: TExpr;
    Stop: TExpr;
    Stmt: TStatement;
    constructor Create; override;
  end;

  TGotoStmt = class(TStatement)
  public
    StmtLabel: TStmtLabel;  // 可能为nil
    constructor Create; override;
  end;

  TWhileStmt = class(TStatement)
  public
    Condition: TExpr;
    Stmt: TStatement;
    constructor Create; override;
  end;

  TCaseRange = record
    Start, Stop: Int64;
  end;

  TCaseSelector = class
  private
    FCount: Integer;
  public
    Values: array of TCaseRange;
    Stmt: TStatement;
    destructor Destroy; override;
    procedure AddRange(Start, Stop: Int64);
    function Contains(Start, Stop: Int64): Boolean;
    procedure Clear;
    property Count: Integer read FCount;
  end;

  TCaseStmt = class(TStatement)
  private
    FCount: Integer;
  public
    //SelectorList: TList;
    Expr: TExpr;
    Selectors: array of TCaseSelector;
    Default: TCompoundStmt;
    constructor Create; override;
    destructor Destroy; override;
    property Count: Integer read FCount;
    procedure AddSelector(Selector: TCaseSelector);
    procedure Clear;
  end;

  TRepeatStmt = class(TStatement)
  public
    Condition: TExpr;
    Stmt: TCompoundStmt;
    constructor Create; override;
  end;

{  TWithStmt = class(TStatement)
  public
  end;}

  TExceptHandler = class
  public
    ExceptVar: TVariable;
    Stmt: TStatement;
  end;

  TExceptBlock = class
  private
    FCount: Integer;  
  public
    ExceptHandlers: array of TExceptHandler;
    Default: TCompoundStmt;

    destructor Destroy; override;
    procedure AddExceptHandler(Block: TExceptHandler);
    procedure Clear;
    property Count: Integer read FCount;
  end;

  TTryStmt = class(TStatement)
  public
    Stmt: TCompoundStmt;
    FinallyStmt: TCompoundStmt;
    ExceptBlock: TExceptBlock;
    constructor Create; override;
    destructor Destroy; override;
    function IsFinallyOrExcept(S: TStatement): Boolean;
  end;

  TRaiseStmt = class(TStatement)
  public
    Expr: TExpr; // a call expr or variable expr;
    constructor Create; override;
  end;

  TSymbolPosition = Pointer;

  // Symbol table
  TSymbolTable = class(TCustomHashTable)
  private
    FOwner: TSymbol;
    FAutoAddToOwner: Boolean;
    function GetSymbol(Index: Integer): TSymbol;
  public
    constructor Create(AOwner: TSymbol);
    function GetStart(const S: string): TSymbolPosition;
    function GetNext(var Pos: TSymbolPosition): TSymbol;

    // 查找符号
    function Find(const S: string): TSymbol; overload;
    // 查找所属单元为M为符号
    function Find(M: TModule; const S: string): TSymbol; overload;
    // 添加符号, 如果名称已经存在, 则返回true, 否则返回false
    function Add(Sym: TSymbol): Boolean;
    // 清空
    procedure Clear(FreeSym: Boolean = False);
    // 按索引取
    property Item[Index: Integer]: TSymbol read GetSymbol; default;
    // 所属
    property Owner: TSymbol read FOwner;
    // 自动添加到Owner
    property AutoAddToOwner: Boolean read FAutoAddToOwner write FAutoAddToOwner; 
  end;

  TDirectiveIdent = (
    idNone,
    idDeprecated, idLibrary, idPlatform, idExperimental, idUnimplemented,
    idRegister, idPascal, idCDecl, idStdCall, idSafeCall,
    idVirtual, idDynamic, idAbstract, idOverride,
    idOverload, idMessage, idReintroduce, idStatic,
    idInline, idAssembler, idVarargs, idLocal, idDispMId,
    idExport, idNear, idFar, idExternal, idForward
  );

  TDirectiveIdents = set of TDirectiveIdent;

function FindDirective(const S: string): TDirectiveIdent;

const
  TypeNames: array[TTypeCode] of string = (
    // typUnknown, typUntype,
    'unknown', 'untype',
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
    'file', 'Text',
    // typProcedural,
    'procedural',
    // typRecord, typObject, typClass, typInterface, typDispInterface, typClassRef,
    'record', 'object', 'class', 'interface', 'dispinterface', 'classref',
    // typEnum, typSet, typSubrange, typArray, typDynamicArray
    'enum', 'set', 'subrange', 'array', 'dynamicarray',
    'unitscope', 'alias', 'clonetype', 'openarray'
  );

  AutoInitTypes = [typAnsiString..typShortString,
      typVariant, typOleVariant, typInterface, typDispInterface,
      typDynamicArray];
  AutoFreeTypes = [
      typAnsiString..typUnicodeString,
      typVariant, typOleVariant, typInterface, typDispInterface,
      typDynamicArray
    ];

procedure ValClear(var V: TValueRec);
procedure ValInit(out V: TValueRec);

function ValFromBool(B: Boolean): TValueRec; overload;
function ValFromChar(C: AnsiChar): TValueRec; overload;
function ValFromWChar(C: WideChar): TValueRec; overload;
function ValFromInt(I: Integer): TValueRec; overload;
function ValFromInt(I: Int64): TValueRec; overload;
function ValFromCurr(I: Currency): TValueRec; overload;
function ValFromReal(R: Real): TValueRec; overload;
function ValFromStr(const S: string): TValueRec; overload;
function ValFromWStr(const S: WideString): TValueRec; overload;
function ValFromSet(S: TSetValue): TValueRec; overload;
function ValFromPtr(P: Pointer): TValueRec; overload;
function ValFromElement(Sym: TSymbol): TValueRec; overload;

procedure ValFromBool(var V: TValueRec; B: Boolean); overload;
procedure ValFromChar(var V: TValueRec; C: AnsiChar); overload;
procedure ValFromWChar(var V: TValueRec; C: WideChar); overload;
procedure ValFromInt(var V: TValueRec; I: Integer); overload;
procedure ValFromInt(var V: TValueRec; I: Int64); overload;
procedure ValFromCurr(var V: TValueRec; I: Currency); overload;
procedure ValFromReal(var V: TValueRec; R: Real); overload;
procedure ValFromStr(var V: TValueRec; const S: string); overload;
procedure ValFromWStr(var V: TValueRec; const S: WideString); overload;
procedure ValFromSet(var V: TValueRec; S: TSetValue); overload;
procedure ValFromPtr(var V: TValueRec; P: Pointer); overload;
procedure ValFromElement(var V: TValueRec; Sym: TSymbol); overload;

function ValIsClear(const V: TValueRec): Boolean;
procedure ValCopy(var Dest: TValueRec; const Source: TValueRec); overload;
function ValCopy(const Source: TValueRec): TValueRec; overload;
function ValCast(const V: TValueRec; vt: TValueType): TValueRec;
function ValToInt(const V: TValueRec): Integer;
function ValToInt64(const V: TValueRec): Int64;
function ValToBool(const V: TValueRec): Boolean;
function ValToReal(const V: TValueRec): Real;
function ValToCurr(const V: TValueRec): Currency;
function ValToStr(const V: TValueRec): string;
function ValToWStr(const V: TValueRec): WideString;
function ValToSet(const V: TValueRec): TSetValue;
function ValToPtr(const V: TValueRec): Pointer;
//function ValToChar(const V: TValueRec): Char;
// not implemented
function ValToVar(const V: TValueRec): Variant;

function ValOp(const v1, v2: TValueRec; op: TExprOpCode): TValueRec;
function ValCmp(const v1, v2: TValueRec; op: TExprOpCode): TValueRec;
function ValIn(const v1, v2: TValueRec): TValueRec;
function ValNot(const v1: TValueRec): TValueRec;
function ValNeg(const v1: TValueRec): TValueRec;

function ValAbs(const V: TValueRec): TValueRec;
function ValHi(const V: TValueRec): TValueRec;
function ValLo(const V: TValueRec): TValueRec;
function ValChr(const V: TValueRec): TValueRec;
function ValOdd(const V: TValueRec): TValueRec;
function ValOrd(const V: TValueRec): TValueRec;
function ValPred(const V: TValueRec): TValueRec;
function ValRound(const V: TValueRec): TValueRec;
function ValSucc(const V: TValueRec): TValueRec;
function ValSwap(const V: TValueRec): TValueRec;
function ValTrunc(const V: TValueRec): TValueRec;

// type helper
// 类型是否和Variant兼容
function TypIsVariantCompatible(typ: TType): Boolean;
// 类型是否能和Variant 运算
function TypIsVariantArithCompatible(typ: TType): Boolean;
// 类型是否能和String运算
function TypIsStringArithCompatible(typ: TType): Boolean;

implementation
uses Math, err;

//var
//  typSizes: array[TTypeCode] of Integer = (
//    // typUnknown, typUntype,
//    0, 0,
//    // typShortint, typByte, typSmallint, typWord, typLongint, typLongWord, typInt64, typUInt64,
//    1, 1, 2, 2, 4, 4, 8, 8,
//    // typComp, typReal48, typSingle, typDouble, typExtended, typCurrency,
//    8, 6, 4, 8, 10, 8,
//    // typBoolean, typByteBool, typWordBool, typLongBool,
//    1, 1, 2, 4,
//    // typAnsiChar, typWideChar,
//    1, 2,
//    // typPointer, typPAnsiChar, typPWideChar,
//    4, 4, 4,
//    // typAnsiString, typWideString, typUnicodeString, typShortString,
//    4, 4, 4, 255,
//    // typVariant, typOleVariant,
//    16, 16,
//    // typFile, typTextFile,
//    4, 4,
//    // typProcedure, typFunction,
//    4, 4,
//    // typRecord, typObject, typClass, typInteface, typDispInterface, typClassRef,
//    0, 0, 0, 4, 4, 4,
//    // typEnum, typSet, typSubrange, typArray, typDynamicArray
//    1, 0, 1, 0, 0
//  );

procedure ValCastErr;
begin
  raise Exception.Create('value cast error');
end;

procedure ValOpErr;
begin
  raise Exception.Create('Invalid operator and operand');
end;

procedure ValFuncErr;
begin
  raise Exception.Create('Invalid function on value');
end;

procedure ValClear(var V: TValueRec);
begin
  case V.VT of
    vtStr: String(V.VStr) := '';
    vtWStr: WideString(V.VWStr) := '';
    vtSet: TSetValue(V.VSet).Free;
    vtArray: TArrayValue(V.VSet).Free;
    vtRecord: TRecordValue(V.VSet).Free;
  end;
  V.VT := vtEmpty;
end;

procedure ValInit(out V: TValueRec);
begin
  V.VT := vtEmpty;
end;

function ValFromBool(B: Boolean): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtBool;
  Result.VBool := B;
end;

function ValFromChar(C: AnsiChar): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtAChr;
  Result.VAChr := C;
end;

function ValFromWChar(C: WideChar): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtWChr;
  Result.VWChr := Word(C);
end;

function ValFromInt(I: Integer): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtInt;
  Result.VInt := I;
end;

function ValFromInt(I: Int64): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtInt64;
  Result.VInt64 := I;
end;

function ValFromCurr(I: Currency): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtCurr;
  Result.VCurr := I;
end;

function ValFromReal(R: Real): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtReal;
  Result.VReal := R;
end;

function ValFromStr(const S: string): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtStr;
  Result.VStr := nil;
  String(Result.VStr) := S;
end;

function ValFromWStr(const S: WideString): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtWStr;
  Result.VWStr := nil;
  WideString(Result.VWStr) := S;
end;

function ValFromSet(S: TSetValue): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtSet;
  Result.VSet := S;
end;

function ValFromPtr(P: Pointer): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtPtr;
  Result.VPtr := P;
end;

function ValFromElement(Sym: TSymbol): TValueRec;
begin
  ValInit(Result);
  Result.VT := vtSymbol;
  Result.VSymbol := Sym;
end;

procedure ValFromBool(var V: TValueRec; B: Boolean);
begin
  ValClear(V);
  V.VT := vtBool;
  V.VBool := B;
end;

procedure ValFromChar(var V: TValueRec; C: AnsiChar);
begin
  ValClear(V);
  V.VT := vtAChr;
  V.VAChr := C;
end;

procedure ValFromWChar(var V: TValueRec; C: WideChar);
begin
  ValClear(V);
  V.VT := vtWChr;
  V.VWChr := Word(C);
end;

procedure ValFromInt(var V: TValueRec; I: Integer);
begin
  ValClear(V);
  V.VT := vtInt;
  V.VInt := I;
end;

procedure ValFromInt(var V: TValueRec; I: Int64);
begin
  ValClear(V);
  V.VT := vtInt64;
  V.VInt64 := I;
end;

procedure ValFromCurr(var V: TValueRec; I: Currency);
begin
  ValClear(V);
  V.VT := vtCurr;
  V.VCurr := I;
end;

procedure ValFromReal(var V: TValueRec; R: Real);
begin
  ValClear(V);
  V.VT := vtReal;
  V.VReal := R;
end;

procedure ValFromStr(var V: TValueRec; const S: string);
begin
  ValClear(V);
  V.VT := vtStr;
  V.VStr := nil;
  String(V.VStr) := S;
end;

procedure ValFromWStr(var V: TValueRec; const S: WideString);
begin
  ValClear(V);
  V.VT := vtWStr;
  V.VWStr := nil;
  WideString(V.VWStr) := S;
end;

procedure ValFromSet(var V: TValueRec; S: TSetValue);
begin
  ValClear(V);
  V.VT := vtSet;
  V.VSet := S;
end;

procedure ValFromPtr(var V: TValueRec; P: Pointer);
begin
  ValClear(V);
  V.VT := vtPtr;
  V.VPtr := P;
end;

procedure ValFromElement(var V: TValueRec; Sym: TSymbol);
begin
  ValClear(V);
  V.VT := vtSymbol;
  V.VSymbol := Sym;
end;

function ValIsClear(const V: TValueRec): Boolean;
begin
  Result := V.VT = vtEmpty;
end;

procedure ValCopy(var Dest: TValueRec; const Source: TValueRec);
begin
  ValClear(Dest);
  case Source.VT of
    vtStr: begin
      Dest.VT := Source.VT;
      Dest.VStr := nil;
      String(Dest.VStr) := String(Source.VStr);
    end;

    vtSet: begin
      Dest.VT := Source.VT;
      Dest.VSet := TSetValue.Create;
      TSetValue(Dest.VSet).Assign(TSetValue(Source.VSet));
    end;

    else
      Move(Source, Dest, SizeOf(Dest));
  end;
end;

function ValCopy(const Source: TValueRec): TValueRec;
begin
  ValInit(Result);
  ValCopy(Result, Source);
end;

function ValCast(const V: TValueRec; vt: TValueType): TValueRec;
begin
  case vt of
    vtInt:      Result := ValFromInt(ValToInt(V));
    vtInt64:    Result := ValFromInt(ValToInt64(V));
    vtReal:     Result := ValFromReal(ValToReal(V));
    vtCurr:     Result := ValFromCurr(ValToCurr(V));
    vtBool:     Result := ValFromBool(ValToBool(V));
    vtPtr:      Result := ValFromPtr(ValToPtr(V));
    else
  end;
end;

function ValToInt(const V: TValueRec): Integer;
begin
  Result := ValToInt64(V);
end;

function ValToInt64(const V: TValueRec): Int64;
begin
  case V.VT of
    vtInt: Result := V.VInt;
    vtInt64: Result := V.VInt64;
    vtBool: Result := Ord(V.VBool);
    vtAChr: Result := Ord(V.VAChr);
    vtWChr: Result := Word(V.VWChr);
  else
    ValCastErr;
    Result := 0;
  end;
end;

function ValToBool(const V: TValueRec): Boolean;
begin
  case V.VT of
    vtBool: Result := V.VBool;
    vtInt: Result := V.VInt <> 0;
    vtInt64: Result := V.VInt64 <> 0;
    vtReal: Result := V.VReal <> 0;
    vtCurr: Result := V.VCurr <> 0;
    else begin
      ValCastErr;
      Result := False;
    end;
  end;
end;

function ValToReal(const V: TValueRec): Real;
begin
  case V.VT of
    vtInt: Result := V.VInt;
    vtInt64: Result := V.VInt64;
    vtReal: Result := V.VReal;
    vtCurr: Result := V.VCurr;
    else begin
      ValCastErr;
      Result := 0;
    end;
  end;
end;

function ValToCurr(const V: TValueRec): Currency;
begin
  case V.VT of
    vtInt: Result := V.VInt;
    vtInt64: Result := V.VInt64;
    vtReal: Result := V.VReal;
    vtCurr: Result := V.VCurr;
    else begin
      ValCastErr;
      Result := 0;
    end;
  end;
end;

function ValToStr(const V: TValueRec): string;
begin
  case V.VT of
    vtStr: Result := String(V.VStr);
    vtWStr: Result := WideString(V.VWStr);
    vtAChr: Result := V.VAChr;
    vtWChr: Result := WideChar(V.VWChr);
    vtInt: Result := IntToStr(V.VInt);
    vtInt64: Result := IntToStr(V.VInt64);
    vtReal: Result := FloatToStr(V.VReal);
    vtCurr: Result := CurrToStr(V.VCurr);
    vtBool: Result := BoolToStr(V.VBool);
    vtPtr: Result := Format('$%p', [V.VPtr]);
    vtSymbol:
      if V.VSymbol <> nil then
        Result := V.VSymbol.Name else Result := '';
  else
    ValCastErr;
    Result := '';
  end;
end;

function ValToWStr(const V: TValueRec): WideString;
begin
  case V.VT of
    vtStr: Result := String(V.VStr);
    vtWStr: Result := WideString(V.VWStr);
    vtAChr: Result := V.VAChr;
    vtWChr: Result := WideChar(V.VWChr);
    vtInt: Result := IntToStr(V.VInt);
    vtInt64: Result := IntToStr(V.VInt64);
    vtReal: Result := FloatToStr(V.VReal);
    vtCurr: Result := CurrToStr(V.VCurr);
    vtBool: Result := BoolToStr(V.VBool);
    vtPtr: Result := Format('$%p', [V.VPtr]);
    vtSymbol:
      if V.VSymbol <> nil then
        Result := V.VSymbol.Name else Result := '';
  else
    ValCastErr;
    Result := '';
  end;
end;

function ValToSet(const V: TValueRec): TSetValue;
begin
  case V.VT of
    vtSet: Result := TSetValue(V.VSet);
  else
    ValCastErr;
    Result := nil;
  end;
end;

function ValToPtr(const V: TValueRec): Pointer;
begin
  case V.VT of
    vtInt: Result := Pointer(V.VInt);
    vtInt64: Result := Pointer(V.VInt64);
    vtStr: Result := V.VStr;
    vtPtr: Result := V.VPtr;
    vtSet: Result := TSetValue(V.VSet);
    vtSymbol: Result := V.VSymbol;
  else
    ValCastErr;
    Result := nil;
  end;
end;

//function ValToChar(const V: TValueRec): Char;
//begin
//  if V.VT = vtStr then
//  begin
//    if (V.VStr <> nil) and (Length(String(V.VStr)) > 0) then
//      Result := String(V.VStr)[1]
//    else
//      Result := #0;
//  end;
//end;

function ValToVar(const V: TValueRec): Variant;
begin
{$ifdef fpc}
  Result := Null;
{$endif}
end;

type
  TBaseType = (btErr, btEmp, btInt, btI64, btFlt, btCur, btSet, btStr, btBol);

function ValOp(const v1, v2: TValueRec; op: TExprOpCode): TValueRec;

  function IntOp(Left, Right: TValueRec; Op: TExprOpCode): TValueRec;
  var
    L, R: Integer;
  begin
    L := ValToInt(Left);
    R := ValToInt(Right);
    case Op of
      opADD: Result := ValFromInt(L + R);
      opSUB: Result := ValFromInt(L - R);
      opMUL: Result := ValFromInt(L * R);
      opIDIV: Result := ValFromInt(L div R);
      opMOD: Result := ValFromInt(L mod R);
      opSHL: Result := ValFromInt(L shl R);
      opSHR: Result := ValFromInt(L shr R);
      opAND: Result := ValFromInt(L and R);
      opOR: Result := ValFromInt(L or R);
      opXOR: Result := ValFromInt(L xor R);
    else
      ValOpErr;
    end;
  end;

  function I64Op(Left, Right: TValueRec; Op: TExprOpCode): TValueRec;
  var
    L, R: Int64;
  begin
    L := ValToInt(Left);
    R := ValToInt(Right);
    case Op of
      opADD: Result := ValFromInt(L + R);
      opSUB: Result := ValFromInt(L - R);
      opMUL: Result := ValFromInt(L * R);
      opIDIV: Result := ValFromInt(L div R);
      opMOD: Result := ValFromInt(L mod R);
      opSHL: Result := ValFromInt(L shl R);
      opSHR: Result := ValFromInt(L shr R);
      opAND: Result := ValFromInt(L and R);
      opOR: Result := ValFromInt(L or R);
      opXOR: Result := ValFromInt(L xor R);
      else
        ValOpErr;
    end;
  end;

  function FltOp(Left, Right: TValueRec; Op: TExprOpCode): TValueRec;
  var
    L, R: Real;
  begin
    L := ValToReal(Left);
    R := ValToReal(Right);
    case Op of
      opADD: Result := ValFromReal(L + R);
      opSUB: Result := ValFromReal(L - R);
      opMUL: Result := ValFromReal(L * R);
      opFDIV: Result := ValFromReal(L / R);
      else
        ValOpErr;
    end;
  end;

  function CurOp(Left, Right: TValueRec; OpCode: TExprOpCode): TValueRec;
  var
    L, R: Currency;
  begin
    L := ValToCurr(Left);
    R := ValToCurr(Right);
    case OpCode of
      opADD: Result := ValFromReal(L + R);
      opSUB: Result := ValFromReal(L - R);
      opMUL: Result := ValFromReal(L * R);
      opFDIV: Result := ValFromReal(L / R);
      else
        ValOpErr;
    end;
  end;

  {function _ValToStr(const V: TValueRec): string;
  begin
    if V.VT = vtInt then      // vtInt 视为char
      Result := Chr(V.VInt)
    else
      Result := ValToStr(V);
  end;}

  function StrOp(const Left, Right: TValueRec; Op: TExprOpCode): TValueRec;
  begin
    if Op = opADD then
    begin
      if (Left.VT in [vtWStr, vtWChr]) or (Right.VT in [vtWStr, vtWChr]) then
        Result := ValFromWStr(ValToWStr(Left) + ValToWStr(Right))
      else
        Result := ValFromStr(ValToStr(Left) + ValToStr(Right))
    end
    else
      ValOpErr;
  end;

  function BolOp(const Left, Right: TValueRec; OpCode: TExprOpCode): TValueRec;
  begin
    case OpCode of
      opAND:  Result := ValFromBool(ValToBool(Left) and ValToBool(Right));
      opOR :  Result := ValFromBool(ValToBool(Left) or ValToBool(Right));
      opXOR:  Result := ValFromBool(ValToBool(Left) xor ValToBool(Right));
    else
      ValOpErr;
    end;
  end;

  function SetOp(const Left, Right: TValueRec; OpCode: TExprOpCode): TValueRec;
  var
    L, R: TSetValue;
  begin
    L := ValToSet(Left);
    R := ValToSet(Right);
    case OpCode of
      opADD: Result := ValFromSet(TSetValue.Add(L, R));
      opSUB: Result := ValFromSet(TSetValue.Sub(L, R));
      opMUL: Result := ValFromSet(TSetValue.Mul(L, R));
    end;
  end;

const
  OpMap: array[vtEmpty..vtWChr, vtEmpty..vtWChr] of TBaseType =
  (
        // vtEmpty, vtInt, vtInt64, vtReal, vtCurr, vtSet, vtBool, vtStr, vtWStr, vtAChr, vtWChr
{vtEmpty} (btInt,   btInt, btI64,   btFlt,  btCur,  btSet, btBol,  btStr, btStr,  btStr,  btStr),
{vtInt}   (btInt,   btInt, btI64,   btFlt,  btCur,  btErr, btErr,  btStr, btErr,  btErr,  btErr),
{vtInt64} (btI64,   btI64, btI64,   btFlt,  btCur,  btErr, btErr,  btErr, btErr,  btErr,  btErr),
{vtReal}  (btFlt,   btFlt, btFlt,   btFlt,  btFlt,  btErr, btErr,  btErr, btErr,  btErr,  btErr),
{vtCurr}  (btCur,   btCur, btCur,   btFlt,  btCur,  btErr, btErr,  btErr, btErr,  btErr,  btErr),
{vtSet}   (btSet,   btErr, btErr,   btErr,  btErr,  btSet, btErr,  btErr, btErr,  btErr,  btErr),
{vtBool}  (btBol,   btErr, btErr,   btErr,  btErr,  btErr, btBol,  btErr, btErr,  btErr,  btErr),
{vtStr}   (btStr,   btStr, btErr,   btErr,  btErr,  btErr, btErr,  btStr, btStr,  btStr,  btStr),
{vtWStr}  (btStr,   btErr, btErr,   btErr,  btErr,  btErr, btErr,  btStr, btStr,  btStr,  btStr),
{vtAChr}  (btErr,   btErr, btErr,   btErr,  btErr,  btErr, btErr,  btStr, btStr,  btStr,  btStr),
{vtWChr}  (btErr,   btErr, btErr,   btErr,  btErr,  btErr, btErr,  btStr, btStr,  btStr,  btStr)
  );

  function SimpleOp(L, R: TValueRec; OpCode: TExprOpCode): TValueRec;
  begin
    case OpMap[v1.VT, v2.VT] of
      btInt:
        if Op = opFDIV then
          Result := FltOp(L, R, OpCode)
        else
          Result := IntOp(L, R, OpCode);
      btI64:
        if Op = opFDIV then
          Result := FltOp(L, R, OpCode)
        else
          Result := I64Op(L, R, OpCode);
      btFlt:
        if Op = opIDIV then
          Result := IntOp(L, R, OpCode)
        else
          Result := FltOp(L, R, OpCode);

      btCur: Result := CurOp(L, R, OpCode);
      btStr: Result := StrOp(L, R, OpCode);
      btBol: Result := BolOp(L, R, OpCode);
      btSet: Result := SetOp(L, R, OpCode);

    else
      ValOpErr;
    end;
  end;
begin
  if (v1.VT <= vtWChr) and (v2.VT <= vtWChr) then
    Result := SimpleOp(v1, v2, Op)
  else
    ValOpErr;
end;

function ValCmp(const v1, v2: TValueRec; op: TExprOpCode): TValueRec;
type
  TCmpResult = (crLess, crEqual, crGreater);

  function IntCmp(L, R: Integer): TCmpResult;
  begin
    if L > R then
      Result := crGreater
    else if L < R then
      Result := crLess
    else
      Result := crEqual;
  end;

  function I64Cmp(L, R: Int64): TCmpResult;
  begin
    if L > R then
      Result := crGreater
    else if L < R then
      Result := crLess
    else
      Result := crEqual;
  end;

  function FltCmp(L, R: Real): TCmpResult;
  begin
    if L > R then
      Result := crGreater
    else if L < R then
      Result := crLess
    else
      Result := crEqual;
  end;

  function CurCmp(L, R: Currency): TCmpResult;
  begin
    if L > R then
      Result := crGreater
    else if L < R then
      Result := crLess
    else
      Result := crEqual;
  end;

  function StrCmp(const L, R: TValueRec): TCmpResult;
  var
    S1, S2: string;
  begin
  // todo 1: 这里可能需要转成unicode
    S1 := ValToStr(L);
    S2 := ValToStr(R);
    Result := IntCmp(CompareStr(S1, S2), 0);
  end;

  function SetCmp(L, R: TSetValue; OpCode: TExprOpCode): Boolean;
  begin
    case OpCode of
      opLE: Result := R.Include(L);
      opGE: Result := L.Include(R);
      opEQ: Result := L.Equal(R);
      opNE: Result := not L.Equal(R);
    else
      ValOpErr;
      Result := False;
    end;
  end;
const
  OpMap: array[vtEmpty..vtWChr, vtEmpty..vtWChr] of TBaseType =
  (
        // vtEmpty, vtInt, vtInt64, vtReal, vtCurr, vtSet, vtBool, vtStr, vtWStr, vtAChr, vtWChr
{vtEmpty} (btInt,   btInt, btI64,   btFlt,  btCur,  btSet, btBol, btStr,  btStr,  btStr,  btStr),
{vtInt}   (btInt,   btInt, btI64,   btFlt,  btCur,  btErr, btErr, btErr,  btErr,  btErr,  btErr),
{vtInt64} (btI64,   btI64, btI64,   btFlt,  btCur,  btErr, btErr, btErr,  btErr,  btErr,  btErr),
{vtReal}  (btFlt,   btFlt, btFlt,   btFlt,  btFlt,  btErr, btErr, btErr,  btErr,  btErr,  btErr),
{vtCurr}  (btCur,   btCur, btCur,   btFlt,  btCur,  btErr, btErr, btErr,  btErr,  btErr,  btErr),
{vtSet}   (btSet,   btErr, btErr,   btErr,  btErr,  btSet, btErr, btErr,  btErr,  btErr,  btErr),
{vtBool}  (btBol,   btErr, btErr,   btErr,  btErr,  btErr, btBol, btErr,  btErr,  btErr,  btErr),
{vtStr}   (btStr,   btErr, btErr,   btErr,  btErr,  btErr, btErr, btStr,  btStr,  btStr,  btStr),
{vtWStr}  (btStr,   btErr, btErr,   btErr,  btErr,  btErr, btErr, btStr,  btStr,  btStr,  btStr),
{vtAChr}  (btStr,   btErr, btErr,   btErr,  btErr,  btErr, btErr, btStr,  btStr,  btStr,  btStr),
{vtWChr}  (btStr,   btErr, btErr,   btErr,  btErr,  btErr, btErr, btStr,  btStr,  btStr,  btStr)
  );
var
  CmpRet: TCmpResult;
  Ret: Boolean;
begin
  if (v1.VT <= vtWChr) and (v2.VT <= vtWChr) then
  begin
    case OpMap[v1.VT, v2.VT] of
      btInt: CmpRet := IntCmp(ValToInt(v1), ValToInt(v2));
      btI64: CmpRet := I64Cmp(ValToInt64(v1), ValToInt64(v2));
      btFlt: CmpRet := FltCmp(ValToReal(v1), ValToReal(v2));
      btCur: CmpRet := CurCmp(ValToCurr(v1), ValToCurr(v2));
      btBol: CmpRet := IntCmp(Integer(ValToBool(v1)), Integer(ValToBool(v2)));
      btStr: CmpRet := StrCmp(v1, v2);
      btSet: begin
        Result := ValFromBool(SetCmp(ValToSet(v1), ValToSet(v2), op));
        Exit;
      end;
    else
      ValOpErr;
      CmpRet := crLess;
    end;
    case op of
      opNE: Ret := CmpRet <> crEqual;
      opEQ: Ret := CmpRet = crEqual;
      opLT: Ret := CmpRet = crLess;
      opLE: Ret := CmpRet <= crEqual;
      opGT: Ret := CmpRet = crGreater;
      opGE: Ret := CmpRet >= crEqual;
      else begin
        ValOpErr;
        Ret := False;
      end;
    end;
    Result := ValFromBool(Ret);
  end
  else
    ValOpErr;
end;

function ValIn(const v1, v2: TValueRec): TValueRec;
begin
  Result := ValFromBool(ValToSet(v2).TestBits(ValToInt(v1)));
end;

function ValNot(const v1: TValueRec): TValueRec;
begin
  case v1.VT of
    vtInt: Result := ValFromInt(not v1.VInt);
    vtInt64: Result := ValFromInt(not v1.VInt64);
    vtBool: Result := ValFromBool(not v1.VBool);
  else
    ValOpErr;
  end;
end;

function ValNeg(const v1: TValueRec): TValueRec;
begin
  case v1.VT of
    vtInt:   Result := ValFromInt(-v1.VInt);
    vtInt64: Result := ValFromInt(-v1.VInt64);
    vtReal:  Result := ValFromReal(-v1.VReal);
    vtCurr:  Result := ValFromCurr(-v1.VCurr);
  else
    ValOpErr;
  end;
end;

function ValAbs(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Abs(V.VInt));
    vtInt64: Result := ValFromInt(Abs(V.VInt64));
    vtReal: Result := ValFromReal(Abs(V.VReal));
    vtCurr: Result := ValFromCurr(Abs(V.VCurr));
  else
    ValFuncErr;
  end;
end;

function ValHi(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Hi(V.VInt));
    vtInt64: Result := ValFromInt(Hi(V.VInt64));
  else
    ValFuncErr;
  end;
end;

function ValLo(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Lo(V.VInt));
    vtInt64: Result := ValFromInt(Lo(V.VInt64));
  else
    ValFuncErr;
  end;
end;

function ValChr(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromChar(Chr(V.VInt));
    vtInt64: Result := ValFromChar(Chr(V.VInt64));
  else
    ValFuncErr;
  end;
end;

function ValOdd(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromBool(Odd(V.VInt));
    vtInt64: Result := ValFromBool(Odd(V.VInt64));
  else
    ValFuncErr;
  end;
end;

function ValOrd(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(V.VInt);
    vtInt64: Result := ValFromInt(V.VInt64);
    vtAChr: Result := ValFromInt(Ord(V.VAChr));
    vtWChr: Result := ValFromInt(Word(V.VWChr));
  else
    ValFuncErr;
  end;
end;

function ValPred(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Pred(V.VInt));
    vtInt64: Result := ValFromInt(Pred(V.VInt64));
    vtAChr: Result := ValFromChar(Pred(V.VAChr));
    vtWChr: Result := ValFromWChar(WideChar(V.VWChr - 1));
  else
    ValFuncErr;
  end;
end;

function ValRound(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(V.VInt);
    vtInt64: Result := ValFromInt(V.VInt64);
    vtReal: Result := ValFromInt(Round(V.VReal));
    vtCurr: Result := ValFromInt(Round(V.VCurr));
  else
    ValFuncErr;
  end;
end;

function ValSucc(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Succ(V.VInt));
    vtInt64: Result := ValFromInt(Succ(V.VInt64));
    vtAChr: Result := ValFromChar(Succ(V.VAChr));
    vtWChr: Result := ValFromWChar(WideChar(V.VWChr + 1));
  else
    ValFuncErr;
  end;
end;

function ValSwap(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(Swap(V.VInt));
    vtInt64: Result := ValFromInt(Swap(V.VInt64));
  else
    ValFuncErr;
  end;
end;

function ValTrunc(const V: TValueRec): TValueRec;
begin
  case V.VT of
    vtInt: Result := ValFromInt(V.VInt);
    vtInt64: Result := ValFromInt(V.VInt64);
    vtReal: Result := ValFromInt(Trunc(V.VReal));
    vtCurr: Result := ValFromInt(Trunc(V.VCurr));
  else
    ValFuncErr;
  end;
end;

var
  _Directives: TStringList;

procedure InitDirectives;
const
  Idents: array[TDirectiveIdent] of string =
  (
    '',
    'deprecated', 'library', 'platform', 'experimental', 'unimplemented',
    'register', 'pascal', 'cdecl', 'stdcall', 'safecall',
    'virtual', 'dynamic', 'abstract', 'override',
    'overload', 'message', 'reintroduce', 'static',
    'inline', 'assembler', 'varargs', 'local', 'dispid',
    'export', 'near', 'far', 'external', 'forward'
  );
var
  I: TDirectiveIdent;
begin
  _Directives := TStringList.Create;
  _Directives.Sorted := True;
  for I := idDeprecated to idForward do
    _Directives.AddObject(Idents[I], TObject(Integer(I)));
end;

function FindDirective(const S: string): TDirectiveIdent;
var
  I: Integer;
begin
  if _Directives.Find(S, I) then
    Result := TDirectiveIdent(_Directives.Objects[I])
  else
    Result := idNone;
end;

function TypIsVariantCompatible(typ: TType): Boolean;
begin
  Result := typ.TypeCode in [typShortint..typCurrency,
                typBoolean..typLongBool, typAnsiChar..typWideChar,
                typAnsiString..typShortString,
                typVariant..typOleVariant, typEnum, typSubrange,
                typInterface..typDispInterface];
end;

function TypIsVariantArithCompatible(typ: TType): Boolean;
begin
  Result := typ.TypeCode in [typShortint..typCurrency,
                typBoolean..typLongBool, typAnsiChar..typWideChar,
                typPAnsiChar..typPWideChar,
                typAnsiString..typShortString,
                typVariant..typOleVariant, typEnum, typSubrange,
                typInterface..typDispInterface];
  if not Result then
    Result := (typ.TypeCode = typArray) and TArrayType(typ).IsPackedString;
end;

function TypIsStringArithCompatible(typ: TType): Boolean;
begin
  Result := typ.TypeCode in [typAnsiString..typShortString,
                typPAnsiChar..typPWideChar, typAnsiChar..typWideChar,
                typVariant..typOleVariant];
  if not Result then
    Result := (typ.TypeCode = typArray) and TArrayType(typ).IsPackedString;
end;

{ TAstNode }

constructor TAstNode.Create;
begin

end;

destructor TAstNode.Destroy;
begin
  inherited;
end;

{ TSymbol }

procedure TSymbol.Add(Sym: TSymbol);
begin
  raise EASTError.CreateFmt('Add not implemented in %s', [Self.ClassName]);
end;

procedure TSymbol.AddSymbol(Sym: TSymbol);
begin
  raise EASTError.CreateFmt('AddSymbol not implemented in %s', [Self.ClassName]);
end;

function TSymbol.GetFullName: string;
var
  Sym: TSymbol;
begin
// 取符号的完全限定名
  Sym := Self;
  while Sym <> nil do
  begin
    if Sym <> Self then
      Result := '.' + Result;
    Result := Sym.Name + Result;
    Sym := Sym.Parent;
  end;
end;

function TSymbol.GetModule: TModule;
var
  Sym: TSymbol;
begin
  Sym := Self;
  while Assigned(Sym) and (Sym.NodeKind <> nkModule) do
    Sym := Sym.Parent;

  if not Assigned(Sym) or (Sym.NodeKind <> nkModule) then
    raise EASTError.CreateFmt('Symbol %s has not module', [Self.Name]);

  Result := TModule(Sym);
end;

function TSymbol.GetSymName: string;
var
  Sym: TSymbol;
begin
// 取符号限定名,不包括模块名
  Sym := Self;
  while Sym <> nil do
  begin
    if Sym <> Self then
      Result := '.' + Result;
    Result := Sym.Name + Result;
    Sym := Sym.Parent;
    if Sym.NodeKind = nkModule then Break;
  end;
end;

function TSymbol.GetUnitName: string;
var
  M: TModule;
begin
  M := GetModule;
  if Assigned(M) then
    Result := M.Name else
    Result := '';
end;
              
{ TModule }

procedure TModule.Add(Sym: TSymbol);
begin
  Symbols.Add(Sym);
end;

procedure TModule.AddSymbol(Sym: TSymbol);
begin
  if Sym.Parent <> nil then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);

  Sym.Parent := Self;
end;

constructor TModule.Create;
begin
  inherited;
  FNodeKind := nkModule;
  Symbols := TSymbolTable.Create(Self);
  Symbols.Capacity := 64;
  InternalSymbols := TSymbolTable.Create(Self);
  InternalSymbols.Capacity := 64;
  LoadedUnits := TSymbolTable.Create(nil);
  LoadedUnits.AutoAddToOwner := False;
  PointerSize := 4;
end;

destructor TModule.Destroy;
begin
  Symbols.Free;
  InternalSymbols.Free;
  LoadedUnits.Free;
  FType.Free;
  inherited;
end;

function TModule.FindSymbol(const S: string): TSymbol;
begin
  Result := Symbols.Find(S);
end;

function TModule.GetType: TType;
begin
  if FType = nil then
  begin
    FType := TSymbolType.Create;
    FType.Parent := Self.Parent;
    TSymbolType(FType).Reference := Self;
  end;
  Result := FType;
end;

procedure TModule.SetNameScope(const Scopes: array of string; Count: Integer);
var
  I: Integer;
begin
  SetLength(Names, Count);
  Self.Name := '';
  for I := 0 to High(Names) do
  begin
    Names[I] := Scopes[I];
    if I > 0 then
      Self.Name := Self.Name + '.';
    Self.Name := Self.Name + Names[I];
  end;
end;

{ TNameScope }

procedure TNameScope.Add(const S: string; Sym: TSymbol);
var
  I: Integer;
begin
  if not FSubNames.Find(S, I) then
    FSubNames.AddObject(S, Sym);
end;

constructor TNameScope.Create;
begin
  inherited;
  FSubNames := TStringList.Create;
  FSubNames.Sorted := True;
end;

destructor TNameScope.Destroy;
begin
  FSubNames.Free;
  FType.Free;
  inherited;
end;

function TNameScope.FindSymbol(const S: string): TSymbol;
var
  I: Integer;
begin
  if FSubNames.Find(S, I) then
    Result := TSymbol(FSubNames.Objects[I])
  else
    Result := nil;
end;

function TNameScope.GetType: TType;
begin
  if FType = nil then
  begin
    FType := TSymbolType.Create;
    FType.Parent := Self.Parent;
    TSymbolType(FType).Reference := Self;
  end;
  Result := FType;
end;

(*
{ TProgram }

constructor TProgram.Create;
begin
  inherited;
end;*)

{ TType }

constructor TType.Create;
begin
  inherited Create;
  FNodeKind := nkType;
end;

procedure TType.CreatePointerType(ASize: Cardinal);
begin
  if FPointerType = nil then
  begin
    FPointerType := TPointerType.Create;
    FPointerType.Parent := Self.Parent;
    FPointerType.Size := Size;
    TPointerType(FPointerType).RefType := Self;
  end;
end;

destructor TType.Destroy;
begin
  FPointerType.Free;
  inherited;
end;

function TType.Equals(typ: TType): Boolean;
  function ArgEquals(A1, A2: TArgument): Boolean;
  begin
    Result := (A1.ArgKind = A2.ArgKind) and
      (A1.Modifier = A2.Modifier) and
      (A1.ArgType.Equals(A2.ArgType));
  end;

  function RetEquals(T1, T2: TType): Boolean;
  begin
    if (T1 = nil) or (T2 = nil) then
      Result := T2 = T1
    else
      Result := T1.Equals(T2);
  end;

  function ProcEquals(P1, P2: TProceduralType): Boolean;
    function AllArgsEqual(P1, P2: TProceduralType): Boolean;
    var
      i: Integer;
    begin
      for i := 0 to P1.CountOfArgs - 1 do
        if not ArgEquals(TArgument(P1.Args[i]), TArgument(P2.Args[i])) then
        begin
          Result := False;
          Exit;
        end;
      Result := True;
    end;
  begin
    Result := (P1.IsMethodPointer = P2.IsMethodPointer)
              and (P1.CountOfArgs = P2.CountOfArgs)
              and RetEquals(P1.ReturnType, P2.ReturnType)
              and AllArgsEqual(P1, P2);
  end;
var
  src: TType;
begin
  src := Self.OriginalType;
  typ := typ.OriginalType;
  Result := src.TypeCode = typ.TypeCode;
  if Result then
    case typ.TypeCode of
      typAnsiString:
        Result := TAnsiStringType(src).CodePage = TAnsiStringType(typ).CodePage;
      typProcedural:
        Result := ProcEquals(TProceduralType(Self), TProceduralType(typ));
      typRecord, typObject, typClass,
      typInterface, typDispInterface:
        Result := (src = typ);
      typClassRef:
        Result := (TClassRefType(src).RefType = TClassRefType(typ).RefType);
      typEnum, typSet, typArray,
      typDynamicArray, typFile:
        Result := (src = typ);
      typSubrange:
        Result := TSubrangeType(src).BaseType.TypeCode =
                  TSubrangeType(typ).BaseType.TypeCode;
      typOpenArray:
        Result := TOpenArrayType(src).ElementType.Equals(
          TOpenArrayType(typ).ElementType);
    end;
end;

function TType.GetAlignSize: Byte;
begin
  Result := FSize;
end;

function TType.IsBoolean: Boolean;
begin
  if TypeCode = typSubrange then
    Result := TSubrangeType(Self).BaseType.TypeCode in [typBoolean..typLongBool]
  else
    Result := TypeCode in [typBoolean..typLongBool];
end;

function TType.IsInteger: Boolean;
begin
  if TypeCode = typSubrange then
    Result := TSubrangeType(Self).BaseType.TypeCode in [typShortint..typUInt64]
  else
    Result := TypeCode in [typShortint..typUInt64];
end;

function TType.IsMethodPointer: Boolean;
begin
  Result := (TypeCode = typProcedural) and TProceduralType(Self).IsMethodPointer;
end;

function TType.IsOrdinal: Boolean;
begin
  Result := TypeCode in [typShortint..typUInt64, typBoolean..typLongBool,
                        typAnsiChar, typWideChar, typSubrange, typEnum];
end;

function TType.IsPointer: Boolean;
begin
  Result := TypeCode in [typPointer, typPAnsiChar, typPWideChar];
end;

function TType.IsProcedural: Boolean;
begin
  Result := TypeCode = typProcedural;
end;

function TType.IsReal: Boolean;
begin
  Result := (TypeCode in [typComp..typCurrency]);
end;

function TType.IsSigned: Boolean;
begin
  Result := TypeCode in [typShortint, typSmallint, typLongint, typInt64,
                  typComp..typCurrency];
end;

function TType.IsSignedInt: Boolean;
begin
  Result := TypeCode in [typShortint, typSmallint, typLongint, typInt64];
end;

function TType.IsStringCompatible: Boolean;
begin
  Result := (TypeCode in [typAnsiString..typShortString]) or
      ((TypeCode = typArray) and TArrayType(Self).IsPackedString);
end;

function TType.IsUntypePointer: Boolean;
begin
  Result := (TypeCode = typPointer) and (TPointerType(Self).RefType = nil);
end;

function TType.IsVariantCompatible: Boolean;
begin
  Result := TypeCode in [typShortint..typCurrency,
                typBoolean..typLongBool, typAnsiChar..typWideChar,
                typAnsiString..typShortString,
                typVariant..typOleVariant, typEnum, typSubrange,
                typInterface..typDispInterface];
end;

function TType.NormalType: TType;
begin
  Result := OriginalType;
  if Result.TypeCode = typSubrange then
    Result := TSubrangeType(Result).BaseType;
end;

function TType.OriginalType: TType;
begin
  Result := Self;
  while True do
  begin
    if Result.TypeCode = typAlias then
      Result := TAliasType(Result).RefType
    else if Result.TypeCode = typClonedType then
      Result := TClonedAliasType(Result).RefType
    else
      Break;
    Assert(Result <> nil);
  end;
end;

{procedure TType.Update(const CntxInfo: TContextInfo);
begin

end;}

{ TPrimitiveType }

constructor TPrimitiveType.Create(ACode: TTypeCode);
begin
  inherited Create;
  FTypeCode := ACode;
end;

function TPrimitiveType.GetAlignSize: Byte;
begin
  if FAlign > 0 then
    Result := FAlign
  else
    Result := inherited GetAlignSize;
end;

{ TUnresolvedType }

constructor TUnresolvedType.Create;
begin
  inherited;
  FTypeCode := typUntype;
end;

{ TAliasType }

constructor TAliasType.Create;
begin
  inherited;
  FTypeCode := typAlias;
end;

procedure TAliasType.Update;
begin
  FSize := RefType.FSize;
end;

{ TTypedAliasType }

constructor TClonedAliasType.Create;
begin
  inherited;
  FTypeCode := typClonedType;
end;

{ TPointerType }

constructor TPointerType.Create;
begin
  inherited;
  FTypeCode := typPointer;
end;

function TPointerType.IsUntype: Boolean;
begin
  Result := RefType = nil;
end;

{ TAnsiStringType }

constructor TAnsiStringType.Create;
begin
  inherited;
  FTypeCode := typAnsiString;
end;

{ TArrayType }

constructor TArrayType.Create;
begin
  inherited;
  FTypeCode := typArray;
end;

function TArrayType.GetAlignSize: Byte;
begin
  if IsPacked then
    Result := 1 else
    Result := ElementType.AlignSize;
end;

function TArrayType.GetDimensionCount: Integer;
var
  Arr: TArrayType;
begin
  Result := 1;
  Arr := Self;
  while Arr.ElementType.TypeCode = typArray do
  begin
    Arr := TArrayType(Arr.ElementType);
    Inc(Result);
  end;
end;

function TArrayType.IsPackedString: Boolean;
begin
  Result := (DimensionCount = 1) and
            (ElementType.TypeCode in [typAnsiChar, typWideChar]);
end;

procedure TArrayType.Update;
var
  T: TType;
begin
// 如果是个packed record,不需要根据record中最大对齐来排列,只要紧密排列即可
// 如果不是packed record,那么它的大小已经根据最大对齐校正了,也只要紧密排列即可
  if not Assigned(ElementType) then
    FSize := 0
  else begin
    FSize := Cardinal(Range.RangeEnd - Range.RangeBegin + 1) * ElementType.Size;

    ArrayAttr := [];
    T := ElementType;
    case T.TypeCode of
      typRecord:
        begin
          if staNeedInit in TRecordType(T).RecordAttr then
            Include(ArrayAttr, staNeedInit);
          if staNeedFree in TRecordType(T).RecordAttr then
          Include(ArrayAttr, staNeedFree);
        end;
      typArray:
        begin
          if staNeedInit in TArrayType(T).ArrayAttr then
            Include(ArrayAttr, staNeedInit);
          if staNeedFree in TArrayType(T).ArrayAttr then
            Include(ArrayAttr, staNeedFree);
        end;
    else
      if T.TypeCode in AutoInitTypes then
        Include(ArrayAttr, staNeedInit);
      if T.TypeCode in AutoFreeTypes then
        Include(ArrayAttr, staNeedFree);
    end;
  end;
end;

{ TDynamicArrayType }

constructor TDynamicArrayType.Create;
begin
  inherited;
  FTypeCode := typDynamicArray;
end;

function TDynamicArrayType.GetAlignSize: Byte;
begin
  if IsPacked then
    Result := 1 else
    Result := ElementType.AlignSize;
end;

{ TOpenArrayType }

constructor TOpenArrayType.Create;
begin
  inherited;
  FTypeCode := typOpenArray;
end;

function TOpenArrayType.GetAlignSize: Byte;
begin
  Result := 1;
end;

{ TEnumType }

procedure TEnumType.CalcSize(MinSize: Integer);
var
  hiSize, loSize: Integer;
begin
  if MinSize = 4 then
  begin
    FSize := 4;
    Exit;
  end;
// DONE 1: 需要修改，比如my=(a,b=255)只需要1字节

  if FLowValue < 0 then
  begin
    if FHighValue > 32767 then
      hiSize := 4
    else if FHighValue > 127 then
      hiSize := 2
    else
      hiSize := 1;

    if FLowValue < -32768 then
      loSize := 4
    else if FLowValue < -128 then
      loSize := 2
    else
      loSize := 1;
  end
  else
  begin
    if FHighValue > 65535 then
      hiSize := 4
    else if FHighValue > 255 then
      hiSize := 2
    else
      hiSize := 1;

    if FLowValue < -32768 then
      loSize := 4
    else if FLowValue < -128 then
      loSize := 2
    else
      loSize := 1;
  end;

  if hiSize > loSize then
    FSize := hiSize
  else
    FSize := loSize;

  if Integer(FSize) < MinSize then
    FSize := MinSize;
end;

constructor TEnumType.Create;
begin
  inherited Create;
  FTypeCode := typEnum;
  Values := TList.Create;
  Values.Capacity := 32;
  MinEnumSize := 1;
end;

destructor TEnumType.Destroy;
begin
  Values.Free;
  inherited;
end;

procedure TEnumType.Update;
begin
  UpdateRange;
  CalcSize(MinEnumSize);
end;

procedure TEnumType.UpdateRange;
var
  I: Integer;
begin
  if Values.Count < 1 then
    raise EASTError.Create('empty enum');

  FHighValue := TEnumValue(Values[0]).Value;
  FLowValue := TEnumValue(Values[0]).Value;
  for I := 1 to Values.Count - 1 do
  begin
    with TEnumValue(Values[I]) do
    begin
      if FHighValue < Value then
        FHighValue := Value;
      if FLowValue > Value then
        FLowValue := Value;
    end;
  end;
end;

{ TShortStringType }

constructor TShortStringType.Create;
begin
  inherited;
  FTypeCode := typShortString;
end;

function TShortStringType.GetAlignSize: Byte;
begin
  Result := 1;
end;

procedure TShortStringType.Update;
begin
  Self.FSize := CharCount + 2;
end;

{ TField }

constructor TField.Create;
begin
  inherited;
  FNodeKind := nkField;
end;

{ TMultiAccessor }

procedure TMultiAccessor.Add(Field: TSymbol);
begin
  if Length(Fields) <= FieldCount then
    SetLength(Fields, FieldCount + 4);
  Fields[FieldCount] := Field;
  Inc(FieldCount);
end;

constructor TMultiAccessor.Create;
begin
  inherited;
  FNodeKind := nkAccessor;
end;

function TMultiAccessor.First: TSymbol;
begin
  if FieldCount > 0 then
    Result := Fields[0] else
    Result := nil;
end;

function TMultiAccessor.Last: TField;
begin
  if FieldCount > 0 then
  begin
    if Fields[FieldCount - 1].NodeKind <> nkField then
      raise EASTError.Create('Invalid accessor');
    Result := TField(Fields[FieldCount - 1]);
  end
  else
    Result := nil;
end;

{ TProperty }

function TProperty.CountOfArgs: Integer;
begin
  if Args = nil then
    Result := 0 else
    Result := Args.Count;
end;

constructor TProperty.Create;
begin
  inherited;
  FNodeKind := nkProperty;
  Index := -2147483647-1;
end;

procedure TProperty.CreateArgs;
begin
  if Args = nil then Args := TList.Create;
end;

destructor TProperty.Destroy;
begin
  ValClear(DefaultValue);
  Args.Free;
  inherited;
end;

function TProperty.GetterType: TType;
begin
  if Getter = nil then
    Result := nil
  else
    case Getter.NodeKind of
      nkField: Result := TField(Getter).FieldType;
      nkMethod: Result := TMethod(Getter).ReturnType;
      nkAccessor: Result := TMultiAccessor(Getter).Last.FieldType;
      else
        raise EASTError.Create('Invalid getter');
    end;
end;

function TProperty.HasIndexSpec: Boolean;
begin
  Result := Index <> -2147483647-1; // d7 bug:-2147483648报overflow 
end;

function TProperty.SetterType: TType;
begin
  if Setter = nil then
    Result := nil
  else
    case Setter.NodeKind of
      nkField: Result := TField(Setter).FieldType;
      nkMethod: Result := TMethod(Setter).ReturnType;
      nkAccessor: Result := TMultiAccessor(Setter).Last.FieldType;
      else
        raise EASTError.Create('Invalid getter');
    end;
end;

{ TRecordBody }

procedure TRecordBody.AddFields(Symbols: TSymbolTable);
var
  I: Integer;
  VarPart: TRecordVariant;
begin
  for I := 0 to Members.Count - 1 do
  begin
    Symbols.Add(TSymbol(Members[I]));
  end;
  if (Selector <> nil) and (Selector.Name <> '') then
  begin
    Symbols.Add(Selector);
  end;
  VarPart := Variants;
  while VarPart <> nil do
  begin
    VarPart.AddFields(Symbols);
    VarPart := VarPart.Next;
  end;
end;

constructor TRecordBody.Create;
begin
  Members := TList.Create;
  MaxAlignSize := 1;
end;

destructor TRecordBody.Destroy;
begin
  Members.Free;
  Variants.Free;
  inherited;
end;

function TRecordBody.RecordSize: Int64;
var
  VarPart: TRecordVariant;
  I, Off, Size: Cardinal;
begin
  if Variants <> nil then
  begin
    VarPart := Variants;
    Off := VarPart.Offset;
    Size := 1;
    while VarPart <> nil do
    begin
      I := VarPart.RecordSize;
      Dec(I, VarPart.Offset);
      if I > Size then Size := I;
      VarPart := VarPart.Next;
    end;
  end
  else if (Selector <> nil) and (Selector.Name <> '') then
    with Selector do begin
      Off := Offset;
      Size := FieldType.Size;
    end
  else if Members.Count > 0 then
    with TField(Members.Last) do begin
      Off := Offset;
      Size := FieldType.Size;
    end
  else begin
    Off := 0;
    Size := 1;
  end;

  if Off > $7fffffff then
    Result := Off
  else if Size > $7fffffff then
    Result := Size
  else
    Result := Off + Int64(Size);

//  if Result <= $7fffffff then
// todo 1: MaxAlignSize要改为GlobalAlizeSize和MaxAlignSize之间最小的
    Result := (Result + MaxAlignSize - 1) and not (MaxAlignSize - 1);
end;

procedure TRecordBody.Update(GlobalAlignSize: Byte; Offset: Cardinal);

  procedure UpdateField(F: TField; var Offset: Cardinal);
  var
    AlSize: Byte;
  begin
    if Offset > $7fffffff then // 一旦大于2GB, 不必再计算
    begin
      F.Offset := Offset;
      Exit;
    end;

    AlSize := F.FieldType.AlignSize;
    if AlSize > GlobalAlignSize then
      AlSize := GlobalAlignSize;
    if AlSize > 1 then
      Offset := (Offset + AlSize -1) and not (AlSize-1);
    F.Offset := Offset;
    if F.FieldType.Size <= $7fffffff then
      Offset := Offset + F.FieldType.Size
    else
      Offset := F.FieldType.Size;
  end;

  procedure AlignTo(var Offset: Cardinal; AlSize: Byte);
  begin
    if AlSize > GlobalAlignSize then
      AlSize := GlobalAlignSize;

    Offset := (Offset + AlSize - 1) and not (AlSize-1);
  end;
var
  I: Integer;
  F: TField;
  VarPart: TRecordVariant;
begin
  for I := 0 to Members.Count - 1 do
  begin
    F := TField(Members[I]);
    UpdateField(F, Offset);
  end;

  if (Selector <> nil) and (Selector.Name <> '') then
    UpdateField(Selector, Offset);

  VarPart := Variants;
  if VarPart <> nil then
  begin
    // 同组的变体部分的MaxAlignSize相同
    AlignTo(Offset, VarPart.MaxAlignSize);
    while VarPart <> nil do
    begin
      VarPart.Update(GlobalAlignSize, Offset);
      VarPart.Offset := Offset;
      VarPart := VarPart.Next;
    end;
  end;
end;

procedure TRecordBody.UpdateAlign;
var
  I, Align: Integer;
  VarPart: TRecordVariant;
begin
  VarPart := Variants;
  while VarPart <> nil do
  begin
    VarPart.UpdateAlign;
    VarPart := VarPart.Next;
  end;

  // 取最大的Align
  Align := 1;
  for I := 0 to Members.Count - 1 do
    with TField(Members[I]).FieldType do
      if AlignSize > Align then Align := AlignSize;

  if (Selector <> nil) and (Selector.Name <> '') then
    with Selector.FieldType do
      if AlignSize > Align then Align := AlignSize;

  MaxAlignSize := Align;

  // 取变体部分最大的Align
  Align := 1;
  VarPart := Variants;
  while VarPart <> nil do
  begin
    if VarPart.MaxAlignSize > Align then
      Align := VarPart.MaxAlignSize;
    VarPart := VarPart.Next;
  end;

  // 同一组变体共享相同的内存,因此它们也应该有相同的Align
  // 统一使用最大的Align
  VarPart := Variants;
  while VarPart <> nil do
  begin
    VarPart.MaxAlignSize := Align;
    VarPart := VarPart.Next;
  end;

  if Align > MaxAlignSize then MaxAlignSize := Align; 
end;

{ TRecordVariant }

destructor TRecordVariant.Destroy;
begin
  Next.Free;
  inherited;
end;

{ TRecordType }

procedure TRecordType.Add(Sym: TSymbol);
begin
  Symbols.Add(Sym);
end;

procedure TRecordType.AddSymbol(Sym: TSymbol);
begin
  if Sym.Parent <> nil then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);
  Sym.Parent := Self;
end;

constructor TRecordType.Create;
begin
  inherited;
  FTypeCode := typRecord;
  GlobalAlignSize := 8;
  Symbols := TSymbolTable.Create(Self);
end;

destructor TRecordType.Destroy;
begin
  Symbols.Free;
  Body.Free;
  inherited;
end;

function TRecordType.FindSymbol(const S: string): TSymbol;
begin
  Result := Symbols.Find(S);
end;

function TRecordType.GetAlignSize: Byte;
begin
  if Body <> nil then
    Result := Body.MaxAlignSize
  else
    Result := 1;
end;

procedure TRecordType.Update;
var
  i: Integer;
  T: TType;
begin
  if Body <> nil then
  begin
    if GlobalAlignSize > 1 then
      UpdateAlign;
    Body.Update(GlobalAlignSize, 0);
    FSize := Body.RecordSize;
  end;

  for i := 0 to Symbols.Count - 1 do
  begin
    T := TField(Symbols[i]).FieldType;
    case T.TypeCode of
      typRecord:
        begin
          if staNeedInit in TRecordType(T).RecordAttr then
            Include(RecordAttr, staNeedInit);
          if staNeedFree in TRecordType(T).RecordAttr then
            Include(RecordAttr, staNeedFree);
        end;
      typArray:
        begin
          if staNeedInit in TArrayType(T).ArrayAttr then
            Include(RecordAttr, staNeedInit);
          if staNeedFree in TArrayType(T).ArrayAttr then
            Include(RecordAttr, staNeedFree);
        end;
    else
      if T.TypeCode in AutoInitTypes then
        Include(RecordAttr, staNeedInit);
      if T.TypeCode in AutoFreeTypes then
        Include(RecordAttr, staNeedFree);
    end;
  end;
end;

procedure TRecordType.UpdateAlign;
begin
  if Body <> nil then
    Body.UpdateAlign;
end;

{ TClassType }

procedure TClassType.Add(Sym: TSymbol);
begin
  Symbols.Add(Sym);
end;

procedure TClassType.AddSymbol(Sym: TSymbol);
begin
  if Sym.Parent <> nil then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);
  Sym.Parent := Self;
end;

constructor TClassType.Create;
begin
  inherited;
  FTypeCode := typClass;
  FSize := 4;
  GlobalAlignSize := 8;
  Symbols := TSymbolTable.Create(Self);
end;

procedure TClassType.CreateInterfaces;
begin
  if Interfaces = nil then Interfaces := TList.Create;
end;

destructor TClassType.Destroy;
begin
  FAllSymbols.Free;
  Symbols.Free;
  Interfaces.Free;
  FClassRef.Free;
  inherited;
end;

function TClassType.FindBaseSymbol(const S: string): TSymbol;
begin
  if Base <> nil then
    Result := Base.FindSymbol(S)
  else
    Result := nil;
end;

function TClassType.FindCurSymbol(const S: string): TSymbol;
begin
  Result := Symbols.Find(S);
end;

function TClassType.FindSymbol(const S: string): TSymbol;
var
  Base: TClassType;
begin
  Base := Self;
  while Base <> nil do
  begin
    Result := Base.FindCurSymbol(S);
    if Result <> nil then Exit;
    Base := Base.Base;
  end;

  Result := nil;
end;

function TClassType.GetAllSymbols: TSymbolTable;

  procedure AddSymbols(Typ: TClassType; SymTable: TSymbolTable);
  var
    I: Integer;
  begin
    if Typ.Base <> nil then
      AddSymbols(Typ.Base, SymTable);
    with SymTable do
      Capacity := Capacity + Typ.Symbols.Count;
    for I := 0 to Typ.Symbols.Count - 1 do
      SymTable.Add(Typ.Symbols[I]);
  end;
begin
  if not Assigned(FAllSymbols) then
  begin
    FAllSymbols := TSymbolTable.Create(nil);
    FAllSymbols.Capacity := 32;
    AddSymbols(Self, FAllSymbols);
  end;
  Result := FAllSymbols;
end;

function TClassType.GetClassRef: TClassRefType;
begin
  if FClassRef = nil then
  begin
    FClassRef := TClassRefType.Create;
    FClassRef.RefType := Self;
    FClassRef.FParent := Self.Parent;
    Include(FClassRef.Attr, saTemp);
    FClassRef.Name := '$' + Self.Name;
  end;
  Result := FClassRef;
end;

function TClassType.IsImplemented(AIntf: TInterfaceType): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Self.Interfaces <> nil then
    for I := 0 to Self.Interfaces.Count - 1 do
      if Self.Interfaces[I] = AIntf then
      begin
        Result := True;
        Break;
      end;
  if not Result and Assigned(Base) then
    Result := Base.IsImplemented(AIntf);
end;

function TClassType.IsInheritedFrom(ABase: TClassType): Boolean;
var
  C: TClassType;
begin
  //C := Self.Base;
  C := Self;
  while Assigned(C) and (C <> ABase) do
    C := C.Base;
  Result := C = ABase;
end;

function TClassType.RttiEnabled: Boolean;
var
  Base: TClassType;
begin
  Base := Self;
  while Assigned(Base) and not (caRtti in Base.ClassAttr) do
    Base := Base.Base;
  Result := Base <> nil;
end;

procedure TClassType.Update(PtrSize: Integer);
var
  MaxAlign: Integer;

  procedure UpdateField(F: TField; var Offset: Int64);
  var
    AlSize: Byte;
  begin
    if Offset > $7fffffff then // 一旦大于2GB, 不必再计算
    begin
      F.Offset := Offset;
      Exit;
    end;

    AlSize := F.FieldType.AlignSize;
    if MaxAlign < AlSize then MaxAlign := AlSize;
    if AlSize > GlobalAlignSize then
      AlSize := GlobalAlignSize;
    if AlSize > 1 then
      Offset := (Offset + AlSize -1) and not (AlSize-1);
    F.Offset := Offset;
    if F.FieldType.Size <= $7fffffff then
      Offset := Offset + F.FieldType.Size
    else
      Offset := F.FieldType.Size;
  end;

  procedure UpdateRootVmt(const Meth: string; Index: Integer);
  var
    Sym: TSymbol;
  begin
    Sym := FindCurSymbol(Meth);
    if Assigned(Sym) and (Sym.NodeKind = nkMethod)
        and (fmVirtual in TMethod(Sym).Modifiers) then
    begin
      TMethod(Sym).VTIndex := Index - ROOT_VMT_OFFSET;
      Vmt[Index] := TMethod(Sym);
    end;
  end;
var
  i, idx: Integer;
  Offset: Int64;
  Sym: TSymbol;
begin
  if Base = nil then
  begin
  // todo 77: MARK 根类
  // 基类无字段,只有一个vmt指针
    ObjectSize := PtrSize;
    VmtEntries := ROOT_VMT_OFFSET;
    SetLength(Vmt, VmtEntries);
    UpdateRootVmt('SafeCallException', 0);
    UpdateRootVmt('AfterConstruction', 1);
    UpdateRootVmt('BeforeDestruction', 2);
    UpdateRootVmt('Dispatch', 3);
    UpdateRootVmt('DefaultHandler', 4);
    UpdateRootVmt('NewInstance', 5);
    UpdateRootVmt('FreeInstance', 6);
    UpdateRootVmt('Destroy', 7);
    Exit;
  end
  else
    Offset := Base.ObjectSize;

  MaxAlign := 0;
  for i := 0 to Symbols.Count - 1 do
  begin
    Sym := Symbols[i];
    if Sym.NodeKind = nkField then
      UpdateField(TField(Sym), Offset);
  end;

  if Interfaces <> nil then
  begin
    // Interface entries
    Offset := (Offset + PtrSize - 1) and not (PtrSize - 1);
    for i := 0 to Interfaces.Count - 1 do
      Offset := Offset + PtrSize;
  end;

  if MaxAlign > GlobalAlignSize then
    MaxAlign := GlobalAlignSize;
  if MaxAlign > 0 then
    ObjectSize := (Offset + MaxAlign - 1) and not (MaxAlign - 1)
  else
    ObjectSize := Offset;

  // 处理VMT
  VmtEntries := Base.VmtEntries;
  for i := 0 to Symbols.Count - 1 do
  begin
    Sym := Symbols[i];
    if not (saStatic in Sym.Attr) and (Sym.NodeKind = nkMethod)
        and ([fmVirtual, fmOverride] * TFunction(Sym).Modifiers = [fmVirtual]) then
    begin
      TMethod(Sym).VTIndex := VmtEntries - ROOT_VMT_OFFSET;
      Inc(VmtEntries);
    end;
  end;
  // todo 1: 计算VMT，需要再考虑接口方法

  SetLength(Vmt, VmtEntries);
  if VmtEntries > 0 then
  begin
    if Assigned(Base) and (Base.VmtEntries > 0) then
      Move(Base.Vmt[0], Self.Vmt[0], Base.VmtEntries * SizeOf(Pointer));

    for i := 0 to Symbols.Count - 1 do
    begin
      Sym := Symbols[i];
      if not (saStatic in Sym.Attr) and (Sym.NodeKind = nkMethod)
        and ([fmVirtual, fmOverride] * TFunction(Sym).Modifiers <> []) then
      begin
        idx := TMethod(Sym).VTIndex + ROOT_VMT_OFFSET;

        if (idx >= 0) and (idx < VmtEntries) then
          Vmt[idx] := TMethod(Sym)
        else
          Assert(False, 'Vmt index out of bound');
      end;
    end;
  end;
end;

{ TSubrangeType }

constructor TSubrangeType.Create;
begin
  inherited;
  FTypeCode := typSubrange;
end;
{
destructor TSubrangeType.Destroy;
begin
  inherited;
end;}

procedure TSubrangeType.SetBaseType(const Value: TType);
begin
  if (Value <> nil) and not Value.IsOrdinal then
    raise EASTError.Create('Subrange需要有序类型');
  FBaseType := Value;
  if FBaseType <> nil then
    Self.Size := FBaseType.Size;
end;

function TSubrangeType.SubSetOf(typ: TSubrangeType): Boolean;
begin
  Result := (typ.RangeBegin <= Self.RangeBegin) and (typ.RangeEnd >= Self.RangeEnd);
end;

{ TSetType }

constructor TSetType.Create;
begin
  inherited;
  FTypeCode := typSet;
end;

function TSetType.GetAlignSize: Byte;
begin
  if Size = 3 then
    Result := 4
  else if Size > 4 then
    Result := 1
  else
    Result := Size;
end;

function TSetType.IsCommonSetType: Boolean;
begin
  Result := RangeType = nil;
end;

procedure TSetType.Update;
begin
  UpdateSize;
end;

procedure TSetType.UpdateSize;
begin
  if RangeType <> nil then
  begin
    FSize := RangeType.RangeEnd div 8 - RangeType.RangeBegin div 8 + 1;
    if Size = 3 then FSize := 4;
  end
  else
    FSize := 32;
end;

{ TClassRefType }

constructor TClassRefType.Create;
begin
  inherited;
  FTypeCode := typClassRef;
  FSize := 4;
end;

function TClassRefType.IsInheritedFrom(ClassRef: TClassRefType): Boolean;
begin
  if (Self.RefType <> nil) and (ClassRef.RefType <> nil) then
  begin
    Result := RefType.IsInheritedFrom(ClassRef.RefType);
  end
  else
    Result := False;
end;

{ TIntfProperty }

function TIntfProperty.CountOfArgs: Integer;
begin
  if Args = nil then
    Result := 0 else
    Result := Args.Count;
end;

constructor TIntfProperty.Create;
begin
  inherited;
  FNodeKind := nkIntfProperty;
end;

procedure TIntfProperty.CreateArgs;
begin
  if Args = nil then Args := TList.Create;
end;

destructor TIntfProperty.Destroy;
begin
  Args.Free;
  inherited;
end;

{ TInterfaceType }

procedure TInterfaceType.Add(Sym: TSymbol);
begin
  Symbols.Add(Sym);
end;

procedure TInterfaceType.AddSymbol(Sym: TSymbol);
begin
  if Sym.Parent <> nil then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);
  Sym.Parent := Self;
end;

constructor TInterfaceType.Create;
begin
  inherited;
  FTypeCode := typInterface;
  FSize := 4;
  Symbols := TSymbolTable.Create(Self);
end;

destructor TInterfaceType.Destroy;
begin
  Symbols.Free;
  FAllSymbols.Free;
  inherited;
end;

function TInterfaceType.FindBaseSymbol(const S: string): TSymbol;
begin
  if Base <> nil then
    Result := Base.FindSymbol(S)
  else
    Result := nil;
end;

function TInterfaceType.FindCurSymbol(const S: string): TSymbol;
begin
  Result := Symbols.Find(S);
end;

function TInterfaceType.FindSymbol(const S: string): TSymbol;
var
  Base: TInterfaceType;
begin
  Base := Self;
  while Base <> nil do
  begin
    Result := Base.FindCurSymbol(S);
    if Result <> nil then Exit;
    Base := Base.Base;
  end;

  Result := nil;
end;

function TInterfaceType.GetAllSymbols: TSymbolTable;

  procedure AddSymbols(Typ: TInterfaceType; SymTable: TSymbolTable);
  var
    I: Integer;
  begin
    if Typ.Base <> nil then
      AddSymbols(Typ.Base, SymTable);
    with SymTable do
      Capacity := Capacity + Typ.Symbols.Count;
    for I := 0 to Typ.Symbols.Count - 1 do
      SymTable.Add(Typ.Symbols[I]);
  end;
begin
  if not Assigned(FAllSymbols) then
  begin
    FAllSymbols := TSymbolTable.Create(nil);
    FAllSymbols.Capacity := 16;
    AddSymbols(Self, FAllSymbols);
  end;
  Result := FAllSymbols;
end;

function TInterfaceType.IsInheritedFrom(ABase: TInterfaceType): Boolean;
var
  C: TInterfaceType;
begin
//  C := Self.Base;
  C := Self;
  while Assigned(C) and (C <> ABase) do
    C := C.Base;
  Result := C = ABase;
end;

{ TObjectType }

procedure TObjectType.Add(Sym: TSymbol);
begin
  Symbols.Add(Sym);
end;

procedure TObjectType.AddSymbol(Sym: TSymbol);
begin
  if Sym.Parent <> nil then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);
  Sym.Parent := Self;
end;

constructor TObjectType.Create;
begin
  inherited;
  FTypeCode := typObject;
  Symbols := TSymbolTable.Create(Self);
end;

destructor TObjectType.Destroy;
begin
  Symbols.Free;
  FAllSymbols.Free;
  inherited;
end;

function TObjectType.FindBaseSymbol(const S: string): TSymbol;
begin
  if Base <> nil then
    Result := Base.FindSymbol(S)
  else
    Result := nil;
end;

function TObjectType.FindCurSymbol(const S: string): TSymbol;
begin
  Result := Symbols.Find(S);
end;

function TObjectType.FindSymbol(const S: string): TSymbol;
var
  Base: TObjectType;
begin
  Base := Self;
  while Base <> nil do
  begin
    Result := Base.FindCurSymbol(S);
    if Result <> nil then Exit;
    Base := Base.Base;
  end;

  Result := nil;
end;

function TObjectType.GetAllSymbols: TSymbolTable;

  procedure AddSymbols(Typ: TObjectType; SymTable: TSymbolTable);
  var
    I: Integer;
  begin
    if Typ.Base <> nil then
      AddSymbols(Typ.Base, SymTable);
    with SymTable do
      Capacity := Capacity + Typ.Symbols.Count;
    for I := 0 to Typ.Symbols.Count - 1 do
      SymTable.Add(Typ.Symbols[I]);
  end;
begin
  if not Assigned(FAllSymbols) then
  begin
    FAllSymbols := TSymbolTable.Create(nil);
    FAllSymbols.Capacity := 32;
    AddSymbols(Self, FAllSymbols);
  end;
  Result := FAllSymbols;
end;

function TObjectType.IsInheritedFrom(ABase: TObjectType): Boolean;
var
  C: TObjectType;
begin
//  C := Self.Base;
  C := Self;
  while Assigned(C) and (C <> ABase) do
    C := C.Base;
  Result := C = ABase;
end;

procedure TObjectType.Update(PtrSize: Integer);
var
  MaxAlign: Integer;

  procedure UpdateField(F: TField; var Offset: Int64);
  var
    AlSize: Byte;
  begin
    if Offset > $7fffffff then // 一旦大于2GB, 不必再计算
    begin
      F.Offset := Offset;
      Exit;
    end;

    AlSize := F.FieldType.AlignSize;
    if MaxAlign < AlSize then MaxAlign := AlSize;
    if AlSize > GlobalAlignSize then
      AlSize := GlobalAlignSize;
    if AlSize > 1 then
      Offset := (Offset + AlSize -1) and not (AlSize-1);
    F.Offset := Offset;
    if F.FieldType.Size <= $7fffffff then
      Offset := Offset + F.FieldType.Size
    else
      Offset := F.FieldType.Size;
  end;

var
  i, BaseVmt: Integer;
  Offset: Int64;
  Sym: TSymbol;
begin
 { for i := 0 to Symbols.Count - 1 do
  begin
    Sym := Symbols[i];
    if Sym.NodeKind = nkMethod then
      if fmVirtual in TFunctionDecl(Sym).Modifiers then
      begin
        Include(ObjectAttr, oaHasVirtual);
        Break;
      end;
  end;}

  if Assigned(Base) then
    BaseVmt := Base.VmtEntries
  else
    BaseVmt := 0;

  VmtEntries := BaseVmt;
  for i := 0 to Symbols.Count - 1 do
  begin
    Sym := Symbols[i];
    if not (saStatic in Sym.Attr) and (Sym.NodeKind = nkMethod)
        and ([fmVirtual, fmOverride] * TFunction(Sym).Modifiers = [fmVirtual]) then
    begin
      TMethod(Sym).VTIndex := VmtEntries;
      Inc(VmtEntries);
    end;
  end;

  if VmtEntries > BaseVmt then
    Include(ObjectAttr, oaHasVirtual);

  if oaHasVirtual in ObjectAttr then
    Include(ObjectAttr, oaHasVmt)
  else if (Base <> nil) and (oaHasVmt in Base.ObjectAttr) then
    Include(ObjectAttr, oaHasVmt);

  if Base = nil then
  begin
    if oaHasVmt in ObjectAttr then
      Include(ObjectAttr, oaBeginVmt);
  end
  else if oaHasVmt in ObjectAttr then
  begin
    if not (oaHasVmt in base.ObjectAttr) then
      Include(ObjectAttr, oaBeginVmt);
  end;

  if Base = nil then
    Offset := 0
  else
    Offset := Base.Size;

  MaxAlign := 0;
  for i := 0 to Symbols.Count - 1 do
  begin
    Sym := Symbols[i];
    if Sym.NodeKind = nkField then
      UpdateField(TField(Sym), Offset);
  end;

  // 添加vmt
  if oaBeginVmt in ObjectAttr then
  begin
    // 先对齐指针
    Offset := (Offset + PtrSize - 1) and not (PtrSize - 1);
    VmtOffset := Offset;
    // 再添加vmt指针
    Offset := Offset + PtrSize;
  end;

  if MaxAlign > GlobalAlignSize then
    MaxAlign := GlobalAlignSize;
  if MaxAlign > 0 then
    FSize := (Offset + MaxAlign - 1) and not (MaxAlign - 1)
  else
    FSize := Offset;
  if FSize = 0 then FSize := 1;
end;

{ TFileType }

constructor TFileType.Create;
begin
  inherited;
  FTypeCode := typFile;
  FSize := 4;
end;

function TFileType.IsUntype: Boolean;
begin
  Result := ElementType = nil;
end;

{ TProceduralType }

function TProceduralType.CountOfArgs: Integer;
begin
  if Args = nil then
    Result := 0 else
    Result := Args.Count;
end;

constructor TProceduralType.Create;
begin
  inherited;

  FTypeCode := typProcedural;
  FSize := 4;
end;

procedure TProceduralType.CreateArgs;
begin
  if Args = nil then Args := TList.Create;
end;

destructor TProceduralType.Destroy;
begin
  Args.Free;
  inherited;
end;

function TProceduralType.MinOfArgs: Integer;
var
  I: Integer;
begin
  Result := 0;
  if Args = nil then Exit;

  for I := 0 to Args.Count - 1 do
    if TArgument(Args[I]).DefaultValue.VT = vtEmpty then
      Inc(Result);
end;

{ TSymbolType }

constructor TSymbolType.Create;
begin
  inherited;
  FTypeCode := typSymbol;
end;

{ TExpr }

constructor TExpr.Create;
begin
  inherited Create;
  FNodeKind := nkExpr;
end;

function TExpr.GetFunctionSymbol: TFunctionDecl;
var
  Ref: TSymbol;
begin
  Ref := GetReference;
  if (Ref <> nil) and (Ref.NodeKind in [nkFunc, nkMethod, nkExternalFunc]) then
    Result := TFunctionDecl(Ref)
  else
    Result := nil;
end;

function TExpr.GetReference: TSymbol;
begin
  case OpCode of
    opSYMBOL: Result := TSymbolExpr(Self).Reference;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        Result := TBinaryExpr(Self).Right.GetReference
      else
        Result := nil;
  else
    Result := nil;
  end;
end;

function TExpr.GetVariableSymbol: TVariable;
var
  Ref: TSymbol;
begin
  Ref := GetReference;
  if (Ref <> nil) and (Ref.NodeKind = nkVariable) then
    Result := TVariable(Ref)
  else
    Result := nil;
end;

function TExpr.HasMemory: Boolean;
const
  ExpectedKinds = [nkField, nkVariable, nkArgument];
begin
  case OpCode of
    opSYMBOL: Result := TSymbolExpr(Self).Reference.NodeKind in ExpectedKinds;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        Result := TBinaryExpr(Self).Right.HasMemory
      else
        Result := False;
    opINDEX:
      Result := TBinaryExpr(Self).Left.HasMemory;
    opINST:
      Result := True;
    opCAST:
      Result := eaVarCast in Self.Attr;
      {with TBinaryExpr(Self) do
      begin
        Result := (Right <> nil) and (TUnaryExpr(Right).Operand <> nil)
                  and TUnaryExpr(Right).Operand.HasMemory;
      end;}

  else
    Result := False;
  end;
end;

function TExpr.IsClassType: Boolean;
var
  Sym: TSymbol;
begin
  Sym := GetReference;
  if Sym <> nil then
    Result := (Sym.NodeKind = nkType) and (TType(Sym).TypeCode = typClass)
  else
    Result := False;
//  Result := (OpCode = opSYMBOL) and
//            (TSymbolExpr(Self).Reference.NodeKind = nkType) and
//            (TType(TSymbolExpr(Self).Reference).TypeCode = typClass);
end;

{function TExpr.IsConstant: Boolean;
begin
  case OpCode of
    opSYMBOL: Result := TSymbolExpr(Self).Reference.NodeKind = nkConstant;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        Result := TBinaryExpr(Self).Right.IsConstant
      else
        Result := False;
  else
    Result := False;
  end;
end;}

{function TExpr.IsField: Boolean;
begin
  case OpCode of
    opSYMBOL: Result := TSymbolExpr(Self).Reference.NodeKind = nkField;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        Result := TBinaryExpr(Self).Right.IsField
      else
        Result := False;
  else
    Result := False;
  end;
end;}

function TExpr.IsFunction: Boolean;
begin
  Result := Self.GetFunctionSymbol <> nil;
end;

function TExpr.IsNilConst: Boolean;
begin
  Result := (Self.OpCode = opNIL);
end;

function TExpr.IsTypedConstant: Boolean;
var
  Ref: TSymbol;
begin
  Ref := Self.GetReference;
  Result := (Ref <> nil) and (Ref.NodeKind = nkVariable)
                and (vaReadOnly in TVariable(Ref).VarAttr)
end;

function TExpr.IsTypeSymbol: Boolean;
var
  Sym: TSymbol;
begin
  Sym := Self.GetReference;
  if Sym <> nil then
    Result := Sym.NodeKind = nkType
  else
    Result := False;
end;

{function TExpr.IsVariable: Boolean;
begin
  case OpCode of
    opSYMBOL: Result := TSymbolExpr(Self).Reference.NodeKind = nkVariable;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        Result := TBinaryExpr(Self).Right.IsVariable
      else
        Result := False;
    opINDEX:
      Result := TBinaryExpr(Self).Left.IsVariable;
    opINST:
      Result := TBinaryExpr(Self).Left.IsVariable;
    opCAST: begin
      Result := False;
      if TBinaryExpr(Self).Right <> nil then
        if TUnaryExpr(TBinaryExpr(Self).Right).Operand <> nil then
          Result := TUnaryExpr(TBinaryExpr(Self).Right).Operand.IsVariable;
    end;
  else
    Result := False;
  end;
end;}

procedure TExpr.Reset;
begin
  Self.Next := nil;
  Self.FParent := nil;
  Self.Attr := [];
  Self.Typ := nil;
end;

procedure TExpr.SetReference(Ref: TSymbol);
begin
  case OpCode of
    opSYMBOL: TSymbolExpr(Self).Reference := Ref;
    opMEMBER:
      if TBinaryExpr(Self).Right <> nil then
        TBinaryExpr(Self).Right.SetReference(Ref);
  end;
end;

{ TUnaryExpr }

procedure TUnaryExpr.Reset;
begin
  inherited;
  FOperand := nil;
end;

procedure TUnaryExpr.SetOperand(const Value: TExpr);
begin
  if FOperand = Value then Exit;
  if (Value <> nil) and (Value.Parent <> nil) then
    raise EASTError.Create('expr has in use');
  FOperand := Value;
  if FOperand <> nil then
    FOperand.FParent := Self;
end;

{ TBinaryExpr }

procedure TBinaryExpr.Reset;
begin
  inherited;
  FLeft := nil;
  FRight := nil;
end;

procedure TBinaryExpr.SetLeft(const Value: TExpr);
begin
  if Value = FLeft then Exit;

  if (Value <> nil) and (Value.Parent <> nil) then
    raise EASTError.Create('expr has in use');
  FLeft := Value;
  if FLeft <> nil then
    FLeft.FParent := Self;
end;

procedure TBinaryExpr.SetRight(const Value: TExpr);
begin
  if Value = FRight then Exit;
  if (Value <> nil) and (Value.Parent <> nil) then
    raise EASTError.Create('expr has in use');
  FRight := Value;
  if FRight <> nil then
    fRight.FParent := Self;
end;

{ TConstExpr }

destructor TConstExpr.Destroy;
begin
  ValClear(Value);
  inherited;
end;

procedure TConstExpr.Reset;
begin
  inherited;
  ValClear(Value);
end;

{ TSymbolExpr }

procedure TSymbolExpr.Reset;
begin
  inherited;
  Self.Reference := nil;
  Self.Name := '';
end;

{ TStrConstExpr }

procedure TStrConstExpr.Reset;
begin
  inherited;
  RawValue := '';
end;

{ TEmptyStmt }

constructor TEmptyStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skEmptyStmt;
end;

{ TCompoundStmt }

constructor TCompoundStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skCompoundStmt;
  Statements := TList.Create;
end;

destructor TCompoundStmt.Destroy;
begin
  Statements.Free;
  inherited;
end;

{ TCallStmt }

constructor TCallStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skCallStmt;
end;

{ TStmtLabel }

constructor TStmtLabel.Create;
begin
  inherited;
  FNodeKind := nkLabel;
end;

{ TAssignmentStmt }

constructor TAssignmentStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skAssignmentStmt;
end;

{ TIfStmt }

constructor TIfStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skIfStmt;
end;

{ TForStmt }

constructor TForStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skForStmt;
end;

{ TWhileStmt }

constructor TWhileStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skWhileStmt;
end;

{ TCaseSelector }

procedure TCaseSelector.AddRange(Start, Stop: Int64);
begin
  if FCount >= Length(Values) then
    SetLength(Values, Length(Values) + 4);
  Values[FCount].Start := Start;
  Values[FCount].Stop := Stop;
  Inc(FCount);
end;

procedure TCaseSelector.Clear;
begin
  FCount := 0;
end;

function TCaseSelector.Contains(Start, Stop: Int64): Boolean;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if ((Start >= Values[i].Start) and (Start <= Values[i].Stop))
      or ((Stop >= Values[i].Start) and (Stop <= Values[i].Stop)) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

destructor TCaseSelector.Destroy;
begin
  Clear;
  inherited;
end;

{ TCaseStmt }

procedure TCaseStmt.AddSelector(Selector: TCaseSelector);
begin
  if FCount >= Length(Selectors) then
    SetLength(Selectors, Length(Selectors) + 4);
  Selectors[FCount] := Selector;
  Inc(FCount);
end;

procedure TCaseStmt.Clear;
var
  I: Integer;
begin
  for I := 0 to High(Selectors) do
  begin
    Selectors[I].Free;
    Selectors[I] := nil;
  end;
end;

constructor TCaseStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skCaseStmt;
end;

destructor TCaseStmt.Destroy;
begin
  Clear;
  inherited;
end;

{ TRepeatStmt }

constructor TRepeatStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skRepeatStmt;
end;

{ TGotoStmt }

constructor TGotoStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skGotoStmt;
end;

{ TFunctionDecl }

procedure TFunctionDecl.AddOverload(Func: TFunctionDecl);
var
  F: TFunctionDecl;
begin
  F := Self;
  while F.NextOverload <> nil do
    F := F.NextOverload;
  F.NextOverload := Func;
end;

function TFunctionDecl.CountOfArgs: Integer;
begin
  if Args = nil then
    Result := 0 else
    Result := Args.Count;
end;

procedure TFunctionDecl.CreateArgs;
begin
  if Args = nil then Args := TList.Create;
end;

procedure TFunctionDecl.CreateProceduralType;
begin
// todo 1: 想法：这些附带类型都要有个名称，便于保存
  FProcType := TProceduralType.Create;
  if Self.Args <> nil then
  begin
    FProcType.CreateArgs;
    FProcType.Args.Assign(Self.Args);
  end;
  FProcType.ReturnType := Self.ReturnType;
  FProcType.CallConvention := Self.CallConvention;
  FProcType.Parent := Self.Parent;
  Include(FProcType.Attr, saTemp);
end;

destructor TFunctionDecl.Destroy;
begin
  Args.Free;
  FProcType.Free;
  inherited;
end;

function TFunctionDecl.GetProceduralType: TProceduralType;
begin
  if not Assigned(FProcType) then
    CreateProceduralType;
  Result := FProcType;
end;

function TFunctionDecl.IsOverload: Boolean;
begin
  Result := (NextOverload <> nil) and (fmOverload in Self.Modifiers);
end;

function TFunctionDecl.MinOfArgs: Integer;
var
  I: Integer;
begin
  Result := 0;
  if Args = nil then Exit;

  for I := 0 to Args.Count - 1 do
    if TArgument(Args[I]).DefaultValue.VT = vtEmpty then
      Inc(Result);
end;

{ TExternalFunction }

constructor TExternalFunction.Create;
begin
  inherited;
  FNodeKind := nkExternalFunc;
end;

{ TFunction }

procedure TFunction.Add(Sym: TSymbol);
begin
  LocalSymbols.Add(Sym);
end;

procedure TFunction.AddSymbol(Sym: TSymbol);
begin
  if not (Sym.NodeKind in [nkType, nkLabel, nkEnumElement,
      nkVariable, nkConstant, nkFunc, nkArgument]) then
    raise EASTError.Create('Node kind invalid for function');

  if Sym.Parent = nil then
    Sym.Parent := Self
  else if Sym.Parent <> Self then
    raise EASTError.CreateFmt(SErr_SymbolHasParent, [Sym.Name]);
end;

constructor TFunction.Create;
begin
  inherited;
  FNodeKind := nkFunc;
  LocalSymbols := TSymbolTable.Create(Self);
  LocalSymbols.Capacity := 16;
end;

destructor TFunction.Destroy;
begin
  LocalSymbols.Free;
  inherited;
end;

{ TMethod }

constructor TMethod.Create;
begin
  inherited;
  FNodeKind := nkMethod;
end;

procedure TMethod.CreateProceduralType;
begin
  inherited;
  FProcType.IsMethodPointer := not (fmStatic in Self.Modifiers);
  FProcType.MethodKind := Self.MethodKind;
  FProcType.ObjectKind := Self.ObjectKind;
end;

function TMethod.IsClassOrStatic: Boolean;
begin
  Result := (saStatic in Self.Attr) or
            (saClass in Self.Attr);
end;

{ TMethodResolution }

constructor TMethodResolution.Create;
begin
  inherited;
  FNodeKind := nkMethodResolution;
end;

{ TBuiltinFunction }

constructor TBuiltinFunction.Create;
begin
  inherited;
  FNodeKind := nkBuiltinFunc;
end;

{ TEnumValue }

constructor TEnumValue.Create;
begin
  inherited;
  FNodeKind := nkEnumElement;
end;

{ TVariable }

constructor TVariable.Create;
begin
  inherited;
  FNodeKind := nkVariable;
end;

destructor TVariable.Destroy;
begin
  ValClear(Value);
  inherited;
end;

{ TConstant }

constructor TConstant.Create;
begin
  inherited;
  FNodeKind := nkConstant;
end;

destructor TConstant.Destroy;
begin
  ValClear(Value);
  inherited;
end;

{ TSetValue }

class function TSetValue.Add(L, R: TSetValue): TSetValue;
var
  I: Integer;
begin
  Result := TSetValue.Create;
  Result.BitStart := Min(L.BitStart, R.BitStart);
  Result.BitCount := Max(L.BitCount, R.BitCount);
  for I := 0 to 31 do
    Result.Bits[I] := L.Bits[I] or R.Bits[I];
end;

procedure TSetValue.Assign(Source: TSetValue);
begin
  Self.BitStart := Source.BitStart;
  Self.BitCount := Source.BitCount;
  Move(Source.Bits, Self.Bits, SizeOf(Self.Bits));
end;

constructor TSetValue.Create;
begin
  BitStart := 255;
end;

function TSetValue.Equal(R: TSetValue): Boolean;
var
  I: Integer;
begin
  Result := (BitStart = R.BitStart) and (BitCount = R.BitCount);
  if Result then
    for I := 0 to 31 do
    begin
      Result := Bits[I] = Bits[I];
      if not Result then Exit;
    end;
end;

function TSetValue.Include(R: TSetValue): Boolean;
var
  I: Integer;
  L, H: Integer;
begin
  L := Min(BitStart, R.BitStart) div 8;
  H := Max(BitCount, R.BitCount) div 8;
  Result := False;
  for I := L to H do
  begin
    Result := (not Bits[I] and R.Bits[I]) = 0;
    if not Result then Exit;
  end;
end;

function TSetValue.MinSize: Integer;
begin
  Result := (BitStart + BitCount) div 8 - BitStart div 8 + 1;
  if Result = 3 then Result := 4;
end;

class function TSetValue.Mul(L, R: TSetValue): TSetValue;
var
  I: Integer;
begin
  Result := TSetValue.Create;
  Result.BitStart := Min(L.BitStart, R.BitStart);
  Result.BitCount := Max(L.BitCount, R.BitCount);
  for I := 0 to 31 do
    Result.Bits[I] := L.Bits[I] and R.Bits[I];
end;

procedure TSetValue.SetBits(Index: Byte; Value: Boolean);
var
  I, Offset: Integer;
begin
  I := Index div 8;
  Offset := Index mod 8;
  if Value then
    Bits[I] := Bits[I] or (1 shl Offset)
  else
    Bits[I] := Bits[I] and not (1 shl Offset);
end;

class function TSetValue.Sub(L, R: TSetValue): TSetValue;
var
  I: Integer;
begin
  Result := TSetValue.Create;
  Result.BitStart := Min(L.BitStart, R.BitStart);
  Result.BitCount := Max(L.BitCount, R.BitCount);
  for I := 0 to 31 do
    Result.Bits[I] := L.Bits[I] and (not R.Bits[I]);
end;

function TSetValue.TestBits(Index: Byte): Boolean;
var
  I, Offset: Integer;
begin
  I := Index div 8;
  Offset := Index mod 8;
  Result := (Bits[I] and (1 shl Offset)) <> 0;
end;

procedure TSetValue.Update;
const
  _bits: array[0..7] of Byte = (1, 2, 4, 8, 16, 32, 64, 128);

  function _bit_start(b: Byte): Integer;
  var
    I: Integer;
  begin
    for I := 0 to 7 do
      if b and _bits[I] <> 0 then
      begin
        Result := I;
        Exit;
      end;
    Result := 0;
  end;

  function _bit_start_rev(b: Byte): Integer;
  var
    I: Integer;
  begin
    for I := 7 downto 0 do
      if b and _bits[I] <> 0 then
      begin
        Result := I;
        Exit;
      end;
    Result := 0;
  end;
var
  I, H, L: Integer;
begin
  BitStart := 0;
  BitCount := 0;
  H := -1; L := -1;
  // 求下界
  for I := 0 to High(Bits) do
    if Bits[I] <> 0 then begin
      L := I;
      Break;
    end;
  if L = -1 then Exit;

  // 求上界
  for I := High(Bits) downto 0 do
    if Bits[I] <> 0 then begin
      H := I;
      Break;
    end;

  BitStart := L * 8 + _bit_start(Bits[L]);
  I := H * 8 + _bit_start_rev(Bits[H]);
  BitCount := I - BitStart + 1;
end;

{ TArrayValue }

procedure TArrayValue.Clear;
begin
  FreeMem(Items);
  Items := nil;
  DimCount := 0;
  Ranges := nil;
end;

destructor TArrayValue.Destroy;
begin
  Clear;
  inherited;
end;

{ TRecordValue }

procedure TRecordValue.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(Items) - 1 do
    ValClear(Items[I]);
end;

destructor TRecordValue.Destroy;
begin
  Clear;
  inherited;
end;

{ TArgument }

constructor TArgument.Create;
begin
  inherited;
  FNodeKind := nkArgument;
end;

destructor TArgument.Destroy;
begin
  ValClear(DefaultValue);
  inherited;
end;

{function TArgument.GetArgKind: TArgumentKind;
begin
  if ArgType.TypeCode = typOpenArray then
  begin
    if TOpenArrayType(ArgType).ElementType = nil then
      Result := akArrayOfConst
    else
      Result := akArrayOfType
  end
  else if ArgType.TypeCode = typUntype then
    Result := akUntype
  else
    Result := akNormal;
end;}

function TArgument.IsReadOnly: Boolean;
begin
  Result := argConst = Modifier;
end;

{ TExceptBlock }

procedure TExceptBlock.AddExceptHandler(Block: TExceptHandler);
begin
  if FCount >= Length(ExceptHandlers) then
    SetLength(ExceptHandlers, FCount + 4);
  ExceptHandlers[FCount] := Block;
  Inc(FCount);
end;

procedure TExceptBlock.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    Self.ExceptHandlers[I].Free;
  Self.ExceptHandlers := nil;
end;

destructor TExceptBlock.Destroy;
begin
  Clear;
  inherited;
end;

{ TTryStmt }

constructor TTryStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skTryStmt;
end;

destructor TTryStmt.Destroy;
begin
  ExceptBlock.Free;
  inherited;
end;

function TTryStmt.IsFinallyOrExcept(S: TStatement): Boolean;
var
  i: Integer;
begin
  Result := Self.FinallyStmt = S;
  if not Result and Assigned(ExceptBlock) then
  begin
    for i := 0 to ExceptBlock.Count - 1 do
      if ExceptBlock.ExceptHandlers[i].Stmt = S then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

{ TRaiseStmt }

constructor TRaiseStmt.Create;
begin
  inherited;
  FNodeKind := nkStmt;
  FStmtKind := skRaiseStmt;
end;

{ TSymbolTable }

function TSymbolTable.Add(Sym: TSymbol): Boolean;
var
  hc: Cardinal;
begin
  hc := HashOf(Sym.Name);
  Result := inherited IsExists(Sym.Name, hc);
  inherited Put(Sym.Name, hc, Sym);
  if FAutoAddToOwner and (Owner <> nil) then
    Owner.AddSymbol(Sym);
end;

procedure TSymbolTable.Clear(FreeSym: Boolean);
var
  i: Integer;
begin
  if FreeSym then
    for i := 0 to Count - 1 do
      Self.Item[i].Free;
  inherited Clear;
end;

constructor TSymbolTable.Create(AOwner: TSymbol);
begin
  inherited Create(0, False);
  FOwner := AOwner;
  FAutoAddToOwner := True;
end;

function TSymbolTable.Find(M: TModule; const S: string): TSymbol;
var
  P: PPHashItem;
begin
  if M = nil then
  begin
    Result := Find(S);
    Exit;
  end;

  P := inherited Lookup(S);
  while P^ <> nil do
  begin
    if (TSymbol(P^.Value).Module = M) and SameText(P^.Key, S) then
    begin
      Result := TSymbol(P^.Value);
      Exit;
    end
    else
      P := @P^.Next;
  end;
  Result := nil;
end;

function TSymbolTable.Find(const S: string): TSymbol;
begin
  Result := TSymbol(inherited Get(S));
end;

function TSymbolTable.GetNext(var Pos: TSymbolPosition): TSymbol;
var
  Item, Old: PPHashItem;
begin
  Item := Pos;

  if Item^ = nil then
    Result := nil
  else
    Result := TSymbol(Item^.Value);

  Old := Item;
	while Item^ <> nil do
	begin
    if CaseSensitive then
    begin
      if Item^.Key = Old^.Key then
        Exit
      else
        Item := @Item^.Next;
    end
    else
    begin
      if SameText(Item^.Key, Old^.Key) then
        Exit
      else
        Item := @Item^.Next;
    end;
	end;
end;

function TSymbolTable.GetStart(const S: string): TSymbolPosition;
begin
  Result := Self.Lookup(S);
end;

function TSymbolTable.GetSymbol(Index: Integer): TSymbol;
begin
  Result := TSymbol(inherited ValueByIndex(Index));
end;

initialization
  InitDirectives;
finalization
  _Directives.Free;
  _Directives := nil;
end.
