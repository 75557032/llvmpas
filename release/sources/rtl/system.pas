unit System;

interface

const
  RTLVersion = 15.00;

{ Variant type codes (wtypes.h) }

  Maxlongint = $7fffffff;

  varEmpty    = $0000; { vt_empty        0 }
  varNull     = $0001; { vt_null         1 }
  varSmallint = $0002; { vt_i2           2 }
  varInteger  = $0003; { vt_i4           3 }
  varSingle   = $0004; { vt_r4           4 }
  varDouble   = $0005; { vt_r8           5 }
  varCurrency = $0006; { vt_cy           6 }
  varDate     = $0007; { vt_date         7 }
  varOleStr   = $0008; { vt_bstr         8 }
  varDispatch = $0009; { vt_dispatch     9 }
  varError    = $000A; { vt_error       10 }
  varBoolean  = $000B; { vt_bool        11 }
  varVariant  = $000C; { vt_variant     12 }
  varUnknown  = $000D; { vt_unknown     13 }
//varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
  varShortInt = $0010; { vt_i1          16 }
  varByte     = $0011; { vt_ui1         17 }
  varWord     = $0012; { vt_ui2         18 }
  varLongWord = $0013; { vt_ui4         19 }
  varInt64    = $0014; { vt_i8          20 }
//varWord64   = $0015; { vt_ui8         21 } {UNSUPPORTED as of v6.x code base}
{  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

  varStrArg   = $0048; { vt_clsid       72 }
  varString   = $0100; { Pascal string 256 } {not OLE compatible }
  varAny      = $0101; { Corba any     257 } {not OLE compatible }
  // custom types range from $110 (272) to $7FF (2047)

  varTypeMask = $0FFF;
  varArray    = $2000;
  varByRef    = $4000;

{ TVarRec.VType values }

  vtInteger    = 0;
  vtBoolean    = 1;
  vtChar       = 2;
  vtExtended   = 3;
  vtString     = 4;
  vtPointer    = 5;
  vtPChar      = 6;
  vtObject     = 7;
  vtClass      = 8;
  vtWideChar   = 9;
  vtPWideChar  = 10;
  vtAnsiString = 11;
  vtCurrency   = 12;
  vtVariant    = 13;
  vtInterface  = 14;
  vtWideString = 15;
  vtInt64      = 16;

{ Virtual method table entries }

  vmtSelfPtr           = -76;
  vmtIntfTable         = -72;
  vmtAutoTable         = -68;
  vmtInitTable         = -64;
  vmtTypeInfo          = -60;
  vmtFieldTable        = -56;
  vmtMethodTable       = -52;
  vmtDynamicTable      = -48;
  vmtClassName         = -44;
  vmtInstanceSize      = -40;
  vmtParent            = -36;
  vmtSafeCallException = -32 deprecated;  // don't use these constants.
  vmtAfterConstruction = -28 deprecated;  // use VMTOFFSET in asm code instead
  vmtBeforeDestruction = -24 deprecated;
  vmtDispatch          = -20 deprecated;
  vmtDefaultHandler    = -16 deprecated;
  vmtNewInstance       = -12 deprecated;
  vmtFreeInstance      = -8 deprecated;
  vmtDestroy           = -4 deprecated;

  vmtQueryInterface    = 0 deprecated;
  vmtAddRef            = 4 deprecated;
  vmtRelease           = 8 deprecated;
  vmtCreateObject      = 12 deprecated;

type
  TObject = class;

  TClass = class of TObject;

  HRESULT = type Longint;  { from WTYPES.H }

  PGUID = ^TGUID;
  TGUID = packed record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
  end;

(* new
  TInterfaceEntry = record
    IID: PGUID;
    VTable: Pointer;
    IOffset: Pointer;
    IType: Longint;
  {$ifdef cpu64}
    __pad: LongWord;
  {$endif}
  end;
*)
  PInterfaceEntry = ^TInterfaceEntry;
  TInterfaceEntry = packed record
    IID: TGUID;
    VTable: Pointer;
    IOffset: Integer;
    ImplGetter: Integer;
  end;

  PInterfaceTable = ^TInterfaceTable;
  TInterfaceTable = packed record
    EntryCount: Integer;
    Entries: array[0..9999] of TInterfaceEntry;
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

{ TObject.Dispatch accepts any data type as its Message parameter.  The
  first 2 bytes of the data are taken as the message id to search for
  in the object's message methods.  TDispatchMessage is an example of
  such a structure with a word field for the message id.
}
  TDispatchMessage = record
    MsgID: Word;
  end;

  TObject = class
    constructor Create;
    procedure Free;
    class function InitInstance(Instance: Pointer): TObject;
    procedure CleanupInstance;
    function ClassType: TClass;
    class function ClassName: ShortString;
    class function ClassNameIs(const Name: string): Boolean;
    class function ClassParent: TClass;
    class function ClassInfo: Pointer;
    class function InstanceSize: Longint;
    class function InheritsFrom(AClass: TClass): Boolean;
    class function MethodAddress(const Name: ShortString): Pointer;
    class function MethodName(Address: Pointer): ShortString;
    function FieldAddress(const Name: ShortString): Pointer;
    function GetInterface(const IID: TGUID; out Obj): Boolean;
    class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    class function GetInterfaceTable: PInterfaceTable;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; virtual;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
    procedure Dispatch(var Message); virtual;
    procedure DefaultHandler(var Message); virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    destructor Destroy; virtual;
  end;

const
  S_OK = 0;
  S_FALSE = $00000001;
  E_NOINTERFACE = HRESULT($80004002);
  E_UNEXPECTED = HRESULT($8000FFFF);
  E_NOTIMPL = HRESULT($80004001);

type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  IUnknown = IInterface;
{$M+}
  IInvokable = interface(IInterface)
  end;
{$M-}

  IDispatch = interface(IUnknown)
    ['{00020400-0000-0000-C000-000000000046}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;
{
  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  TInterfacedClass = class of TInterfacedObject;}

  PShortString = ^ShortString;
  PAnsiString = ^AnsiString;
  PWideString = ^WideString;
  PString = PAnsiString;

  UCS2Char = WideChar;
  PUCS2Char = PWideChar;
  UCS4Char = type LongWord;
  PUCS4Char = ^UCS4Char;
  TUCS4CharArray = array [0..$effffff] of UCS4Char;
  PUCS4CharArray = ^TUCS4CharArray;
  UCS4String = array of UCS4Char;

  UTF8String = type string;
  PUTF8String = ^UTF8String;

  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  PointerArray = array [0..512*1024*1024 - 2] of Pointer;
  PPointerArray = ^PointerArray;
  TBoundArray = array of Integer;
  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;

  PLongint      = ^Longint;
  PInteger      = ^Integer;
  PCardinal     = ^Cardinal;
  PWord         = ^Word;
  PSmallInt     = ^SmallInt;
  PByte         = ^Byte;
  PShortInt     = ^ShortInt;
  PInt64        = ^Int64;
  PLongWord     = ^LongWord;
  PSingle       = ^Single;
  PDouble       = ^Double;
  PDate         = ^Double;
  PDispatch     = ^IDispatch;
  PPDispatch    = ^PDispatch;
  PError        = ^LongWord;
  PWordBool     = ^WordBool;
  PUnknown      = ^IUnknown;
  PPUnknown     = ^PUnknown;
  PPWideChar    = ^PWideChar;
  PPChar        = ^PChar;
  PPAnsiChar    = PPChar;
  PExtended     = ^Extended;
  PComp         = ^Comp;
  PCurrency     = ^Currency;
  PVariant      = ^Variant;
  POleVariant   = ^OleVariant;
  PPointer      = ^Pointer;
  PBoolean      = ^Boolean;

  TDateTime = type Double;
  PDateTime = ^TDateTime;

  THandle = LongWord;

  TVarArrayBound = packed record
    ElementCount: Integer;
    LowBound: Integer;
  end;
  TVarArrayBoundArray = array [0..0] of TVarArrayBound;
  PVarArrayBoundArray = ^TVarArrayBoundArray;
  TVarArrayCoorArray = array [0..0] of Integer;
  PVarArrayCoorArray = ^TVarArrayCoorArray;

  PVarArray = ^TVarArray;
  TVarArray = packed record
    DimCount: Word;
    Flags: Word;
    ElementSize: Integer;
    LockCount: Integer;
    Data: Pointer;
    Bounds: TVarArrayBoundArray;
  end;

  TVarType = Word;
  PVarData = ^TVarData;
  TVarData = packed record
    case Integer of
      0: (VType: TVarType;
          case Integer of
            0: (Reserved1: Word;
                case Integer of
                  0: (Reserved2, Reserved3: Word;
                      case Integer of
                        varSmallInt: (VSmallInt: SmallInt);
                        varInteger:  (VInteger: Integer);
                        varSingle:   (VSingle: Single);
                        varDouble:   (VDouble: Double);
                        varCurrency: (VCurrency: Currency);
                        varDate:     (VDate: TDateTime);
                        varOleStr:   (VOleStr: PWideChar);
                        varDispatch: (VDispatch: Pointer);
                        varError:    (VError: HRESULT);
                        varBoolean:  (VBoolean: WordBool);
                        varUnknown:  (VUnknown: Pointer);
                        varShortInt: (VShortInt: ShortInt);
                        varByte:     (VByte: Byte);
                        varWord:     (VWord: Word);
                        varLongWord: (VLongWord: LongWord);
                        varInt64:    (VInt64: Int64);
                        varString:   (VString: Pointer);
                        varAny:      (VAny: Pointer);
                        varArray:    (VArray: PVarArray);
                        varByRef:    (VPointer: Pointer);
                     );
                  1: (VLongs: array[0..2] of LongInt);
               );
            2: (VWords: array [0..6] of Word);
            3: (VBytes: array [0..13] of Byte);
          );
      1: (RawData: array [0..3] of LongInt);
  end;

type
  TVarOp = Integer;

const
  opAdd =        0;
  opSubtract =   1;
  opMultiply =   2;
  opDivide =     3;
  opIntDivide =  4;
  opModulus =    5;
  opShiftLeft =  6;
  opShiftRight = 7;
  opAnd =        8;
  opOr =         9;
  opXor =        10;
  opCompare =    11;
  opNegate =     12;
  opNot =        13;

  opCmpEQ =      14;
  opCmpNE =      15;
  opCmpLT =      16;
  opCmpLE =      17;
  opCmpGT =      18;
  opCmpGE =      19;

type
  PVarRec = ^TVarRec;
  TVarRec = record { do not pack this record; it is compiler-generated }
    case Byte of
      vtInteger:    (VInteger: Integer; VType: Byte);
      vtBoolean:    (VBoolean: Boolean);
      vtChar:       (VChar: Char);
      vtExtended:   (VExtended: PExtended);
      vtString:     (VString: PShortString);
      vtPointer:    (VPointer: Pointer);
      vtPChar:      (VPChar: PChar);
      vtObject:     (VObject: TObject);
      vtClass:      (VClass: TClass);
      vtWideChar:   (VWideChar: WideChar);
      vtPWideChar:  (VPWideChar: PWideChar);
      vtAnsiString: (VAnsiString: Pointer);
      vtCurrency:   (VCurrency: PCurrency);
      vtVariant:    (VVariant: PVariant);
      vtInterface:  (VInterface: Pointer);
      vtWideString: (VWideString: Pointer);
      vtInt64:      (VInt64: PInt64);
  end;

procedure FillChar(var Dest; Count: NativeInt; Value: AnsiChar); external;
procedure Move(const Source; var Dest; Count: NativeInt); external;

function InterLockedIncrement(var dest: Integer): Integer; external;
function InterLockedIncrement64(var dest: Int64): Int64; external;
function InterLockedDecrement(var dest: Integer): Integer; external;
function InterLockedDecrement64(var dest: Int64): Int64; external;
function InterLockedCompareExchange(var dest: Integer; exchange, comparand: Integer): Integer; external;
function InterLockedCompareExchange64(var dest: Int64; exchange, comparand: Int64): Int64; external;
function InterLockedExchange(var dest: Integer; value: Integer): Integer; external;
function InterLockedExchange64(var dest: Int64; value: Int64): Int64; external;
function InterLockedExchangeAdd(var dest: Integer; value: Integer): Integer; external;
function InterLockedExchangeAdd64(var dest: Int64; value: Int64): Int64; external;

function _GetMem(Size: Integer): Pointer;
procedure _FreeMem(P: Pointer);

// -------------------------
procedure _IntOverflow;
procedure _OutOfRange;
procedure _IOCheck;
procedure _SafecallCheck(hr: Integer);
procedure _RaiseExcept(exobj: TObject); external;
procedure _Rethrow(exobj: TObject); external;
procedure _FreeExceptObject(var exobj: TObject); external;
procedure _HandleSafecallExcept(Instance, ExceptObject: TObject); external;
function _InternalHandleSafecall(Instance, ExceptObject: TObject): Integer;
procedure _HandleCtorExcept(E: Pointer; Instance: TObject; Flag: Shortint); external;
procedure _HandleFinally(ExceptObject: TObject); external;
procedure _Terminated;

function _IsClass(Child: TObject; Parent: TClass): Boolean;
function _AsClass(Child: TObject; Parent: TClass): TObject;
function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;
function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;

// -------------------------
function _Int64Div(a, b: Int64): Int64; external;
function _Int64Mod(a, b: Int64): Int64; external;
function _UInt64Div(a, b: UInt64): UInt64; external;
function _UInt64Mod(a, b: UInt64): UInt64; external;

function _Round(v: Double): Int64;
function _Trunc(v: Double): Int64;

// -------------------------
//procedure _Write0Int(F: Pointer; Value: Integer);
//procedure _Write0AStr(F: Pointer; const S: AnsiString);
//procedure _Write0PaStr(F: Pointer; const S: PAnsiChar);

// -------------------------
procedure _AStrClr(var S: AnsiString);
procedure _AStrAddRef(var S: AnsiString);
procedure _AStrNew(var S: AnsiString; Count: Integer);
function _AStrPtr(const S: AnsiString): PAnsiChar;
function _AStrLength(const S: AnsiString): Integer;
procedure _AStrAsgCopy(var Dest: AnsiString; const Source: AnsiString);
procedure _AStrAsg(var Dest: AnsiString; const Source: AnsiString);
procedure _AStrSetLength(var S: AnsiString; Len: Integer);
procedure _AStrCopy(var Dest: AnsiString; const Source: AnsiString; Start, Count: Integer);
procedure _AStrDelete(var Dest: AnsiString; Index, Count: Integer);
procedure _AStrInsert(var Dest: AnsiString; const Source: AnsiString; Index: Integer);
procedure _AStrFromSStr(var Dest: AnsiString; const Source: ShortString);
procedure _AStrFromWStr(var Dest: AnsiString; const Source: WideString);
procedure _AStrFromUStr(var Dest: AnsiString; const Source: UnicodeString);
procedure _AStrFromACh(var Dest: AnsiString; ACh: AnsiChar);
procedure _AStrFromWCh(var Dest: AnsiString; WCh: WideChar);
procedure _AStrFromPACh(var Dest: AnsiString; Buf: PAnsiChar);
procedure _AStrFromPAChLen(var Dest: AnsiString; Buf: PAnsiChar; Count: Integer);
procedure _AStrFromPWCh(var Dest: AnsiString; Buf: PWideChar);
procedure _AStrFromPWChLen(var Dest: AnsiString; Buf: PWideChar; Count: Integer);
procedure _AStrFromAArray(var Dest: AnsiString; Buf: PAnsiChar; MaxChars: Integer);
procedure _AStrFromWArray(var Dest: AnsiString; Buf: PWideChar; MaxChars: Integer);
procedure _AStrCat(var S1: AnsiString; const S2: AnsiString);
procedure _AStrCat3(var S1: AnsiString; const S2, S3: AnsiString);
procedure _AStrCatN(var S1: AnsiString; const Source: array of AnsiString);
// -------------------------

procedure _WStrClr(var S: WideString);
procedure _WStrNew(var S: WideString; Count: Integer);
function _WStrPtr(const S: WideString): PWideChar;
function _WStrLength(const S: WideString): Integer;
procedure _WStrAsgCopy(var Dest: WideString; const Source: WideString);
procedure _WStrAsg(var Dest: WideString; const Source: WideString);
procedure _WStrSetLength(var S: WideString; Len: Integer);
procedure _WStrCopy(var Dest: WideString; const Source: WideString; Start, Count: Integer);
procedure _WStrDelete(var Dest: WideString; Index, Count: Integer);
procedure _WStrInsert(var Dest: WideString; const Source: WideString; Index: Integer);
procedure _WStrFromSStr(var Dest: WideString; const Source: ShortString);
procedure _WStrFromAStr(var Dest: WideString; const Source: AnsiString);
procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
procedure _WStrFromACh(var Dest: WideString; ACh: AnsiChar);
procedure _WStrFromWCh(var Dest: WideString; WCh: WideChar);
procedure _WStrFromPACh(var Dest: WideString; Buf: PAnsiChar);
procedure _WStrFromPAChLen(var Dest: WideString; Buf: PAnsiChar; Count: Integer);
procedure _WStrFromPWCh(var Dest: WideString; Buf: PWideChar);
procedure _WStrFromPWChLen(var Dest: WideString; Buf: PWideChar; Count: Integer);
procedure _WStrFromAArray(var Dest: WideString; Buf: PAnsiChar; MaxChars: Integer);
procedure _WStrFromWArray(var Dest: WideString; Buf: PWideChar; MaxChars: Integer);
procedure _WStrCat(var S1: WideString; const S2: WideString);
procedure _WStrCat3(var S1: WideString; const S2, S3: WideString);
procedure _WStrCatN(var S1: WideString; const Source: array of WideString);
// -------------------------
procedure _UStrClr(var S: UnicodeString);
procedure _UStrNew(var S: UnicodeString; Count: Integer);
function _UStrPtr(const S: UnicodeString): PWideChar;
function _UStrLength(const S: UnicodeString): Integer;
procedure _UStrAsgCopy(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrSetLength(var S: UnicodeString; Len: Integer);
procedure _UStrCopy(var Dest: UnicodeString; const Source: UnicodeString; Start, Count: Integer);
procedure _UStrDelete(var Dest: UnicodeString; Index, Count: Integer);
procedure _UStrInsert(var Dest: UnicodeString; const Source: UnicodeString; Index: Integer);
procedure _UStrFromSStr(var Dest: UnicodeString; const Source: ShortString);
procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
procedure _UStrFromAStr(var Dest: UnicodeString; const Source: AnsiString);
procedure _UStrFromACh(var Dest: UnicodeString; ACh: AnsiChar);
procedure _UStrFromWCh(var Dest: UnicodeString; WCh: WideChar);
procedure _UStrFromPACh(var Dest: UnicodeString; Buf: PAnsiChar);
procedure _UStrFromPAChLen(var Dest: UnicodeString; Buf: PAnsiChar; Count: Integer);
procedure _UStrFromPWCh(var Dest: UnicodeString; Buf: PWideChar);
procedure _UStrFromPWChLen(var Dest: UnicodeString; Buf: PWideChar; Count: Integer);
procedure _UStrFromAArray(var Dest: UnicodeString; Buf: PAnsiChar; MaxChars: Integer);
procedure _UStrFromWArray(var Dest: UnicodeString; Buf: PWideChar; MaxChars: Integer);
procedure _UStrCat(var S1: UnicodeString; const S2: UnicodeString);
procedure _UStrCat3(var S1: UnicodeString; const S2, S3: UnicodeString);
procedure _UStrCatN(var S1: UnicodeString; const Source: array of UnicodeString);
// ----------------------
procedure _SStrClr(var Dest: ShortString);
function _StrLength(const Dest: ShortString): Integer;
procedure _SStrAsg(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
procedure _SStrSetLength(var Dest: ShortString; Len: Integer);
procedure _SStrCopy(var Dest: ShortString; MaxChars: Integer; const Source: ShortString; Start, Count: Integer);
procedure _SStrDelete(var Dest: ShortString; Index, Count: Integer);
procedure _SStrInsertS(var Dest: ShortString; MaxChars: Integer; const Source: ShortString; Index: Integer);
procedure _SStrInsertA(var Dest: ShortString; MaxChars: Integer; const Source: AnsiString; Index: Integer);
procedure _SStrInsertW(var Dest: ShortString; MaxChars: Integer; const Source: WideString; Index: Integer);
procedure _SStrInsertU(var Dest: ShortString; MaxChars: Integer; const Source: UnicodeString; Index: Integer);
procedure _SStrFromSStr(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
procedure _SStrFromAStr(var Dest: ShortString; MaxChars: Integer; const Source: WideString);
procedure _SStrFromWStr(var Dest: ShortString; MaxChars: Integer; const Source: WideString);
procedure _SStrFromUStr(var Dest: ShortString; MaxChars: Integer; const Source: UnicodeString);
procedure _SStrFromACh(var Dest: ShortString; MaxChars: Integer; ACh: AnsiChar);
procedure _SStrFromWCh(var Dest: ShortString; MaxChars: Integer; WCh: WideChar);
procedure _SStrFromPACh(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar);
procedure _SStrFromPAChLen(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar; Count: Integer);
procedure _SStrFromPWCh(var Dest: ShortString; Buf: PWideChar; MaxChars: Integer);
procedure _SStrFromPWChLen(var Dest: ShortString; MaxChars: Integer; Buf: PWideChar; Count: Integer);
procedure _SStrFromAArray(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar; Count: Integer);
procedure _SStrFromWArray(var Dest: ShortString; MaxChars: Integer; Buf: PWideChar; Count: Integer);
procedure _SStrCat(var S1: ShortString; MaxChars: Integer; const S2: ShortString);
procedure _SStrCat3(var S1: ShortString; MaxChars: Integer; const S2, S3: ShortString);
procedure _SStrCatN(var S1: ShortString; MaxChars: Integer; const Source: array of ShortString);

//--- String Compare -----
function _AStrComp(const S1, S2: AnsiString): Integer;
function _WStrComp(const S1, S2: WideString): Integer;
function _UStrComp(const S1, S2: UnicodeString): Integer;
function _SStrComp(const S1, S2: ShortString): Integer;
function _AarrComp(const S1: PAnsiChar; MaxChars1: Integer; S2: PAnsiChar; MaxChars2: Integer): Integer;
function _WarrComp(const S1: PWideChar; MaxChars1: Integer; S2: PWideChar; MaxChars2: Integer): Integer;

function _AStrCompWStr(const S1: AnsiString; const S2: WideString): Integer;
function _AStrCompUStr(const S1: AnsiString; const S2: UnicodeString): Integer;
function _AStrCompSStr(const S1: AnsiString; const S2: ShortString): Integer;
//function _AStrCompSWStr(const S1: AnsiString; const S2: ShortString): Integer;
function _AStrCompPa(const S1: AnsiString; S2: PAnsiChar): Integer;
function _AStrCompPw(const S1: AnsiString; S2: PWideChar): Integer;
function _AStrCompAarr(const S1: AnsiString; S2: PAnsiChar; MaxChars: Integer): Integer;
function _AStrCompWarr(const S1: AnsiString; S2: PWideChar; MaxChars: Integer): Integer;
function _AStrCompACh(const S1: AnsiString; Ch: AnsiChar): Integer;
function _AStrCompWCh(const S1: AnsiString; Ch: WideChar): Integer;

function _WStrCompAStr(const S1: WideString; const S2: AnsiString): Integer;
function _WStrCompUStr(const S1: WideString; const S2: UnicodeString): Integer;
function _WStrCompSStr(const S1: WideString; const S2: ShortString): Integer;
//function _WStrCompSWStr(const S1: WideString; const S2: ShortString): Integer;
function _WStrCompPa(const S1: WideString; S2: PAnsiChar): Integer;
function _WStrCompPw(const S1: WideString; S2: PWideChar): Integer;
function _WStrCompAarr(const S1: WideString; S2: PAnsiChar; MaxChars: Integer): Integer;
function _WStrCompWarr(const S1: WideString; S2: PWideChar; MaxChars: Integer): Integer;
function _WStrCompACh(const S1: WideString; Ch: AnsiChar): Integer;
function _WStrCompWCh(const S1: WideString; Ch: WideChar): Integer;

function _UStrCompAStr(const S1: UnicodeString; const S2: AnsiString): Integer;
function _UStrCompWStr(const S1: UnicodeString; const S2: WideString): Integer;
function _UStrCompSStr(const S1: UnicodeString; const S2: ShortString): Integer;
//function _UStrCompSWStr(const S1: UnicodeString; const S2: ShortString): Integer;
function _UStrCompPa(const S1: UnicodeString; S2: PAnsiChar): Integer;
function _UStrCompPw(const S1: UnicodeString; S2: PWideChar): Integer;
function _UStrCompAarr(const S1: UnicodeString; S2: PAnsiChar; MaxChars: Integer): Integer;
function _UStrCompWarr(const S1: UnicodeString; S2: PWideChar; MaxChars: Integer): Integer;
function _UStrCompACh(const S1: UnicodeString; Ch: AnsiChar): Integer;
function _UStrCompWCh(const S1: UnicodeString; Ch: WideChar): Integer;

function _SStrCompAStr(const S1: ShortString; const S2: AnsiString): Integer;
function _SStrCompWStr(const S1: ShortString; const S2: WideString): Integer;
function _SStrCompUStr(const S1: ShortString; const S2: UnicodeString): Integer;
//function _SStrCompSWStr(const S1: ShortString; const S2: ShortString): Integer;
function _SStrCompPa(const S1: ShortString; S2: PAnsiChar): Integer;
function _SStrCompPw(const S1: ShortString; S2: PWideChar): Integer;
function _SStrCompAarr(const S1: ShortString; S2: PAnsiChar; MaxChars: Integer): Integer;
function _SStrCompWarr(const S1: ShortString; S2: PWideChar; MaxChars: Integer): Integer;
function _SStrCompACh(const S1: ShortString; Ch: AnsiChar): Integer;
function _SStrCompWCh(const S1: ShortString; Ch: WideChar): Integer;

function _PaCompAStr(const S1: PAnsiChar; const S2: AnsiString): Integer;
function _PaCompWStr(const S1: PAnsiChar; const S2: WideString): Integer;
function _PaCompUStr(const S1: PAnsiChar; const S2: UnicodeString): Integer;
//function _PaCompSWStr(const S1: PAnsiChar; const S2: ShortString): Integer;
function _PaCompAarr(const S1: PAnsiChar; S2: PAnsiChar; MaxChars: Integer): Integer;
function _PaCompWarr(const S1: PAnsiChar; S2: PWideChar; MaxChars: Integer): Integer;
function _PaCompACh(const S1: PAnsiChar; Ch: AnsiChar): Integer;
function _PaCompWCh(const S1: PAnsiChar; Ch: WideChar): Integer;

function _PwCompAStr(const S1: PWideChar; const S2: AnsiString): Integer;
function _PwCompWStr(const S1: PWideChar; const S2: WideString): Integer;
function _PwCompUStr(const S1: PWideChar; const S2: UnicodeString): Integer;
//function _PaCompSWStr(const S1: PWideChar; const S2: ShortString): Integer;
function _PwCompAarr(const S1: PWideChar; S2: PAnsiChar; MaxChars: Integer): Integer;
function _PwCompWarr(const S1: PWideChar; S2: PWideChar; MaxChars: Integer): Integer;
function _PwCompACh(const S1: PWideChar; Ch: AnsiChar): Integer;
function _PwCompWCh(const S1: PWideChar; Ch: WideChar): Integer;

function _AarrCompAStr(const S1: PAnsiChar; MaxChars: Integer; const S2: AnsiString): Integer;
function _AarrCompWStr(const S1: PAnsiChar; MaxChars: Integer; const S2: WideString): Integer;
function _AarrCompUStr(const S1: PAnsiChar; MaxChars: Integer; const S2: UnicodeString): Integer;
//function _AarrCompSWStr(const S1: PAnsiChar; MaxChars: Integer; const S2: ShortString): Integer;
function _AarrCompPa(const S1: PAnsiChar; MaxChars: Integer; S2: PAnsiChar): Integer;
function _AarrCompPw(const S1: PAnsiChar; MaxChars: Integer; S2: PWideChar): Integer;
function _AarrCompWarr(const S1: PAnsiChar; MaxChars1: Integer; S2: PWideChar; MaxChars2: Integer): Integer;
function _AarrCompACh(const S1: PAnsiChar; MaxChars: Integer; Ch: AnsiChar): Integer;
function _AarrCompWCh(const S1: PAnsiChar; MaxChars: Integer; Ch: WideChar): Integer;

function _WarrCompAStr(const S1: PWideChar; MaxChars: Integer; const S2: AnsiString): Integer;
function _WarrCompWStr(const S1: PWideChar; MaxChars: Integer; const S2: WideString): Integer;
function _WarrCompUStr(const S1: PWideChar; MaxChars: Integer; const S2: UnicodeString): Integer;
//function _WarrCompSWStr(const S1: PWideChar; MaxChars: Integer; const S2: ShortString): Integer;
function _WarrCompPa(const S1: PWideChar; MaxChars: Integer; S2: PAnsiChar): Integer;
function _WarrCompPw(const S1: PWideChar; MaxChars: Integer; S2: PWideChar): Integer;
function _WarrCompAarr(const S1: PWideChar; MaxChars1: Integer; S2: PAnsiChar; MaxChars2: Integer): Integer;
function _WarrCompACh(const S1: PWideChar; MaxChars: Integer; Ch: AnsiChar): Integer;
function _WarrCompWCh(const S1: PWideChar; MaxChars: Integer; Ch: WideChar): Integer;

function _AChCompAStr(const S1: AnsiChar; const S2: AnsiString): Integer;
function _AChCompUStr(const S1: AnsiChar; const S2: UnicodeString): Integer;
function _AChCompSStr(const S1: AnsiChar; const S2: ShortString): Integer;
//function _AChCompSWStr(const S1: AnsiChar; const S2: ShortString): Integer;
function _AChCompPa(const S1: AnsiChar; S2: PAnsiChar): Integer;
function _AChCompPw(const S1: AnsiChar; S2: PWideChar): Integer;
function _AChCompAarr(const S1: AnsiChar; S2: PAnsiChar; MaxChars: Integer): Integer;
function _AChCompWarr(const S1: AnsiChar; S2: PWideChar; MaxChars: Integer): Integer;

function _WChCompAStr(const S1: WideChar; const S2: AnsiString): Integer;
function _WChCompUStr(const S1: WideChar; const S2: UnicodeString): Integer;
function _WChCompSStr(const S1: WideChar; const S2: ShortString): Integer;
//function _WChCompSWStr(const S1: WideChar; const S2: ShortString): Integer;
function _WChCompPa(const S1: WideChar; S2: PAnsiChar): Integer;
function _WChCompPw(const S1: WideChar; S2: PWideChar): Integer;
function _WChCompAarr(const S1: WideChar; S2: PAnsiChar; MaxChars: Integer): Integer;
function _WChCompWarr(const S1: WideChar; S2: PWideChar; MaxChars: Integer): Integer;

// -------------------------
procedure _VarClr(var V: Variant);
procedure _VarOp(var V1: Variant; const V2: Variant; Op: TVarOp);
procedure _VarNot(var V: Variant);
procedure _VarNeg(var V: Variant);
procedure _VarCopy(var Dest: Variant; const Source: Variant);
procedure _Var2AStr(const V: Variant; var Dest: AnsiString);
procedure _Var2WStr(const V: Variant; var Dest: WideString);
procedure _Var2UStr(const V: Variant; var Dest: UnicodeString);
procedure _Var2SStr(const V: Variant; var Dest: ShortString; MaxChars: Integer);
procedure _Var2Intf(const V: Variant; var Dest: IInterface);

// ----------------------
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
function _IntfClr(var Dest: IInterface): Pointer;
procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
procedure _IntfAddRef(const Dest: IInterface);
//procedure _ObjCastIntf(var Dest: IInterface; obj: TObject; Offset: Pointer; itype: Longint);

// ----------------------
function _SetIn(var S; Size, Elem: Byte): Boolean;
procedure _SetElem(var S; Size, Elem: Byte);
procedure _SetRange(var S; Size, Lo, Hi: Byte);
procedure _SetUnion(var Dest; const Src; Size: Byte);
procedure _SetSub(var Dest; const Src; Size: Byte);
procedure _SetInterset(var Dest; const Src; Size: Byte);
function _SetNE(const Left, Right; Size: Byte): Boolean;
function _SetEQ(const Left, Right; Size: Byte): Boolean;
function _SetLE(const Left, Right; Size: Byte): Boolean;
function _SetGE(const Left, Right; Size: Byte): Boolean;
procedure _SetInclude(var S; Size, Elem: Byte);
procedure _SetExclude(var S; Size, Elem: Byte);

function _NSetIn(var S; Size, Elem: Byte): Boolean;
procedure _NSetElem(var S; Size, Elem: Byte);
procedure _NSetRange(var S; Size, Lo, Hi: Byte);
procedure _NSetUnion(var Dest; const Src; Size: Byte);
procedure _NSetSub(var Dest; const Src; Size: Byte);
procedure _NSetInterset(var Dest; const Src; Size: Byte);
function _NSetNE(const Left, Right; Size: Byte): Boolean;
function _NSetEQ(const Left, Right; Size: Byte): Boolean;
function _NSetLE(const Left, Right; Size: Byte): Boolean;
function _NSetGE(const Left, Right; Size: Byte): Boolean;
procedure _NSetInclude(var S; Size, Elem: Byte);
procedure _NSetExclude(var S; Size, Elem: Byte);
procedure _SetCopy(var Dest; const Src; d_lo, d_hi, s_lo, s_hi: Byte);
procedure _SetInflate(var Dest; const Src; lo, hi, maxHi: Byte);
procedure _SetExpand(var Dest; const Src; lo, hi: Byte);

type
  TRuntimeError = (reNone, reOutOfMemory, reInvalidPtr, reDivByZero,
  reRangeError, reIntOverflow, reInvalidOp, reZeroDivide, reOverflow,
  reUnderflow, reInvalidCast, reAccessViolation, rePrivInstruction,
  reControlBreak, reStackOverflow,
  { reVar* used in Variants.pas }
  reVarTypeCast, reVarInvalidOp,
  reVarDispatch, reVarArrayCreate, reVarNotArray, reVarArrayBounds,
  reAssertionFailed,
  reExternalException, { not used here; in SysUtils }
  reIntfCastError, reSafeCallError);

procedure Error(errorCode: TRuntimeError);
procedure Halt(code: Integer);
procedure FreeAndNil(var obj: TObject);

var
  ErrorProc: procedure (ErrorCode: Byte; ErrorAddr: Pointer);
implementation

procedure _CrtExit(code: Integer); external;

procedure Halt(code: Integer);
begin
  // first, call unit finalization
  
  // then terminate program
  _CrtExit(code);
end;

procedure Error(errorCode: TRuntimeError);
begin
  if Assigned(ErrorProc) then
    ErrorProc(Ord(errorCode), nil { caller address });
  Halt(1);
end;

procedure FreeAndNil(var obj: TObject);
var
  temp: TObject;
begin
  temp := obj;
  obj := nil;
  temp.Free;
end;

function _GetMem(Size: Integer): Pointer;
begin
  GetMem(Result, Size);
end;

procedure _FreeMem(P: Pointer);
begin
  FreeMem(P);
end;

function _FinalizeRecord(P: Pointer; TypeInfo: Pointer): Pointer;
begin
  // todo 1: 以后补上
  Result := nil;
end;

//----------------------------

procedure _IntOverflow;
begin
  Error(reIntOverflow);
end;

procedure _OutOfRange;
begin
  Error(reRangeError);
end;

procedure _IOCheck;
begin
end;

procedure _SafecallCheck(hr: Integer);
begin
end;
(*
procedure _RaiseExcept(exobj: TObject);
begin
end;

procedure _HandleSafecallExcept(Instance, ExceptObject: TObject);
begin
end;

procedure _HandleCtorExcept(E: Pointer; Instance: TObject; Flag: Shortint);
begin
// compiler generated
end;
*)
function _InternalHandleSafecall(Instance, ExceptObject: TObject): Integer;
begin
  if Instance = nil then
    Result := HResult($8000FFFF)
  else
    Result := Instance.SafeCallException(ExceptObject, nil);
  ExceptObject.Free;
end;

procedure _Terminated;
begin
end;

function _IsClass(Child: TObject; Parent: TClass): Boolean;
begin
  Result := (Child <> nil) and Child.InheritsFrom(Parent);
end;

function _AsClass(Child: TObject; Parent: TClass): TObject;
begin
  Result := Child;
  if not (Child is Parent) then
    Error(reInvalidCast);
end;

function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;
begin
  Result := _SafeIntfAsClass(Intf, Parent) <> nil;
end;

function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
{var
  Temp: Pointer;}
begin
  {Temp := nil;
  _IntfCast(IInterface(Temp), Intf, ObjCastGUID);
  Result := _AsClass(TObject(Temp), Parent);}
  Result := nil;
end;

function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
begin
  {if (Intf <> nil) and (Intf.QueryInterface(ObjCastGUID, Result) = S_OK)
     and (Result is Parent) then Exit;}
  Result := nil;
end;

// ------------------------------

function _Round(v: Double): Int64;
begin
end;

function _Trunc(v: Double): Int64;
begin
end;

// ---------------------------
type
  PAnsiStrRec = ^AnsiStrRec;
  AnsiStrRec = packed record
    __pads: Word;
    codePage: Word;
    refCnt: Longint;
{$ifdef CPU64}
    length: NativeInt;
{$ELSE}
    length: Longint;
{$ENDIF}
  end;

procedure _AStrClr(var s: AnsiString);
var
  P: PAnsiStrRec;
begin
  if Pointer(s) = nil then Exit;
  P := Pointer(NativeInt(S) - Sizeof(AnsiStrRec));
  Pointer(S) := nil;
  if P.refCnt > 0 then
    if InterlockedDecrement(P.refCnt) = 0 then
	  FreeMem(P);
end;

procedure _AStrAddRef(var S: AnsiString);
var
  P: PAnsiStrRec;
begin
  P := Pointer(NativeInt(S) - sizeof(AnsiStrRec));
  if P <> nil then
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
end;

procedure _AStrNew(var S: AnsiString; Count: Integer);
begin
end;

const
	_EmptyStr: AnsiChar = #0;

function _AStrPtr(const S: AnsiString): PAnsiChar;
begin
	if Pointer(S) = nil then
		Result := @_EmptyStr
	else
		Result := Pointer(S);
end;

function _AStrLength(const S: AnsiString): Integer;
begin
	if Pointer(S) = nil then
		Result := 0
	else
		Result := (PInteger(S) - 1)^;
end;

procedure _AStrAsgCopy(var Dest: AnsiString; const Source: AnsiString);
begin
end;

procedure _AStrAsg(var Dest: AnsiString; const Source: AnsiString);
begin
end;

procedure _AStrSetLength(var S: AnsiString; Len: Integer);
begin
end;

procedure _AStrCopy(var Dest: AnsiString; const Source: AnsiString; Start, Count: Integer);
begin
end;

procedure _AStrDelete(var Dest: AnsiString; Index, Count: Integer);
begin
end;

procedure _AStrInsert(var Dest: AnsiString; const Source: AnsiString; Index: Integer);
begin
end;

procedure _AStrFromSStr(var Dest: AnsiString; const Source: ShortString);
begin
end;

procedure _AStrFromWStr(var Dest: AnsiString; const Source: WideString);
begin
end;

procedure _AStrFromUStr(var Dest: AnsiString; const Source: UnicodeString);
begin
end;

procedure _AStrFromACh(var Dest: AnsiString; ACh: AnsiChar);
begin
end;

procedure _AStrFromWCh(var Dest: AnsiString; WCh: WideChar);
begin
end;

procedure _AStrFromPACh(var Dest: AnsiString; Buf: PAnsiChar);
begin
end;

procedure _AStrFromPAChLen(var Dest: AnsiString; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _AStrFromPWCh(var Dest: AnsiString; Buf: PWideChar);
begin
end;

procedure _AStrFromPWChLen(var Dest: AnsiString; Buf: PWideChar; Count: Integer);
begin
end;

procedure _AStrFromAArray(var Dest: AnsiString; Buf: PAnsiChar; MaxChars: Integer);
begin
end;

procedure _AStrFromWArray(var Dest: AnsiString; Buf: PWideChar; MaxChars: Integer);
begin
end;

procedure _AStrCat(var S1: AnsiString; const S2: AnsiString);
begin
end;

procedure _AStrCat3(var S1: AnsiString; const S2, S3: AnsiString);
begin
end;

procedure _AStrCatN(var S1: AnsiString; const Source: array of AnsiString);
begin
end;
// -------------------------
procedure _WStrClr(var S: WideString);
begin
end;

procedure _WStrNew(var S: WideString; Count: Integer);
begin
end;

const
	_EmptyWStr: WideChar = #0;

function _WStrPtr(const S: WideString): PWideChar;
begin
	Result := Pointer(S);
	if Result = nil then
		Result := @_EmptyWStr;
end;

function _WStrLength(const S: WideString): Integer;
begin
	if Pointer(S) = nil then
		Result := 0
	else
		Result := (PInteger(S) - 1)^;
end;

procedure _WStrAsgCopy(var Dest: WideString; const Source: WideString);
begin
end;

procedure _WStrAsg(var Dest: WideString; const Source: WideString);
begin
end;

procedure _WStrSetLength(var S: WideString; Len: Integer);
begin
end;

procedure _WStrCopy(var Dest: WideString; const Source: WideString; Start, Count: Integer);
begin
end;

procedure _WStrDelete(var Dest: WideString; Index, Count: Integer);
begin
end;

procedure _WStrInsert(var Dest: WideString; const Source: WideString; Index: Integer);
begin
end;

procedure _WStrFromSStr(var Dest: WideString; const Source: ShortString);
begin
end;

procedure _WStrFromAStr(var Dest: WideString; const Source: AnsiString);
begin
end;

procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
begin
end;

procedure _WStrFromACh(var Dest: WideString; ACh: AnsiChar);
begin
end;

procedure _WStrFromWCh(var Dest: WideString; WCh: WideChar);
begin
end;

procedure _WStrFromPACh(var Dest: WideString; Buf: PAnsiChar);
begin
end;

procedure _WStrFromPAChLen(var Dest: WideString; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _WStrFromPWCh(var Dest: WideString; Buf: PWideChar);
begin
end;

procedure _WStrFromPWChLen(var Dest: WideString; Buf: PWideChar; Count: Integer);
begin
end;

procedure _WStrFromAArray(var Dest: WideString; Buf: PAnsiChar; MaxChars: Integer);
begin
end;

procedure _WStrFromWArray(var Dest: WideString; Buf: PWideChar; MaxChars: Integer);
begin
end;

procedure _WStrCat(var S1: WideString; const S2: WideString);
begin
end;

procedure _WStrCat3(var S1: WideString; const S2, S3: WideString);
begin
end;

procedure _WStrCatN(var S1: WideString; const Source: array of WideString);
begin
end;
// -------------------------
procedure _UStrClr(var S: UnicodeString);
begin
end;

procedure _UStrNew(var S: UnicodeString; Count: Integer);
begin
end;

function _UStrPtr(const S: UnicodeString): PWideChar;
begin
	Result := Pointer(S);
	if Result = nil then
		Result := @_EmptyWStr;
end;

function _UStrLength(const S: UnicodeString): Integer;
begin
	if Pointer(S) = nil then
		Result := 0
	else
		Result := (PInteger(S) - 1)^;
end;

procedure _UStrAsgCopy(var Dest: UnicodeString; const Source: UnicodeString);
begin
end;

procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString);
begin
end;

procedure _UStrSetLength(var S: UnicodeString; Len: Integer);
begin
end;

procedure _UStrCopy(var Dest: UnicodeString; const Source: UnicodeString; Start, Count: Integer);
begin
end;

procedure _UStrDelete(var Dest: UnicodeString; Index, Count: Integer);
begin
end;

procedure _UStrInsert(var Dest: UnicodeString; const Source: UnicodeString; Index: Integer);
begin
end;

procedure _UStrFromSStr(var Dest: UnicodeString; const Source: ShortString);
begin
end;

procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
begin
end;

procedure _UStrFromAStr(var Dest: UnicodeString; const Source: AnsiString);
begin
end;

procedure _UStrFromACh(var Dest: UnicodeString; ACh: AnsiChar);
begin
end;

procedure _UStrFromWCh(var Dest: UnicodeString; WCh: WideChar);
begin
end;

procedure _UStrFromPACh(var Dest: UnicodeString; Buf: PAnsiChar);
begin
end;

procedure _UStrFromPAChLen(var Dest: UnicodeString; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _UStrFromPWCh(var Dest: UnicodeString; Buf: PWideChar);
begin
end;

procedure _UStrFromPWChLen(var Dest: UnicodeString; Buf: PWideChar; Count: Integer);
begin
end;

procedure _UStrFromAArray(var Dest: UnicodeString; Buf: PAnsiChar; MaxChars: Integer);
begin
end;

procedure _UStrFromWArray(var Dest: UnicodeString; Buf: PWideChar; MaxChars: Integer);
begin
end;

procedure _UStrCat(var S1: UnicodeString; const S2: UnicodeString);
begin
end;

procedure _UStrCat3(var S1: UnicodeString; const S2, S3: UnicodeString);
begin
end;

procedure _UStrCatN(var S1: UnicodeString; const Source: array of UnicodeString);
begin
end;

// ----------------------
procedure _SStrClr(var Dest: ShortString);
begin
	Dest[0] := #0;
end;

function _StrLength(const Dest: ShortString): Integer;
begin
	Result := Integer(Dest[0]);
end;

procedure _SStrAsg(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
begin
end;

procedure _SStrSetLength(var Dest: ShortString; Len: Integer);
begin
end;

procedure _SStrCopy(var Dest: ShortString; MaxChars: Integer; const Source: ShortString; Start, Count: Integer);
begin
end;

procedure _SStrDelete(var Dest: ShortString; Index, Count: Integer);
begin
end;

procedure _SStrInsertS(var Dest: ShortString; MaxChars: Integer; const Source: ShortString; Index: Integer);
begin
end;

procedure _SStrInsertA(var Dest: ShortString; MaxChars: Integer; const Source: AnsiString; Index: Integer);
begin
end;

procedure _SStrInsertW(var Dest: ShortString; MaxChars: Integer; const Source: WideString; Index: Integer);
begin
end;

procedure _SStrInsertU(var Dest: ShortString; MaxChars: Integer; const Source: UnicodeString; Index: Integer);
begin
end;

procedure _SStrFromSStr(var Dest: ShortString; MaxChars: Integer; const Source: ShortString);
begin
end;

procedure _SStrFromAStr(var Dest: ShortString; MaxChars: Integer; const Source: WideString);
begin
end;

procedure _SStrFromWStr(var Dest: ShortString; MaxChars: Integer; const Source: WideString);
begin
end;

procedure _SStrFromUStr(var Dest: ShortString; MaxChars: Integer; const Source: UnicodeString);
begin
end;

procedure _SStrFromACh(var Dest: ShortString; MaxChars: Integer; ACh: AnsiChar);
begin
end;

procedure _SStrFromWCh(var Dest: ShortString; MaxChars: Integer; WCh: WideChar);
begin
end;

procedure _SStrFromPACh(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar);
begin
end;

procedure _SStrFromPAChLen(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _SStrFromPWCh(var Dest: ShortString; Buf: PWideChar; MaxChars: Integer);
begin
end;

procedure _SStrFromPWChLen(var Dest: ShortString; MaxChars: Integer; Buf: PWideChar; Count: Integer);
begin
end;

procedure _SStrFromAArray(var Dest: ShortString; MaxChars: Integer; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _SStrFromWArray(var Dest: ShortString; MaxChars: Integer; Buf: PWideChar; Count: Integer);
begin
end;

procedure _SStrCat(var S1: ShortString; MaxChars: Integer; const S2: ShortString);
begin
end;

procedure _SStrCat3(var S1: ShortString; MaxChars: Integer; const S2, S3: ShortString);
begin
end;

procedure _SStrCatN(var S1: ShortString; MaxChars: Integer; const Source: array of ShortString);
begin
end;

// -------------------------
function _AStrComp(const S1, S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrComp(const S1, S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrComp(const S1, S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrComp(const S1, S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrComp(const S1: PAnsiChar; MaxChars1: Integer; S2: PAnsiChar; MaxChars2: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrComp(const S1: PWideChar; MaxChars1: Integer; S2: PWideChar; MaxChars2: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompWStr(const S1: AnsiString; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompUStr(const S1: AnsiString; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompSStr(const S1: AnsiString; const S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _AStrCompSWStr(const S1: AnsiString; const S2: ShortString): Integer;
function _AStrCompPa(const S1: AnsiString; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompPw(const S1: AnsiString; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompAarr(const S1: AnsiString; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompWarr(const S1: AnsiString; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompACh(const S1: AnsiString; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AStrCompWCh(const S1: AnsiString; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompAStr(const S1: WideString; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompUStr(const S1: WideString; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompSStr(const S1: WideString; const S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _WStrCompSWStr(const S1: WideString; const S2: ShortString): Integer;
function _WStrCompPa(const S1: WideString; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompPw(const S1: WideString; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompAarr(const S1: WideString; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompWarr(const S1: WideString; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompACh(const S1: WideString; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WStrCompWCh(const S1: WideString; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompAStr(const S1: UnicodeString; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompWStr(const S1: UnicodeString; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompSStr(const S1: UnicodeString; const S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _UStrCompSWStr(const S1: UnicodeString; const S2: ShortString): Integer;
function _UStrCompPa(const S1: UnicodeString; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompPw(const S1: UnicodeString; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompAarr(const S1: UnicodeString; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompWarr(const S1: UnicodeString; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompACh(const S1: UnicodeString; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _UStrCompWCh(const S1: UnicodeString; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompAStr(const S1: ShortString; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompWStr(const S1: ShortString; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompUStr(const S1: ShortString; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _SStrCompSWStr(const S1: ShortString; const S2: ShortString): Integer;
function _SStrCompPa(const S1: ShortString; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompPw(const S1: ShortString; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompAarr(const S1: ShortString; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompWarr(const S1: ShortString; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompACh(const S1: ShortString; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _SStrCompWCh(const S1: ShortString; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompAStr(const S1: PAnsiChar; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompWStr(const S1: PAnsiChar; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompUStr(const S1: PAnsiChar; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _PaCompSWStr(const S1: PAnsiChar; const S2: ShortString): Integer;
function _PaCompAarr(const S1: PAnsiChar; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompWarr(const S1: PAnsiChar; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompACh(const S1: PAnsiChar; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PaCompWCh(const S1: PAnsiChar; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompAStr(const S1: PWideChar; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompWStr(const S1: PWideChar; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompUStr(const S1: PWideChar; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _PaCompSWStr(const S1: PWideChar; const S2: ShortString): Integer;
function _PwCompAarr(const S1: PWideChar; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompWarr(const S1: PWideChar; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompACh(const S1: PWideChar; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _PwCompWCh(const S1: PWideChar; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompAStr(const S1: PAnsiChar; MaxChars: Integer; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompWStr(const S1: PAnsiChar; MaxChars: Integer; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompUStr(const S1: PAnsiChar; MaxChars: Integer; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _AarrCompSWStr(const S1: PAnsiChar; MaxChars: Integer; const S2: ShortString): Integer;
function _AarrCompPa(const S1: PAnsiChar; MaxChars: Integer; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompPw(const S1: PAnsiChar; MaxChars: Integer; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompWarr(const S1: PAnsiChar; MaxChars1: Integer; S2: PWideChar; MaxChars2: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompACh(const S1: PAnsiChar; MaxChars: Integer; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AarrCompWCh(const S1: PAnsiChar; MaxChars: Integer; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompAStr(const S1: PWideChar; MaxChars: Integer; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompWStr(const S1: PWideChar; MaxChars: Integer; const S2: WideString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompUStr(const S1: PWideChar; MaxChars: Integer; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _WarrCompSWStr(const S1: PWideChar; MaxChars: Integer; const S2: ShortString): Integer;
function _WarrCompPa(const S1: PWideChar; MaxChars: Integer; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompPw(const S1: PWideChar; MaxChars: Integer; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompAarr(const S1: PWideChar; MaxChars1: Integer; S2: PAnsiChar; MaxChars2: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompACh(const S1: PWideChar; MaxChars: Integer; Ch: AnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WarrCompWCh(const S1: PWideChar; MaxChars: Integer; Ch: WideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompAStr(const S1: AnsiChar; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompUStr(const S1: AnsiChar; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompSStr(const S1: AnsiChar; const S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _AChCompSWStr(const S1: AnsiChar; const S2: ShortString): Integer;
function _AChCompPa(const S1: AnsiChar; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompPw(const S1: AnsiChar; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompAarr(const S1: AnsiChar; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _AChCompWarr(const S1: AnsiChar; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompAStr(const S1: WideChar; const S2: AnsiString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompUStr(const S1: WideChar; const S2: UnicodeString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompSStr(const S1: WideChar; const S2: ShortString): Integer;
begin
// todo: Need code
	Result := 0; 
end;

//function _WChCompSWStr(const S1: WideChar; const S2: ShortString): Integer;
function _WChCompPa(const S1: WideChar; S2: PAnsiChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompPw(const S1: WideChar; S2: PWideChar): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompAarr(const S1: WideChar; S2: PAnsiChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

function _WChCompWarr(const S1: WideChar; S2: PWideChar; MaxChars: Integer): Integer;
begin
// todo: Need code
	Result := 0; 
end;

// -------------------------
procedure _VarClr(var V: Variant);
begin
end;

procedure _VarOp(var V1: Variant; const V2: Variant; Op: TVarOp);
begin
end;

procedure _VarNot(var V: Variant);
begin
end;

procedure _VarNeg(var V: Variant);
begin
end;

procedure _VarCopy(var Dest: Variant; const Source: Variant);
begin
end;

procedure _Var2AStr(const V: Variant; var Dest: AnsiString);
begin
end;

procedure _Var2WStr(const V: Variant; var Dest: WideString);
begin
end;

procedure _Var2UStr(const V: Variant; var Dest: UnicodeString);
begin
end;

procedure _Var2SStr(const V: Variant; var Dest: ShortString; MaxChars: Integer);
begin
end;

procedure _Var2Intf(const V: Variant; var Dest: IInterface);
begin
end;
// -------------------------
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
begin
  if Source = nil then
    Dest := nil
  else if Source.QueryInterface(IID, Dest) <> 0 then
    Error(reIntfCastError);
end;

function _IntfClr(var Dest: IInterface): Pointer;
var
  P: Pointer;
begin
  Result := @Dest;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;

procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
var
  P: Pointer;
begin
  P := Pointer(Dest);
  if Source <> nil then
    Source._AddRef;
  Pointer(Dest) := Pointer(Source);
  if P <> nil then
    IInterface(P)._Release;
end;

procedure _IntfAddRef(const Dest: IInterface);
begin
  if Dest <> nil then Dest._AddRef;
end;

// -------------------------
function _SetIn(var S; Size, Elem: Byte): Boolean;
begin
// Placehold
	Result := False;
end;

procedure _SetElem(var S; Size, Elem: Byte);
begin
// Placehold
	
end;

procedure _SetRange(var S; Size, Lo, Hi: Byte);
begin
end;

procedure _SetUnion(var Dest; const Src; Size: Byte);
begin
end;

procedure _SetSub(var Dest; const Src; Size: Byte);
begin
end;

procedure _SetInterset(var Dest; const Src; Size: Byte);
begin
end;

function _SetNE(const Left, Right; Size: Byte): Boolean;
begin
// Placehold
	Result := False;
end;

function _SetEQ(const Left, Right; Size: Byte): Boolean;
begin
// Placehold
	Result := False;
end;

function _SetLE(const Left, Right; Size: Byte): Boolean;
begin
// Placehold
	Result := False;
end;

function _SetGE(const Left, Right; Size: Byte): Boolean;
begin
// Placehold
	Result := False;
end;

procedure _SetInclude(var S; Size, Elem: Byte);
begin
end;

procedure _SetExclude(var S; Size, Elem: Byte);
begin
end;

function _NSetIn(var S; Size, Elem: Byte): Boolean;
var
	c: Integer;
begin
	{c := Elem div 8;
	if c < Size then
		Result := PByte(@S)[c] and (1 shl (Elem mod 8))
	else
		Result := False;}
end;

procedure _NSetElem(var S; Size, Elem: Byte);
begin
end;

procedure _NSetRange(var S; Size, Lo, Hi: Byte);
begin
end;

procedure _NSetUnion(var Dest; const Src; Size: Byte);
begin
end;

procedure _NSetSub(var Dest; const Src; Size: Byte);
begin
end;

procedure _NSetInterset(var Dest; const Src; Size: Byte);
begin
end;

function _NSetNE(const Left, Right; Size: Byte): Boolean;
begin
end;

function _NSetEQ(const Left, Right; Size: Byte): Boolean;
begin
end;

function _NSetLE(const Left, Right; Size: Byte): Boolean;
begin
end;

function _NSetGE(const Left, Right; Size: Byte): Boolean;
begin
end;

procedure _NSetInclude(var S; Size, Elem: Byte);
begin
end;

procedure _NSetExclude(var S; Size, Elem: Byte);
begin
end;

procedure _SetCopy(var Dest; const Src; d_lo, d_hi, s_lo, s_hi: Byte);
begin
// Placehold
end;

procedure _SetInflate(var Dest; const Src; lo, hi, maxHi: Byte);
begin
// Placehold
end;

procedure _SetExpand(var Dest; const Src; lo, hi: Byte);
begin
// Placehold
end;

//-----------------------------
type
  PMethRec = ^MethRec;
  MethRec = packed record
    recSize: Word;
    methAddr: Pointer;
    nameLen: Byte;
    { nameChars[nameLen]: AnsiChar }
  end;

constructor TObject.Create;
begin
end;

function TObject.ClassType: TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

class function TObject.ClassName: ShortString;
begin
	Result := PShortString((PAnsiChar(Self) + vmtClassName))^;
//  Result := PShortString(PPointer(Integer(Self) + vmtClassName)^)^;
end;

class function TObject.ClassNameIs(const Name: string): Boolean;
var
  Temp: ShortString;
  I: Byte;
begin
  Result := False;
  Temp := ClassName;
  for I := 0 to Byte(Temp[0]) do
    if Temp[I] <> Name[I] then Exit;
  Result := True;
end;

class function TObject.ClassInfo: Pointer;
begin
  Result := PPointer(Integer(Self) + vmtTypeInfo)^;
end;

class function TObject.ClassParent: TClass;
begin
  Pointer(Result) := PPointer(Integer(Self) + vmtParent)^;
  if Result <> nil then
    Pointer(Result) := PPointer(Result)^;
end;

procedure TObject.CleanupInstance;
var
  ClassPtr: TClass;
  InitTable: Pointer;
begin
  ClassPtr := ClassType;
  InitTable := PPointer(Integer(ClassPtr) + vmtInitTable)^;
  while (ClassPtr <> nil) and (InitTable <> nil) do
  begin
    _FinalizeRecord(Self, InitTable);
    ClassPtr := ClassPtr.ClassParent;
    if ClassPtr <> nil then
      InitTable := PPointer(Integer(ClassPtr) + vmtInitTable)^;
  end;
end;

destructor TObject.Destroy;
begin
end;

function TObject.FieldAddress(const Name: ShortString): Pointer;
var
  LFieldTablePtr: Pointer;
  LFldCount: Word;
  LName: PShortString;
  LClass: TClass;
begin
  Result := nil;

  LClass := PPointer(Self)^;
  while True do
  begin
    { Obtain the field table and count }
    LFieldTablePtr := PPointer(PByte(LClass) + vmtFieldTable)^;
    if LFieldTablePtr <> nil then
    begin
      LFldCount := PWord(LFieldTablePtr)^;
      Inc(PWord(LFieldTablePtr), 1);  { Count: Word }
      Inc(PPointer(LFieldTablePtr), 1); { ClassTab: Pointer }
    end else
      LFldCount := 0;

    { Search for the field if we have more than one. Also tested for a correct table ptr }
    if LFldCount > 0 then
    begin
      while LFldCount > 0 do
      begin
        LName := PShortString(PByte(LFieldTablePtr) + SizeOf(Word) + SizeOf(Longword));

        if (LName^[0] = Name[0]) and (LName^ = Name) // !!
           {(UTF8Compare(LName^, Name))} then
        begin
          Result := Pointer(PByte(Self) + PLongword(LFieldTablePtr)^);
          Exit;
        end else
        begin
          Dec(LFldCount);
          { Skip 1 word, 1 Pointer, the length of the name (1 Byte) and the characters of the name }
          Inc(PByte(LFieldTablePtr), SizeOf(Word) + SizeOf(Longword) + Byte(LName^[0]) + 1);
        end;
      end;
    end;

    { Go to the parent class }
    LClass := LClass.ClassParent;
    if LClass = nil then
      Exit;
  end;
end;

procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;

function InvokeImplGetter(Self: TObject; ImplGetter: Cardinal): IInterface;
var
  M: function: IInterface of object;
begin
  TMethod(M).Data := Self;
  if (ImplGetter >= $ff000000) and (ImplGetter <= $ffffffff) then
    Result := IInterface(Pointer(Cardinal(Self) + (ImplGetter and $00FFFFFF)))
  else if (ImplGetter >= $fe000000) and (ImplGetter <= $feffffff) then
  begin
    // sign extend vmt slot offset = smallint cast
    TMethod(M).Code := PPointer(Integer(Self) + SmallInt(ImplGetter))^;
    Result := M;
  end
  else
  begin
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;
(*  case ImplGetter of
    $FF000000..$FFFFFFFF:  // Field
        Result := IInterface(Pointer(Cardinal(Self) + (ImplGetter and $00FFFFFF)));
    $FE000000..$FEFFFFFF:  // virtual method
      begin
        // sign extend vmt slot offset = smallint cast
        TMethod(M).Code := PPointer(Integer(Self) + SmallInt(ImplGetter))^;
        Result := M;
      end;
  else // static method
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;*)
end;

function TObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  InterfaceEntry := GetInterfaceEntry(IID);
  if InterfaceEntry <> nil then
  begin
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(Obj) := Pointer(PAnsiChar(Self) + InterfaceEntry^.IOffset);
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    end
    else
      IInterface(Obj) := InvokeImplGetter(Self, InterfaceEntry^.ImplGetter);
  end;
  Result := Pointer(Obj) <> nil;
end;

class function TObject.GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  I: Integer;
begin 
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
      begin
        Result := @IntfTable.Entries[I];
//        if Result^.IID = IID then Exit;
        if (Result^.IID.D1 = IID.D1) and
           (Result^.IID.D2 = IID.D2) and
           (Result^.IID.D3 = IID.D3) and
           (Int64(Result^.IID.D4) = Int64(IID.D4)) then Exit;
      end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := nil;
end;

class function TObject.GetInterfaceTable: PInterfaceTable;
begin
  Result := PPointer(Integer(Self) + vmtIntfTable)^;
end;

class function TObject.InitInstance(Instance: Pointer): TObject;
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  FillChar(Instance^, InstanceSize, #0);
  PInteger(Instance)^ := Integer(Self);
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
        with IntfTable.Entries[I] do
        begin
          if VTable <> nil then
            PInteger(@PChar(Instance)[IOffset])^ := Integer(VTable);
        end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := Instance;
end;
{ old code 
class function TObject.InheritsFrom(AClass: TClass): Boolean;
var
  ClassPtr: TClass;
begin
  ClassPtr := Self;
  while (ClassPtr <> nil) and (ClassPtr <> AClass) do
    ClassPtr := PPointer(Integer(ClassPtr) + vmtParent)^;
  Result := ClassPtr = AClass;
end;}
class function TObject.InheritsFrom(AClass: TClass): Boolean;
var
  ClassPtr: TClass;
begin
  ClassPtr := Self;
  while (ClassPtr <> nil) and (ClassPtr <> AClass) do
  begin
    ClassPtr := PPointer(PAnsiChar(ClassPtr) + vmtParent)^;
	if ClassPtr = nil then Break;
	ClassPtr := PPointer(ClassPtr)^;
  end;
  Result := ClassPtr = AClass;
end;

class function TObject.InstanceSize: Longint;
begin
  Result := PInteger(Integer(Self) + vmtInstanceSize)^;
end;

class function TObject.MethodAddress(const Name: ShortString): Pointer;
var
  LMethTablePtr: Pointer;
  LMethCount: Word;
  LMethEntry: PMethRec;
  LSelf: TClass;
begin
  Result := nil;

  LSelf := Pointer(Self);
  while True do
  begin
    { Obtain the method table and count }
    LMethTablePtr := PPointer(PByte(LSelf) + vmtMethodTable)^;
    if LMethTablePtr <> nil then
    begin
      LMethCount := PWord(LMethTablePtr)^;
      Inc(PWord(LMethTablePtr), 1);
    end else
      LMethCount := 0;

    { Search for the method if we have more than one. Also tested for a correct table ptr }
    if LMethCount > 0 then
    begin
      LMethEntry := LMethTablePtr;

      while LMethCount > 0 do
      begin
        if (LMethEntry^.nameLen = Byte(Name[0])) and
            (PShortString(@LMethEntry^.nameLen)^ = Name) // !!
           {(UTF8Compare(PShortString(@LMethEntry^.nameLen)^, Name))} then
        begin
          Result := LMethEntry.methAddr;
          Exit;
        end else
        begin
          Dec(LMethCount);
          LMethEntry := Pointer(PByte(LMethEntry) + LMethEntry.recSize);
        end;
      end;
    end;

    { Go to the parent class }
    LSelf := LSelf.ClassParent;
    if LSelf = nil then
      Exit;
  end;
end;

class function TObject.MethodName(Address: Pointer): ShortString;
var
  LMethTablePtr: Pointer;
  LMethCount: Word;
  LMethEntry: PMethRec;
  LSelf: TClass;
begin
  Result := '';

  LSelf := Pointer(Self);
  while True do
  begin
    { Obtain the method table and count }
    LMethTablePtr := PPointer(PByte(LSelf) + vmtMethodTable)^;
    if LMethTablePtr <> nil then
    begin
      LMethCount := PWord(LMethTablePtr)^;
      Inc(PWord(LMethTablePtr), 1);
    end else
      LMethCount := 0;

    { Search for the method if we have more than one. Also tested for a correct table ptr }
    if LMethCount > 0 then
    begin
      LMethEntry := LMethTablePtr;

      while LMethCount > 0 do
      begin
        if LMethEntry^.methAddr = Address then
        begin
        //  Result := UTF8ShortStringToString(PShortString(@LMethEntry.nameLen)^);
          Result := PShortString(@LMethEntry.nameLen)^;
          Exit;
        end else
        begin
          Dec(LMethCount);
          LMethEntry := Pointer(PByte(LMethEntry) + LMethEntry.recSize);
        end;
      end;
    end;

    { Go to the parent class }
    LSelf := LSelf.ClassParent;
    if LSelf = nil then
      Exit;
  end;
end;

function TObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HResult($8000FFFF); { E_UNEXPECTED }
end;

procedure TObject.DefaultHandler(var Message);
begin
end;

function GetDynaMethod(vmt: TClass; selector: SmallInt): Pointer;
type
  TDynaMethodTable = record
    Count: Word;
    Selectors: array[0..9999999] of SmallInt;
    {Addrs: array[0..0] of Pointer;}
  end;
  PDynaMethodTable = ^TDynaMethodTable;
var
  dynaTab: PDynaMethodTable;
  Parent: Pointer;
  Addrs: PPointer;
  I: Cardinal;
begin
  while True do
  begin
    dynaTab := PPointer(PByte(vmt) + vmtDynamicTable)^;
    if dynaTab <> nil then
    begin
      for I := 0 to dynaTab.Count - 1 do
        if dynaTab.Selectors[I] = selector then
        begin
          Addrs := PPointer(PByte(@dynaTab.Selectors) + dynaTab.Count * SizeOf(dynaTab.Selectors[0]));
          Result := PPointer(PByte(Addrs) + I * SizeOf(Pointer))^;
          Exit;
        end;
    end;
    Parent := PPointer(PByte(vmt) + vmtParent)^;
    if Parent = nil then Break;
    vmt := PPointer(Parent)^;
  end;
  Result := nil;
end;

procedure TObject.Dispatch(var Message);
type
  //THandlerProc = procedure(Self: Pointer; var Message) { of object };
  THandlerProc = procedure(var Message) of object;
var
  MsgID: Word;
  Addr: Pointer;
  M: THandlerProc;
begin
  MsgID := TDispatchMessage(Message).MsgID;
  if (MsgID <> 0) and (MsgID < $C000) then
  begin
    Addr := GetDynaMethod(PPointer(Self)^, MsgID);
    if Addr <> nil then
    begin
      //THandlerProc(Addr)(Self, Message)
      TMethod(M).Data := Self;
      TMethod(M).Code := Addr;
      M(Message);
    end
    else
      Self.DefaultHandler(Message);
  end
  else
    Self.DefaultHandler(Message);
end;

procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;

class function TObject.NewInstance: TObject;
begin
  Result := InitInstance(_GetMem(InstanceSize));
end;

procedure TObject.FreeInstance;
begin
  CleanupInstance;
  _FreeMem(Self);
end;

end.