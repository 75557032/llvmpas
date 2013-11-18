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

procedure FillChar(var Dest; Count: NativeInt; Value: AnsiChar);

function _GetMem(Size: Integer): Pointer;
procedure _FreeMem(P: Pointer);

// -------------------------
procedure _IntOverflow;
procedure _OutOfRange;
procedure _IOCheck;
procedure _RaiseExcept;
procedure _SafecallCheck(hr: Integer);
procedure _HandleSafecallExcept(Instance, ExceptObject: TObject);
procedure _HandleCtorExcept(E: Pointer; Instance: TObject; Flag: Shortint);
procedure _HandleFinally();
// -------------------------
function _Int64Div(a, b: Int64): Int64;
function _Int64Mod(a, b: Int64): Int64;
function _Round(v: Double): Int64;
function _Trunc(v: Double): Int64;

// -------------------------
procedure _AStrClr(var S: AnsiString);
procedure _AStrNew(var S: AnsiString; Count: Integer);
function _AStrPtr(const S: AnsiString): PAnsiChar;
function _AStrLength(const S: AnsiString): Integer;
function _AStrComp(const S1, S2: AnsiString): Integer;
function _AStrEqual(const S1, S2: AnsiString): Boolean;
procedure _AStrAsgCopy(var Dest: AnsiString; const Source: AnsiString);
procedure _AStrAsg(var Dest: AnsiString; const Source: AnsiString);
procedure _AStrSetLength(var S: AnsiString; Len: Integer);
procedure _AStrCopy(var Dest: AnsiString; const Source: AnsiString; Start, Count: Integer);
procedure _AStrFromSStr(var Dest: AnsiString; Source: PAnsiChar);
procedure _AStrFromWStr(var Dest: AnsiString; const Source: WideString);
procedure _AStrFromUStr(var Dest: AnsiString; const Source: UnicodeString);
procedure _AStrFromACh(var Dest: AnsiString; ACh: AnsiChar);
procedure _AStrFromWCh(var Dest: AnsiString; WCh: WideChar);
procedure _AStrFromPACh(var Dest: AnsiString; Buf: PAnsiChar);
procedure _AStrFromPAChLen(var Dest: AnsiString; Buf: PAnsiChar; Count: Integer);
procedure _AStrFromPWCh(var Dest: AnsiString; Buf: PWideChar);
procedure _AStrFromPWChLen(var Dest: AnsiString; Buf: PWideChar; Count: Integer);
procedure _AStrFromAArray(var Dest: AnsiString; Buf: PAnsiChar; Count: Integer);
procedure _AStrFromWArray(var Dest: AnsiString; Buf: PWideChar; Count: Integer);
procedure _AStrCat(var S1: AnsiString; const S2: AnsiString);
procedure _AStrCat3(var S1: AnsiString; const S2, S3: AnsiString);
procedure _AStrCatN(var S1: AnsiString; Count: Integer; StrList: Pointer);

// -------------------------
procedure _VarClr(var V: TVarData);
procedure _VarOp(var V1: TVarData; const V2: TVarData; Op: TVarOp);
procedure _VarNot(var V: TVarData);
procedure _VarNeg(var V: TVarData);
procedure _VarCopy(var Dest: TVarData; const Source: TVarData);
procedure _Var2AStr(const V: TVarData; var Dest: AnsiString);
procedure _Var2WStr(const V: TVarData; var Dest: WideString);
procedure _Var2UStr(const V: TVarData; var Dest: UnicodeString);

implementation

procedure FillChar(var Dest; Count: NativeInt; Value: AnsiChar);
var
  I: NativeInt;
  V: Int64;
  PB: PByte;
  P: PInt64;
  Total: NativeInt;
begin
  if Count >= 8 then
  begin
    V := Byte(Value) or (Byte(Value) shl 8) or
      (Byte(value) shl 16) or (Byte(value) shl 24);
    V := V or (V shl 32);
    P := PInt64(@Dest);
    Total := Count shr 3;

    for I := 0 to Total - 1 do
    begin
      P^ := V;
      Inc(P);
    end;
    PB := Pointer(P);
    { Get the remainder (mod 8) }
    Total := Count and $07;
  end
  else
  begin
    PB := PByte(@Dest);
    Total := Count;
  end;

  for I := Total - 1 downto 0 do
    PB[I] := Byte(Value);
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
end;

procedure _OutOfRange;
begin
end;

procedure _IOCheck;
begin
end;

procedure _RaiseExcept;
begin
end;

procedure _SafecallCheck(hr: Integer);
begin
end;

procedure _HandleSafecallExcept(Instance, ExceptObject: TObject);
begin
end;

procedure _HandleCtorExcept(E: Pointer; Instance: TObject; Flag: Shortint);
begin
// compiler generated
end;

procedure _HandleFinally();
begin

end;

// ------------------------------

function _Int64Div(a, b: Int64): Int64;
begin
end;

function _Int64Mod(a, b: Int64): Int64;
begin
end;

function _Round(v: Double): Int64;
begin
end;

function _Trunc(v: Double): Int64;
begin
end;

// ---------------------------
procedure _AStrClr(var s: AnsiString);
begin
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

function _AStrComp(const S1, S2: AnsiString): Integer;
begin
	//todo :
	Result := 0;
end;

function _AStrEqual(const S1, S2: AnsiString): Boolean;
begin
	//todo :
	Result := False;
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

procedure _AStrFromSStr(var Dest: AnsiString; Source: PAnsiChar);
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

procedure _AStrFromAArray(var Dest: AnsiString; Buf: PAnsiChar; Count: Integer);
begin
end;

procedure _AStrFromWArray(var Dest: AnsiString; Buf: PWideChar; Count: Integer);
begin
end;

procedure _AStrCat(var S1: AnsiString; const S2: AnsiString);
begin
end;

procedure _AStrCat3(var S1: AnsiString; const S2, S3: AnsiString);
begin
end;

procedure _AStrCatN(var S1: AnsiString; Count: Integer; StrList: Pointer);
begin
end;

// -------------------------
procedure _VarClr(var V: TVarData);
begin
end;

procedure _VarOp(var V1: TVarData; const V2: TVarData; Op: TVarOp);
begin
end;

procedure _VarNot(var V: TVarData);
begin
end;

procedure _VarNeg(var V: TVarData);
begin
end;

procedure _VarCopy(var Dest: TVarData; const Source: TVarData);
begin
end;

procedure _Var2AStr(const V: TVarData; var Dest: AnsiString);
begin
end;

procedure _Var2WStr(const V: TVarData; var Dest: WideString);
begin
end;

procedure _Var2UStr(const V: TVarData; var Dest: UnicodeString);
begin
end;

// -------------------------

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
        if (Int64(Result^.IID.D1) = Int64(IID.D1)) and
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

class function TObject.InheritsFrom(AClass: TClass): Boolean;
var
  ClassPtr: TClass;
begin
  ClassPtr := Self;
  while (ClassPtr <> nil) and (ClassPtr <> AClass) do
    ClassPtr := PPointer(Integer(ClassPtr) + vmtParent)^;
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