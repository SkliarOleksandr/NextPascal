unit ILTranslator;

interface

uses
  System.SysUtils, System.Classes, Generics.Defaults, Generics.Collections, System.Math, NPCompiler.Utils,
  NPCompiler.DataTypes, IL.Types, IL.TypeInfo, VM.Core, VM.Invoke, VM.Types; // system

const
   C_1M_ITEMS = 1024*1024;
   cRttiUnitName = 'sys.rtti';

type
  TNativeUIntArray = array of NativeUInt;

type
  TInstructions = array[0..C_1M_ITEMS] of NativeUInt;
  PInstructions = ^TInstructions;
  PInstruction = ^NativeUInt;


  TILArgs = class

  end;

  TILInstruction = record
  public
    Cond: TILCondition; // условие
    Code: TILCode;      // код
    Offset: UInt32;     // смещение в target коде
    Line: Integer;      // строка в исходном коде
    Args: TILArgs;      // аргументы
  end;
  PILInstr = ^TILInstruction;
  TILInstructions = array of TILInstruction;

  TILVarClass = (
    VarMutable,      // модифицируемая переменная
    VarParam,        // модифицируемый параметр
    VarConst,        // немодифицируемый параметр
    VarResult        // возвращаемый параметр
  );

  PILVariable = ^TILVariable;
  TILVariable = record
  public
    Name: string;                      // название переменной
    Flags: UInt8;                      // флаги
    RTTI: TRTTIField;
    AbsoluteTo: PILVariable;
    AbsoluteOffset: Integer;
    VarScope: TILARG_SCOPE;
    VarClass: TILVarClass;             // класс модификации переменной
    TmpUsed: Boolean;
    DefaultValue: Pointer;             // указатель на значени по умолчанию
    IsResult: Boolean;                 // это результат
    IsParam: Boolean;                  // это параметр
  end;

  TILVariables = array of TILVariable;
  PILVariables = ^TILVariables;

  TParams = array of TRTTIParameter;

  TILUnit = class;


  TILArgument = class;

  PILType = ^TIMGType;

  TILProc = class
  private
    VarsTmp: array of PILVariable;// список временных переменных
    function GetILCount: Integer; inline;
  public
  type
    TILProcedures = array of TILProc;
  var
    Name: string;                   // название(для отладки)
    Struct: PILType;                // структруа (если метод)
    ILUnit: TILUnit;                // Модуль в котором обьявлена процедура
    Flags: UInt8;                   // Флаги
    Params: TParams;                // Параметры
    Vars: TILVariables;             // Локальные переменные, включая параметры
    ProcType: TProcType;            // Тип процедуры (процедура/функция)
    NestedProcs: TILProcedures;     // Локальные процедуры
    CodeSize: UInt32;               // Размер машинного кода в байтах
    IL: TILInstructions;            // IL-инструкции (дебаг/коррекция смещений переходов)
    InvokeAdapterID: Integer;       // ID адаптера вызова импортируемой процедуры
    ImportIndex: Integer;           // индекс из таблицы импорта (-1 не является импортируемой)
    ProcInfo: TOffset;              // RTTI процедуры
    // поля продублированные из RTTI
    Offset: TOffset;                // смещение начала кода процедуры в образе
    StackSize: Integer;             // Размер стека (память локальных переменных)
    ImportLib: TOffset;             // библиотека импорта (0 если нет)
    ImportName: TOffset;            // имя в библиотеке импорта (0 если нет)
    CallConvention: TCallConvention;// натация вызова
    VirtualIndex: Integer;
    ExportIndex: Integer;
    IsIntfMethod: Boolean;
    SelfArg: TILArgument;
    function IsImported: Boolean; inline;
    function GetTMPVar(DataTypeID: TDataTypeID; IsReference: Boolean): PILVariable;
    function GetStackSize: Integer;
  public
    destructor Destroy; override;
    property ILCount: Integer read GetILCount;
  end;

  TILProcedures = TILProc.TILProcedures;

  TIMGType = packed record
    ID: TDataTypeID;        // ID типа
    Name: string;           // название типа
    Offset: TOffset;        // смещение в секции RTTI
    StrictRTTI: TOffset;    // смещение в урезанной секции RTTI
    Fields: TILVariables;   // поля
    Methods: TILProcedures; // методы
  end;

  TIMGTypes = array of TIMGType;

  TExportUnits = array [0..65535] of TRTTIUnit;
  PExportUnits = ^TExportUnits;

  TUInt32DynArray = array of UInt32;
  PUInt32DynArray = TUInt32DynArray;

  TConstInfoRec = packed record
    TypeInfo: TOffset;
    Value: TOffset;
    Size: Integer;
  end;
  PConstInfoRec =^TConstInfoRec;
  TConstInfoArray = array of TConstInfoRec;

  TILBreakPoint = record
    SrcTextLine: Integer;   // строка в исходном коде
    VMTextLine: Integer;    // строка в target-asm коде
    Offset: TOffset;        // смещение в target коде
  end;

  TILBreakPoints = array of TILBreaKPoint;

  // модули
  TILUnit = class
  public
    Name: string;                // название модуля
    Flags: Integer;
    Index: Integer;              // индекс в пакете
    Consts: TConstInfoArray;     // таблица смещений констант модуля
    Procs: TILProcedures;          // процедуры
    Vars: TILVariables;            // глобальные переменные
    ExportProcsCount: Integer;   // кол-во экспортируемых процедур
    VMVars: PVMVariables;        // глобальные переменные модуля
    VMUnit: PRTTIUnit;           // ссылка на RTTI описание в VM образе
    Types: TIMGTypes;            // типы обьявленные в модуле
    BreakPoints: TILBreakPoints; // точки останова
    InitProc: TILProc;           // секция инициализации
    FinalProc: TILProc;
    destructor Destroy; override;          // секция финализации
  end;

  // тип аргумента инструкции
  TILArgumentType = (
    atImmConst,  // аргумент задан константой (непосредственной или табличной)
    atLocal,     // аргумент задан локальной переменной
    atGlobal,    // аргумент задан глобальной переменной
    atField,     // аргумент задан полем структуры
    atReference  // аргумент задан ссылкой (ссылка может быть только локальной)
  );

  TILMethod = record
    Self: TILArgument;
    Proc: TILProc;
  end;
  PILMethod = ^TILMethod;

  // контекст трансляции
  TILTContext = record
    PUnit: TILUnit;       // текущий модуль
    Proc: TILProc;        // текущая процедура
    Cond: TILCondition;   // условие текущей IL инструкции
    Stream: TStream;      // входной поток
    ILCode: TILCode;      // код IL инструкции
    ILIndex: Integer;     // индекс IL инструкции
    ILCount: Integer;
    Args: TILArgs;        // аргументы IL инструкции
  end;

  TILUnits = array of TILUnit;

  TCallType = (
    CallStatic,
    CallStaticExternal,
    CallIndirect,
    CallMethod,
    CallMethodIndirect,
    CallMethodVirtual,
    CallMethodInterface,
    CallMethodExternal
  );

  TVMArgType = (
    VMARG_IMM,
    VMARG_VAR
  );

  TVMCodeClass = (
    vmStore,
    vmClear,
    vmIncRef,
    vmDecRef
  );

  TILArgData = record
    case Integer of
      0: (I8: Int8);
      1: (I16: Int16);
      2: (I32: Int32);
      3: (I64: Int64);
      4: (NInt: NativeInt);
      5: (U8: UInt8);
      6: (U16: UInt16);
      7: (U32: UInt32);
      8: (U64: UInt64);
      9: (NUInt: NativeUInt);
     10: (PTR: Pointer);
     11: (AsVariable: PILVariable);
     12: (AsProcedure: TILProc);
     13: (AsTypeInfo: PRTTIType);
     14: (AsMethod: TILMethod);
  end;

  TILTranslator = class;

  TVMCodeArg = class
  protected
  public
    function GetDataCnt(MT: TILTranslator): Integer; virtual;
    procedure WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt); virtual; abstract;
  end;

  TVMCodeArgGeneric<T> = class(TVMCodeArg)
  private
    FValue: T;
  public
    property Value: T read FValue;
    constructor Create(const Value: T);
  end;

  TILArgument = class(TVMCodeArg)
  private
    FData: TILArgData;
  public
    ArgumentType: TILArgumentType;
    ArgClass: TILARG_CLASS;
    ArgScope: TILARG_SCOPE;
    TypeInfo: TOffset;
    Next: TILArgument;
    property I8: Int8 read FData.I8 write FData.I8;
    property I16: Int16 read FData.I16 write FData.I16;
    property I32: Int32 read FData.I32 write FData.I32;
    property I64: Int64 read FData.I64 write FData.I64;
    property NInt: NativeInt read FData.NInt write FData.NInt;
    property U8: UInt8 read FData.U8 write FData.U8;
    property U16: UInt16 read FData.U16 write FData.U16;
    property U32: UInt32 read FData.U32 write FData.U32;
    property U64: UInt64 read FData.U64 write FData.U64;
    property NUInt: NativeUInt read FData.NUInt write FData.NUInt;
    property PTR: Pointer read FData.PTR write FData.PTR;
    property AsVariable: PILVariable read FData.AsVariable write FData.AsVariable;
    property AsProcedure: TILProc read FData.AsProcedure write FData.AsProcedure;
    property AsTypeInfo: PRTTIType read FData.AsTypeInfo write FData.AsTypeInfo;
    property AsMethod: TILMethod read FData.AsMethod write FData.AsMethod;
    function IsLocalVar: Boolean;
    function IsReference: Boolean; inline;
    function IsResultVar: Boolean; inline;
    function GetVarOffset: TOffset;
    function GetDataCnt(MT: TILTranslator): Integer; override;
    procedure Clear;
    constructor Create;
    constructor CreateFromVar(Variable: PILVariable);
    destructor Destroy; override;
    procedure WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt); override;
  end;

  TVMCodeArgILType = class(TVMCodeArgGeneric<PILType>)
  public
    procedure WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt); override;
  end;

  TVMCAILProc = class(TVMCodeArgGeneric<TILProc>)
  public
    procedure WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt); override;
  end;

  TVMCAProcRtti = class(TVMCodeArgGeneric<TILProc>)
  public
    procedure WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt); override;
  end;

  TVMCodeLine = record
    SrcTextLine: Integer;
    AsmTextLine: Integer;
    UnitID: Integer;
    Offset: Integer;
  end;
  TVMCodeLines = TList<TVMCodeLine>;

  TStrLiteral = record
    DTID: TDataTypeID;
    Offset: TOffset;
  end;

  TStrLiteralsArray = array of TStrLiteral;

  TRTTIClasses = record
    _trtti: TOffset;
    _trttiunit: TOffset;
    _trttiordinal: TOffset;
    _trttiarray: TOffset;
    _trttiset: TOffset;
    _trttidynarray: TOffset;
    _trttifloat: TOffset;
    _trttipointer: TOffset;
    _trttivariant: TOffset;
    _trttirecord: TOffset;
    _trtticlass: TOffset;
    _trttiinterface: TOffset;
    _trttiproctype: TOffset;
    _trttiprocedure: TOffset;
  end;

  TReadTypeContext = record
    Stream: TStream;
    ILUnit: TILUnit;
    ILType: PILType;
    TypeName: TOffset;
  end;

  TIL_D_Args = class(TILArgs)
    D: TILArgument;
  end;

  TIL_DS_Args = class(TILArgs)
    D: TILArgument;
    S: TILArgument;
  end;

  TIL_LR_Args = class(TILArgs)
    L: TILArgument;
    R: TILArgument;
  end;

  TIL_CMP_ARGS = TIL_LR_Args;
  TIL_CMPJ_ARGS = TIL_LR_Args;
  TIL_TEST_ARGS = TIL_LR_Args;
  TIL_CONVERT_ARGS = TIL_DS_Args;

  TIL_DSS_Args = class(TILArgs)
    D: TILArgument;
    L: TILArgument;
    R: TILArgument;
  end;

  TIL_LEA_ARGS = class(TILArgs)
    D: TILArgument;
    B: TILArgument;
    Offset: TILArgument;
  end;
  TIL_ARRDALLOC_ARGS = TIL_DS_Args;
  TIL_ARRAYLENGTH_ARGS = TIL_DS_Args;
  TIL_TYPEINFO_ARGS = TIL_DS_Args;
  TIL_QUERYTYPE_ARGS = TIL_DSS_Args;
  TIL_MOVE_ARGS = TIL_DS_Args;
  TIL_MOVEZERO_ARGS = TIL_D_Args;
  TIL_NOT_ARGS = TIL_DS_Args;
  TIL_INC_ARGS = TIL_D_Args;
  TIL_GETBIT_ARGS = class(TILArgs)
    Dst, BitArray, BitIndex: TILArgument;
  end;
  TIL_SETBIT_ARGS = class(TILArgs)
    Dst, BitIndex, BitValue: TILArgument;
  end;
  TIL_SETBOOL_ARGS = TIL_D_Args;
  TIL_ETHROW_ARGS = TIL_D_Args;
  TIL_STRONGREF_ARGS = TIL_DS_Args;
  TIL_WEAKREF_ARGS = TIL_DS_Args;
  TIL_DNEW_ARGS = TIL_D_Args;
  TIL_DNEWOBJ_ARGS = TIL_DS_Args;
  TIL_DFREE_ARGS = TIL_D_Args;
  TIL_INCREF_ARGS = TIL_D_Args;
  TIL_DECREF_ARGS = TIL_D_Args;
  TIL_DECREFFINAL_ARGS = class(TILArgs)
    Dst: TILArgument;
    FinalProc: TILArgument;
  end;
  TIL_INIT_ARGS = TIL_D_Args;
  TIL_JMP_ARGS = TIL_D_Args;
  TIL_MEMSET_ARGS = class(TILArgs)
    Dst, Bpt: TILArgument;
  end;
  TIL_INHTCALL_ARGS = class(TILArgs)
    ProcArg: TILArgument;
    TypeArg: TILArgument;
    SelfArg: TILArgument;
  end;
  TIL_GETPTR_ARGS = TIL_DS_Args;
  TIL_GETSPTR_ARGS = class(TILArgs)
    D, B, S: TILArgument;
  end;

  TIL_LDMETHOD_ARGS = TIL_DS_Args;
  TIL_LDSMETHOD_ARGS = class(TILArgs)
    D, B, S: TILArgument;
  end;

  TIL_NEARCALL_ARGS = TIL_D_Args;

  TIL_ARRAYCOPY_ARGS = class(TILArgs)
    D, S, F, L: TILArgument;
  end;

  TIL_RDREF_ARGS = TIL_DS_Args;
  TIL_WDREF_ARGS = TIL_DS_Args;

  TIL_ARRAYMOVE_ARGS = class(TILArgs)
    SrcArr: TILArgument;
    SrcIdx: TILArgument;
    DstArr: TILArgument;
    DstIdx: TILArgument;
    Cnt: TILArgument;
  end;

  TIL_CHCKBND_ARGS = class(TILArgs)
    AArray: TILArgument;
    AIndex: TILArgument;
  end;

  TILArguments = array of TILArgument;

  TIL_PCALL_ARGS = class(TILArgs)
    PArg: TILArgument;
    CallArgs: TILArguments;
  end;

  TIL_FMACRO_ARGS = class(TILArgs)
    M, D: TILArgument;
  end;

  {ветвление}
  TILBranch = record
    CmpArgs: TIL_LR_Args;  // аргументы инструкции сравнения
    CmpCond: TILCondition; // условие сравнения
    ElseIdx: Integer;      // индекс первой инструкции else секции
    EndIdx: Integer;       // индекс последней инструкции ветвления if/case/else
  end;
  PILBranch = ^TILBranch;

  {контекст ветвлений}
  TILBranchContext = record
  type
    TILBranchStack = TPool<TILBranch>;
  private
    FItems: TILBranchStack;
    function GetLast: PILBranch;
    function GetCount: Integer;
  public
    procedure Init;
    procedure Add(const Args: TIL_LR_Args);
    procedure BrClose;
    property Last: PILBranch read GetLast;
    property Count: Integer read GetCount;
  end;
  PILBranchContext = ^TILBranchContext;

  {базовый класс IL транслятора}
  TILTranslator = class
  private
    FIncludeDebugInfo: Boolean;
    FIncludeRTTI: Boolean;
    FIMG: TILMemoryStream;
    procedure SetRTTICharset(const Value: TRTTICharset);
  protected
    FIMGHeader: PIMGHeader;
    FUnits: TILUnits;
    VMUnits: PRTTIUnitsSArray;
    FSystemTypes: TIMGTypes;                // встроенные типы
    FRTTI: TILMemoryStream;                 // RTTI сборки
    FRTTIProcs: TILMemoryStream;            // RTTI процедур сборки
    FRTTICharset: TRTTICharset;             // набор символов для RTTI
    FRTTIClassTypes: TRTTIClasses;          // classtypes для RTTI
    FConstsSize: UInt32;
    FStrLiterals: TStrLiteralsArray;        // таблица смещений строковых литералов сборки
    FStrLiteralsValues: TStrArray;          // таблица значений строковых литералов сборки
    FInterfaceCnt: Integer;
    FInterfaceID: Integer;
    FInterfaces: TList<PRTTIInterface>;
    FImportTableCount: Integer;
    FArgs: TObjectsPool<TVMCodeArg>;
    FCodeAlign: Integer;                    // выравнивание кода
    FDataAlign: Integer;                    // выравнивание данных
    procedure LoadILUnitDecl(Index: Integer; Stream: TStream); virtual;
    procedure LoadILUnitBody(ILUnit: TILUnit; Stream: TStream); virtual;
    procedure LoadILTypes(ILUnit: TILUnit; Stream: TStream); virtual;
    procedure LoadILConsts(ILUnit: TILUnit; Stream: TStream); virtual; abstract;
    procedure LoadILSimpleConst(Stream: TStream; DataType: PRTTIType); virtual;
    procedure LoadILRecordConst(ILUnit: TILUnit; Stream: TStream; CItem: PConstInfoRec); virtual;
    procedure LoadILGlobalVars(ILUnit: TILUnit; Stream: TStream); virtual;
    procedure LoadILProcDecls(Stream: TStream; ILUnit: TILUnit; ParentProc: TILProc; out Procedures: TILProcedures); virtual;
    procedure LoadILProcBodies(Stream: TStream; ILUnit: TILUnit; ParentProc: TILProc; const Procedures: TILProcedures); virtual;
    procedure LoadILProcBody(Stream: TStream; Proc: TILProc); virtual;
    procedure LoadILParameters(Stream: TStream; Proc: TILProc); virtual;
    procedure LoadILParameter(ILUnit: TILUnit; Stream: TStream; ParamsCount: Integer; Params: PRTTIParams); virtual;
    procedure LoadILLocalVars(Proc: TILProc; Stream: TStream; var MemSize: Integer; StartVarIndex, VarCount: Integer); virtual;
    procedure LoadILMethodsBodies(ILUnit: TILUnit; Stream: TStream); virtual;
    procedure LoadILArgumentInternal(const Context: TILTContext; out Arg: TILArgument; PrevArg: TILArgument); virtual;
    procedure LoadILArgument(const Context: TILTContext; out Arg: TILArgument);
    procedure LoadILMethodArg(const Context: TILTContext; SelfArg: TILArgument; var Arg: TILArgument);

    procedure CheckListArg(const Context: TILTContext; var Arg: TILArgument); virtual;
    procedure AfterLoadILGlobalVars(ILUnit: TILUnit); virtual;
    procedure AddFixOffset(Offset: TOffset); virtual;

    function GetTMPVar(const Context: TILTContext; DataTypeID: TDataTypeID; IsReference: Boolean): PILVariable;
    function GetUnitsCount: Integer;
    function GetString(const Name: TOffset): UnicodeString;
    function GetUnit(Index: Integer): TILUnit; inline;
    function GetILType(UnitID, TypeID: Integer): PILType; inline;
    function GetIMGPtr(const offset: TOffset): Pointer; inline;
    function GetILMethod(Struct: PRTTIStruct; MethodIndex: Integer): TILProc;
    function GetILField(Struct: PRTTIStruct; FieldIndex: Integer): PILVariable;
    function GetParamDataSize(RTTI: PRTTIField): Integer;
    function GetProcName(Proc: TILProc): string;
    function GetSysTypeInfo(SysTypeIndex: Integer): PRTTIType; inline;
    function GetTypeInfo(const ARG: TILArgument): PRTTIType; overload; inline;
    function GetProcInfo(const ProcInfoOffset: TOffset): PRTTIProcedure; overload; inline;
    function GetProcInfo(const Proc: TILProc): PRTTIProcedure; overload; inline;

    function ArgGet: TILArgument; overload;
    function ArgGet(const ILVar: PILVariable): TILArgument; overload;
    function ArgGet(const ILType: PILType): TVMCodeArg; overload;
    function ArgGet(const ILProc: TILProc): TVMCodeArg; overload;
    function GetArgProcRtti(const ILProc: TILProc): TVMCodeArg; overload;
    function CreateILUnit(const Name: string): TILUnit; virtual;
    function CreateILUnitProc(ILUnit: TILUnit): TILProc; virtual;
    function PrepareCALLInfo(const ProcArg: TILArgument; out ParamsCount: Integer; out Params: PRTTIParams; out ResultType: PRTTIType;
                             out ADDR: NativeUInt): TCallType;
    function PassByRef(const Param: PRTTIParameter): Boolean;
    function ReadVarStaticArrayValue(Stream: TStream; const Variable: PILVariable; ArrType: PRTTIArray): Pointer;
    function ReadGlobalVarValue(Stream: TStream; const Variable: PILVariable): Pointer;
    function ReadTypeSpecInfo(Stream: TStream): TOffset;
    // функция вычитывает информацию о типе
    function ReadVarType(ILUnit: TILUnit; Stream: TStream; VarFlags: Integer): TOffset;
    procedure ReadImportInfo(Stream: TStream; TypeInfo: PRTTIType);
    procedure ReadILFields(ILUnit: TILUnit; Stream: TStream; Struct: PRTTIStruct; ILType: PILType); virtual;
    procedure ReadILMethodDecl(ILUnit: TILUnit; Stream: TStream; ProcInfo: PRTTIProcedure); virtual;
    procedure ReadILMethodsDecls(ILUnit: TILUnit; Stream: TStream; Struct: PRTTIStruct; var Procedures: TILProcedures); virtual;
    procedure RTTIProcToILProc(RTTIProc: PRTTIProcedure; Proc: TILProc);
    //////////////////////////////////////////////////////////////////////////////////////////
    ///  Функции загрузки RTTI
    procedure ReadOrdinalTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadSetTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadArrayTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadDynArrayTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadRecordTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadClassTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadIntfTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadPointerType(var Context: TReadTypeContext); virtual;
    procedure ReadWeakRefType(var Context: TReadTypeContext); virtual;
    procedure ReadClassOfType(var Context: TReadTypeContext); virtual;
    procedure ReadProcTypeInfo(var Context: TReadTypeContext); virtual;
    procedure ReadClassIntfaces(var Context: TReadTypeContext; var ClassType: PRTTIClass); virtual;

    procedure ReadILCode(Stream: TStream; Proc: TILProc); virtual;
    procedure CorrectProcJMPOffsets(Proc: TILProc); virtual;
    procedure BeforeInstruction(const Ctx: TILTContext); virtual;
    procedure AfterInstruction(const Ctx: TILTContext); virtual;

    procedure Translate_NOPE(const Ctx: TILTContext); virtual; abstract;
    procedure Translate_2OperandOpCode(const Ctx: TILTContext; var Args: TIL_DS_Args); virtual; abstract;
    procedure Translate_3OperandOpCode(const Ctx: TILTContext; var Args: TIL_DSS_Args); virtual; abstract;
    procedure Translate_LDPTR_Instruction(const Ctx: TILTContext; var Dst: TILArgument; const Src: TILArgument); virtual; abstract;
    procedure Translate_LEA(const Ctx: TILTContext; var Args: TIL_LEA_Args); virtual; abstract;
    procedure Translate_ARRDALLOC(const Ctx: TILTContext; var Args: TIL_ARRDALLOC_ARGS); virtual; abstract;
    procedure Translate_ARRAYLENGTH(const Ctx: TILTContext; var Args: TIL_ARRAYLENGTH_ARGS); virtual; abstract;
    procedure Translate_ARRAYCOPY(const Ctx: TILTContext; var Args: TIL_ARRAYCOPY_ARGS); virtual; abstract;
    procedure Translate_ARRAYMOVE(const Ctx: TILTContext; var Args: TIL_ARRAYMOVE_ARGS); virtual; abstract;
    procedure Translate_TYPEINFO(const Ctx: TILTContext; var Args: TIL_TYPEINFO_ARGS); virtual; abstract;
    procedure Translate_QUERYTYPE(const Ctx: TILTContext; var Args: TIL_QUERYTYPE_ARGS); virtual; abstract;
    procedure Translate_UNIQUE(const Ctx: TILTContext; var Args: TIL_D_Args); virtual; abstract;
    procedure Translate_CMP(const Ctx: TILTContext; var Args: TIL_CMP_ARGS); virtual; abstract;
    procedure Translate_CMPJ(const Ctx: TILTContext; var Args: TIL_CMPJ_ARGS); virtual; abstract;
    procedure Translate_TEST(const Ctx: TILTContext; var Args: TIL_TEST_ARGS); virtual; abstract;
    procedure Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_CONVERT_ARGS); virtual; abstract;
    procedure Translate_MOVE(const Ctx: TILTContext; var Args: TIL_MOVE_ARGS); virtual; abstract;
    procedure Translate_MOVEZERO(const Ctx: TILTContext; var Args: TIL_MOVEZERO_ARGS); virtual; abstract;
    procedure Translate_NOT(const Ctx: TILTContext; var Args: TIL_NOT_ARGS); virtual; abstract;
    procedure Translate_INC(const Ctx: TILTContext; var Args: TIL_INC_ARGS); virtual; abstract;
    procedure Translate_GETBIT(const Ctx: TILTContext; var Args: TIL_GETBIT_ARGS); virtual; abstract;
    procedure Translate_SETBIT(const Ctx: TILTContext; var Args: TIL_SETBIT_ARGS); virtual; abstract;
    procedure Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_SETBOOL_ARGS); virtual; abstract;
    procedure Translate_TRYBEGIN(const Ctx: TILTContext); virtual; abstract;
    procedure Translate_TRYEND(const Ctx: TILTContext); virtual; abstract;
    procedure Translate_NEARCALL(const Ctx: TILTContext; var Args: TIL_NEARCALL_ARGS); virtual; abstract;
    procedure Translate_ETHROW(const Ctx: TILTContext; var Args: TIL_ETHROW_ARGS); virtual; abstract;
    procedure Translate_INCREF(const Ctx: TILTContext; var Args: TIL_INCREF_ARGS); virtual; abstract;
    procedure Translate_DECREF(const Ctx: TILTContext; var Args: TIL_DECREF_ARGS); virtual; abstract;
    procedure Translate_DECREFFINAL(const Ctx: TILTContext; var Args: TIL_DECREFFINAL_ARGS); virtual; abstract;
    procedure Translate_INIT(const Ctx: TILTContext; var Args: TIL_INIT_ARGS); virtual; abstract;
    procedure Translate_WEAKREF(const Ctx: TILTContext; var Args: TIL_WEAKREF_ARGS); virtual; abstract;
    procedure Translate_STRONGREF(const Ctx: TILTContext; var Args: TIL_STRONGREF_ARGS); virtual; abstract;
    procedure Translate_MEMSET(const Ctx: TILTContext; var Args: TIL_MEMSET_ARGS); virtual; abstract;
    procedure Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS); virtual; abstract;
    procedure Translate_CHKBND(const Ctx: TILTContext; var Args: TIL_CHCKBND_ARGS); virtual; abstract;
    procedure Translate_MEMGET(const Ctx: TILTContext; var Args: TIL_DNEW_ARGS); virtual; abstract;
    procedure Translate_MEMFREE(const Ctx: TILTContext; var Args: TIL_DFREE_ARGS); virtual; abstract;
    procedure Translate_DNEWOBJ(const Ctx: TILTContext; var Args: TIL_DNEWOBJ_ARGS); virtual; abstract;
    procedure Translate_JMP(const Ctx: TILTContext; var Args: TIL_JMP_ARGS); virtual; abstract;
    procedure Translate_RET(const Ctx: TILTContext); virtual; abstract;
    procedure Translate_GETPTR(const Ctx: TILTContext; var Args: TIL_GETPTR_ARGS); virtual; abstract;
    procedure Translate_LDMETHOD(const Ctx: TILTContext; var Args: TIL_LDMETHOD_ARGS); virtual; abstract;
    procedure Translate_LDSMETHOD(const Ctx: TILTContext; var Args: TIL_LDSMETHOD_ARGS); virtual; abstract;
    procedure Translate_GETSPTR(const Ctx: TILTContext; var Args: TIL_GETSPTR_ARGS); virtual; abstract;
    procedure Translate_RDREF(const Ctx: TILTContext; var Args: TIL_RDREF_ARGS); virtual; abstract;
    procedure Translate_WDREF(const Ctx: TILTContext; var Args: TIL_WDREF_ARGS); virtual; abstract;
    procedure Translate_REFCNT(const Ctx: TILTContext; var Args: TIL_DS_ARGS); virtual; abstract;
    procedure Translate_NOW(const Ctx: TILTContext; var Args: TIL_D_Args); virtual; abstract;
    procedure Translate_FMACRO(const Ctx: TILTContext; var Args: TIL_FMACRO_ARGS); virtual; abstract;
    procedure Translate_LDMETHODPTR_Instruction(const Ctx: TILTContext; const Dst, Method: TILArgument); virtual; abstract;

    procedure Read_2OperandOpCode(var Ctx: TILTContext);
    procedure Read_3OperandOpCode(var Ctx: TILTContext);
    procedure Read_LEA(var Ctx: TILTContext);
    procedure Read_ARRDALLOC(var Ctx: TILTContext);
    procedure Read_ARRAYLENGTH(var Ctx: TILTContext);
    procedure Read_ARRAYCOPY(var Ctx: TILTContext);
    procedure Read_ARRAYMOVE(var Ctx: TILTContext);
    procedure Read_TYPEINFO(var Ctx: TILTContext);
    procedure Read_QUERYTYPE(var Ctx: TILTContext);
    procedure Read_UNIQUE(var Ctx: TILTContext);
    procedure Read_CMP(var Ctx: TILTContext);
    procedure Read_CMPJ(var Ctx: TILTContext);
    procedure Read_TEST(var Ctx: TILTContext);
    procedure Read_CONVERT(var Ctx: TILTContext);
    procedure Read_MOVE(var Ctx: TILTContext);
    procedure Read_MOVEZERO(var Ctx: TILTContext);
    procedure Read_NOT(var Ctx: TILTContext);
    procedure Read_INC(var Ctx: TILTContext);
    procedure Read_GETBIT(var Ctx: TILTContext);
    procedure Read_SETBIT(var Ctx: TILTContext);
    procedure Read_SETBOOL(var Ctx: TILTContext);
    procedure Read_TRYBEGIN(var Ctx: TILTContext);
    procedure Read_TRYEND(var Ctx: TILTContext);
    procedure Read_NEARCALL(var Ctx: TILTContext);
    procedure Read_ETHROW(var Ctx: TILTContext);
    procedure Read_INCREF(var Ctx: TILTContext);
    procedure Read_DECREF(var Ctx: TILTContext);
    procedure Read_DECREFFINAL(var Ctx: TILTContext);
    procedure Read_INIT(var Ctx: TILTContext);
    procedure Read_WEAKREF(var Ctx: TILTContext);
    procedure Read_STRONGREF(var Ctx: TILTContext);
    procedure Read_MEMSET(var Ctx: TILTContext);
    procedure Read_INHT_CALL(var Ctx: TILTContext);
    procedure Read_PCALL(var Ctx: TILTContext);
    procedure Read_CHKBND(var Ctx: TILTContext);
    procedure Read_MEMGET(var Ctx: TILTContext);
    procedure Read_MEMFREE(var Ctx: TILTContext);
    procedure Read_DNEWOBJ(var Ctx: TILTContext);
    procedure Read_JMP(var Ctx: TILTContext);
    procedure Read_RET(var Ctx: TILTContext);
    procedure Read_GETPTR(var Ctx: TILTContext);
    procedure Read_LDMETHOD(var Ctx: TILTContext);
    procedure Read_LDSMETHOD(var Ctx: TILTContext);
    procedure Read_GETSPTR(var Ctx: TILTContext);
    procedure Read_RDREF(var Ctx: TILTContext);
    procedure Read_WDREF(var Ctx: TILTContext);
    procedure Read_REFCNT(var Ctx: TILTContext);
    procedure Read_NOW(var Ctx: TILTContext);
    procedure Read_FMACRO(var Ctx: TILTContext);

    procedure WriteProcProlog(Proc: TILProc); virtual;
    procedure WriteProcEpilog(Proc: TILProc; LastILCode: TILCode); virtual;

    procedure WriteVMExportProcs(ILUnit: TILUnit); virtual;
    procedure WriteILUnitProlog(Stream: TStream; ILUnit: TILUnit); virtual;
    procedure WriteILUnitEpilog(Stream: TStream; ILUnit: TILUnit); virtual;
    procedure WriteProcsRTTI(ILUnit: TILUnit); virtual;
    procedure WriteTypesRTTIArray(ILUnit: TILUnit); virtual; abstract;
    procedure WriteUnitsRTTIArray(UnitsCount: Integer); virtual;
    procedure WriteProcLocalVarDebugInfo(Proc: TILProc); virtual;

    procedure MapImportMethod(aType: PRTTIType; Proc: TILProc); virtual; abstract;
    procedure MapImportProc(Proc: TILProc); virtual; abstract;
    procedure MapImportType(aType: PRTTIType); virtual;
    procedure MapRTTIClassTypes; virtual;
    procedure MakeImportTable; virtual; abstract;
    procedure SetSystemTypesOffsets(Types: PSystemTypes; Offsets: TIMGTypes);
    procedure CorrectOffsets; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /////////////////////////////////
    property IMG: TILMemoryStream read FIMG;
    property IncludeRTTI: Boolean read FIncludeRTTI write FIncludeRTTI;
    property IncludeDebugInfo: Boolean read FIncludeDebugInfo write FIncludeDebugInfo;
    property Units[Index: Integer]: TILUnit read GetUnit;
    property UnitsCount: Integer read GetUnitsCount;
    property RTTICharset: TRTTICharset read FRTTICharset write SetRTTICharset;
    function GetTypeInfo(const TypeInfoOffset: TOffset): PRTTIType; overload; inline;
    procedure LoadILCode(Stream: TStream); overload; virtual;
    procedure LoadILCode(const Path: string); overload; virtual;
    procedure SaveTargetCode(Stream: TStream); overload; virtual; abstract;
    procedure SaveTargetCode(const Path: string); overload; virtual;
  end;

procedure AbortWork(const Message: string; const Params: array of const); overload;
procedure AbortWork(const Message: string); overload;
procedure AbortWorkNotSupported(const Message: string = '');

implementation

function MemAlign(Value: Integer; Align: Integer = 4): Integer; overload;
var
  M: Integer;
begin
  M := (Value mod Align);
  if M = 0 then
    Result := Value
  else
    Result := Value + (Align - M);
end;

procedure AbortWork(const Message: string; const Params: array of const); overload;
begin
  raise Exception.CreateFmt(Message, Params);
end;

procedure AbortWork(const Message: string); overload;
begin
  raise Exception.Create(Message);
end;

procedure AbortWorkNotSupported(const Message: string = '');
begin
  AbortWork(Message + 'not supportd');
end;

function TILProc.GetTMPVar(DataTypeID: TDataTypeID; IsReference: Boolean): PILVariable;
var
  i, c: Integer;
begin
  c := Length(VarsTmp);
  for i := 0 to c - 1 do
  begin
    Result := VarsTmp[i];
    if not Result.TmpUsed then
    begin
      Result.TmpUsed := True;
      Exit;
    end;
  end;

  SetLength(VarsTmp, c + 1);
  New(Result);
  VarsTmp[c] := Result;
  Result.VarScope := ARG_SCOPE_LOCAL;
  Result.VarClass := VarMutable;
  Result.TmpUsed := True;
  Result.RTTI.Name := 0;
  Result.RTTI.IsReference := IsReference;
  Result.AbsoluteTo := nil;
  if c > 0 then begin
    Result.RTTI.Offset := VarsTmp[c - 1].RTTI.Offset + MemAlign(cDataTypeSizes[DataTypeID]);
  end else begin
    Result.RTTI.Offset := StackSize;
  end;
end;

destructor TILProc.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(VarsTmp) - 1 do
    Dispose(VarsTmp[i]);

  for i := 0 to Length(NestedProcs) - 1 do
    NestedProcs[i].Free;

  // освобождаем аргументы
  for i := 0 to Length(IL) - 1 do
    IL[i].Args.Free;
end;

function TILProc.GetILCount: Integer;
begin
  Result := Length(IL);
end;

function TILProc.GetStackSize: Integer;
var
  c: Integer;
begin
  Result := StackSize;
  c := Length(VarsTmp);
  if c > 0 then
    Result := VarsTmp[c - 1].RTTI.Offset + PTR_SIZE;
end;

function TILProc.IsImported: Boolean;
begin
  Result := (ImportLib > 0);
end;

{ TVMCodeArg }

function TVMCodeArg.GetDataCnt(MT: TILTranslator): Integer;
begin
  Result := 1;
end;

{ TVMCodeArgILType }

procedure TVMCodeArgILType.WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt);
begin
  if MT.IncludeRTTI then
    IP^ := FValue.Offset
  else
    IP^ := FValue.StrictRTTI;
end;

{ TVMCAILProc }

procedure TVMCAILProc.WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt);
begin
  IP^ := FValue.Offset;
end;

{ TVMCodeArgGeneric<T> }

constructor TVMCodeArgGeneric<T>.Create(const Value: T);
begin
  FValue := Value;
end;

{ TVMCAProcRtti }

procedure TVMCAProcRtti.WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt);
begin
  IP^ := FValue.ProcInfo;
end;

{ TILArgument }

procedure TILArgument.Clear;
begin
  FData.I64 := 0;
  ArgumentType := atImmConst;
  ArgClass := ARG_VAR;
  ArgScope := ARG_SCOPE_LOCAL;
  TypeInfo := 0;
  Next := nil;
end;

function TILArgument.IsLocalVar: Boolean;
begin
  Result := (ArgClass = ARG_VAR) and (ArgScope = ARG_SCOPE_LOCAL) and
            ((NUInt > 10000) and not AsVariable.RTTI.IsReference);
end;

function TILArgument.IsReference: Boolean;
begin
  Result := ArgumentType = atReference;
end;

function TILArgument.IsResultVar: Boolean;
begin
  Result := (ArgClass = ARG_VAR) and AsVariable.IsResult;
end;

procedure TILArgument.WriteData(MT: TILTranslator; const Proc: TILProc; IP: PNativeUInt);
var
  Offset: NativeUInt;
  ti: PRTTIType;
begin
  case ArgClass of
    ARG_VAR: Offset := FData.AsVariable.RTTI.Offset;
    ARG_PROC: Offset := FData.AsProcedure.Offset;
    ARG_SCONST, ARG_TCONST, ARG_SIZEOF: Offset := FData.NUInt;
    ARG_CONST: begin
      ti := MT.GetTypeInfo(TypeInfo);
      if ti.DataSize = 8 then
      begin
        PInt64(IP)^ := FData.I64;
        Exit;
      end;
      Offset := FData.NUInt;
    end;
    ARG_TYPE: Offset := TypeInfo;
  else
    assert(false);
    Exit;
  end;
  IP^ := Offset;
end;

constructor TILArgument.Create;
begin

end;

constructor TILArgument.CreateFromVar(Variable: PILVariable);
begin
  FData.AsVariable := Variable;
  ArgClass := ARG_VAR;
  ArgScope := Variable.VarScope;
end;

destructor TILArgument.Destroy;
begin

  inherited;
end;

function TILArgument.GetDataCnt(MT: TILTranslator): Integer;
var
  ti: PRTTIType;
begin
  if (ArgClass = ARG_CONST) then
  begin
   ti := MT.GetTypeInfo(TypeInfo);
   if ti.DataSize = 8 then
     Exit(2);
  end;
  Result := 1;
end;

function TILArgument.GetVarOffset: TOffset;
begin
  Result := AsVariable.RTTI.Offset
end;

{ TILTranslator }

procedure TILTranslator.CheckListArg(const Context: TILTContext; var Arg: TILArgument);
var
  Src: TILArgument;
begin
  Src := Arg;
  // аллокация временной переменной в Arg
  Arg := ArgGet();
  Arg.ArgumentType := atReference;
  Arg.ArgScope := ARG_SCOPE_LOCAL;
  Arg.Next := nil;
  Arg.AsVariable := GetTMPVar(Context, dtPointer, True);

  // инструкция загрузки адреса
  Translate_LDPTR_Instruction(Context, Arg, Src);
  // освобождаем если небыло надобности
  if Arg.NUInt = Src.NUInt then
    Arg.AsVariable.TmpUsed := False;

  // освобождаем цепочку
  //ArgReturn(Src);
end;

procedure TILTranslator.CorrectOffsets;
begin

end;

procedure TILTranslator.CorrectProcJMPOffsets(Proc: TILProc);
begin

end;

constructor TILTranslator.Create;
begin
  FIMG := TILMemoryStream.Create;
  FRTTI := TILMemoryStream.Create;
  FRTTIProcs := TILMemoryStream.Create;
  FInterfaces := TList<PRTTIInterface>.Create;
  FRTTICharset := RTTICharsetUTF16;
  FDataAlign := PTR_SIZE;
  FCodeAlign := PTR_SIZE;
end;

function TILTranslator.CreateILUnit(const Name: string): TILUnit;
begin
  Result := TILUnit.Create;
end;

function TILTranslator.CreateILUnitProc(ILUnit: TILUnit): TILProc;
begin
  Result := TILProc.Create;
end;

destructor TILTranslator.Destroy;
var
  i: Integer;
begin
  for i := 0 to FArgs.Count - 1 do
    FArgs.Items[i].Free;
  FInterfaces.Free;
  FRTTIProcs.Free;
  FRTTI.Free;
  FIMG.Free;
  for i := 0 to Length(FUnits) - 1 do
    FUnits[i].Free;
  inherited;
end;

function TILTranslator.GetILField(Struct: PRTTIStruct; FieldIndex: Integer): PILVariable;
var
  IMGType: ^TIMGType;
  Fields: TILVariables;
  Ancestor: PRTTIStruct;
begin
  while Struct.Ancestor <> 0 do
  begin
    Ancestor := PRTTIStruct(GetTypeInfo(Struct.Ancestor));
    if FieldIndex >= Ancestor.TotalFieldsCount then
      Break;
    Struct := Ancestor;
  end;
  IMGType := Addr(FUnits[Struct.UnitID].Types[Struct.Index]);
  Fields := IMGType.Fields;
  Result := Addr(Fields[FieldIndex - (Struct.TotalFieldsCount - Struct.FieldsCount)]);
end;

function TILTranslator.GetILMethod(Struct: PRTTIStruct; MethodIndex: Integer): TILProc;
var
  IMGType: ^TIMGType;
  Methods: TILProcedures;
  Ancestor: PRTTIStruct;
begin
  while Struct.Ancestor <> 0 do
  begin
    Ancestor := PRTTIStruct(GetTypeInfo(Struct.Ancestor));
    if MethodIndex >= Ancestor.TotalMethodsCount then
      Break;
    Struct := Ancestor;
  end;
  IMGType := Addr(FUnits[Struct.UnitID].Types[Struct.Index]);
  Methods := IMGType.Methods;

  Result := Methods[MethodIndex - (Struct.TotalMethodsCount - Struct.MethodsCount)];
end;

function TILTranslator.GetILType(UnitID, TypeID: Integer): PILType;
begin
  Result := addr(FUnits[UnitID].Types[TypeID]);
end;

function TILTranslator.GetIMGPtr(const offset: TOffset): Pointer;
begin
  if offset > 0 then
    Result := PByte(IMG.Memory) + offset
  else
    Result := nil;
end;

function TILTranslator.GetParamDataSize(RTTI: PRTTIField): Integer;
var
  TI: PRTTIType;
begin
  if RTTI.IsReference then
    Exit(PTR_SIZE);

  TI := GetTypeInfo(RTTI.DataType);

  if TI.DataSize > 8 then
    Exit(PTR_SIZE);

  if IsDataTypeReferenced(TI.DataTypeID) then
    Result := PTR_SIZE
  else
    Result := MemAlign(TI.DataSize);
end;

function TILTranslator.GetProcInfo(const Proc: TILProc): PRTTIProcedure;
begin
  Result := GetProcInfo(Proc.ProcInfo);
end;

function TILTranslator.GetProcName(Proc: TILProc): string;
begin
  Result := Proc.Name;
  if Assigned(Proc.Struct) then
    Result := Proc.Struct.Name + '.' + Result;
end;

function TILTranslator.GetProcInfo(const ProcInfoOffset: TOffset): PRTTIProcedure;
begin
//  if ProcInfoOffset > 0 then
    Result := PRTTIProcedure(PByte(FRTTIProcs.Memory) + ProcInfoOffset)
//  else
//    Result := nil;
end;

function TILTranslator.ArgGet: TILArgument;
begin
  Result := TILArgument.Create;
  FArgs.Add(Result);
end;

function TILTranslator.ArgGet(const ILVar: PILVariable): TILArgument;
begin
  Result := TILArgument.CreateFromVar(ILVar);
  FArgs.Add(Result);
end;

function TILTranslator.ArgGet(const ILType: PILType): TVMCodeArg;
begin
  Result := TVMCodeArgILType.Create(ILType);
  FArgs.Add(Result);
end;

procedure TILTranslator.AddFixOffset(Offset: TOffset);
begin

end;

procedure TILTranslator.AfterInstruction(const Ctx: TILTContext);
begin

end;

procedure TILTranslator.AfterLoadILGlobalVars(ILUnit: TILUnit);
begin

end;

function TILTranslator.ArgGet(const ILProc: TILProc): TVMCodeArg;
begin
  Result := TVMCAILProc.Create(ILProc);
  FArgs.Add(Result);
end;

procedure TILTranslator.BeforeInstruction(const Ctx: TILTContext);
begin

end;

function TILTranslator.GetArgProcRtti(const ILProc: TILProc): TVMCodeArg;
begin
  Result := TVMCAProcRtti.Create(ILProc);
  FArgs.Add(Result);
end;

function TILTranslator.GetString(const Name: TOffset): UnicodeString;
begin
  if Name > 0 then
  case FRTTICharset of
    RTTICharsetASCII: Result := UnicodeString(AnsiString(PByte(IMG.Memory) + Name));
    RTTICharsetUTF16: Result := UnicodeString(PByte(IMG.Memory) + Name);
  end else
    Result := '';
end;

function TILTranslator.GetSysTypeInfo(SysTypeIndex: Integer): PRTTIType;
begin
  if (SysTypeIndex < 0) or (SysTypeIndex >= TSystemTypes.Count) then
    raise Exception.CreateFmt('Invalid system type index: %d', [SysTypeIndex]);
  Result := PRTTIType(PByte(FRTTI.Memory) + FSystemTypes[SysTypeIndex].Offset);
end;

function TILTranslator.GetTypeInfo(const ARG: TILArgument): PRTTIType;
begin
  Result := GetTypeInfo(ARG.TypeInfo);
end;

function TILTranslator.GetTMPVar(const Context: TILTContext; DataTypeID: TDataTypeID; IsReference: Boolean): PILVariable;
begin
  Result := Context.Proc.GetTMPVar(DataTypeID, IsReference);
  Result.RTTI.DataType := FSystemTypes[Integer(dtPointer)].Offset;
end;

function TILTranslator.GetTypeInfo(const TypeInfoOffset: TOffset): PRTTIType;
begin
  if TypeInfoOffset > 0 then
    Result := PRTTIType(PByte(FRTTI.Memory) + TypeInfoOffset)
  else
    Result := nil;
end;

function TILTranslator.GetUnit(Index: Integer): TILUnit;
begin
  Result := FUnits[Index];
end;

function TILTranslator.GetUnitsCount: Integer;
begin
  Result := Length(FUnits);
end;

procedure TILTranslator.LoadILArgument(const Context: TILTContext; out Arg: TILArgument);
begin
  LoadILArgumentInternal(Context, Arg, nil);
end;

procedure TILTranslator.LoadILArgumentInternal(const Context: TILTContext; out Arg: TILArgument; PrevArg: TILArgument);
var
  DataPrefix: Byte;
  Idx: Integer;
  TypeInfo: PRTTIType;
  Variable: PILVariable;
  UnitID: Integer;
  Data: UInt64;
begin
  Arg := ArgGet();
  DataPrefix := Context.Stream.ReadUInt8;
  {если аргумент - непосредственная константа}
  if Check(DataPrefix, ARG_IMMCONST) then
  begin
    Idx := DataPrefix and 63;
    Arg.TypeInfo := FSystemTypes[Idx].Offset;
    TypeInfo := GetSysTypeInfo(Idx);
    Data := 0;
    Context.Stream.Read(Data, TypeInfo.DataSize);
    Arg.U64 := Data;
    Arg.ArgumentType := atImmConst;
    {Расширение знака}
    case TypeInfo.DataTypeID of
      dtInt8: Arg.I64 := Arg.I8;
      dtInt16: Arg.I64 := Arg.I16;
      dtInt32: Arg.I64 := Arg.I32;
    end;
    Arg.ArgClass := TILARG_CLASS.ARG_CONST;
    Arg.ArgScope := TILARG_SCOPE.ARG_SCOPE_LOCAL;
  end else begin
    Arg.ArgClass := TILARG_CLASS((DataPrefix shr 2) and 7);
    Arg.ArgScope := TILARG_SCOPE(DataPrefix and 3);
    // если аргумент обьявлен в другом модуле
    if Arg.ArgScope = ARG_SCOPE_UNIT then
      UnitID := Context.Stream.ReadStretchUInt
    else
      UnitID := Context.PUnit.Index;

    Arg.U64 := Context.Stream.ReadStretchUInt;
    case Arg.ArgClass of
      {аргумент - переменная}
      ARG_VAR: begin
        case Arg.ArgScope of
          ////////////////////////////////////////////////////////////
          ///  локальная переменная
          ARG_SCOPE_LOCAL: begin
            Variable := Addr(Context.Proc.Vars[Arg.U32]);
            Arg.TypeInfo := Variable.RTTI.DataType;
            if Variable.RTTI.IsReference then
              Arg.ArgumentType := atReference
            else
              Arg.ArgumentType := atLocal;
            Arg.PTR := Variable;
          end;
          ////////////////////////////////////////////////////////////
          ///  глобальная переменная
          ARG_SCOPE_GLOBAL: begin
            Variable := Addr(Context.PUnit.Vars[Arg.U32]);
            Arg.TypeInfo := Variable.RTTI.DataType;
            if Variable.RTTI.IsReference then
              raise Exception.Create('Invalid combination [GLOBAL VAR and REFERENCE]');
            Arg.ArgumentType := atGlobal;
            Arg.AsVariable := Variable;
          end;
          ////////////////////////////////////////////////////////////
          ///  поле структуры
          ARG_SCOPE_STRUCT: begin
            {в контексте цепочки X.Y.Z...}
            if Assigned(PrevArg) then begin
              Arg.ArgumentType := atImmConst;
            end else begin
              TypeInfo := GetTypeInfo(Context.Proc.Struct.Offset);
              Arg.AsVariable := GetILField(PRTTIStruct(TypeInfo), Arg.U32);
              Arg.TypeInfo := Arg.AsVariable.RTTI.DataType;
              Arg.ArgScope := ARG_SCOPE_STRUCT;
              Arg.ArgumentType := atField;
              //Assert(false);
            end;
          end;
          ARG_SCOPE_UNIT: begin
            Variable := Addr(FUnits[UnitID].Vars[Arg.U32]);
            Arg.TypeInfo := Variable.RTTI.DataType;
            if Variable.RTTI.IsReference then
              raise Exception.Create('Invalid combination [GLOBAL VAR and REFERENCE]');
            Arg.ArgumentType := atGlobal;
            // сохраняем в аргументе адрес описания глобальной переменной
            // позже в процедуре CorrectOffsets адрес будет заменен на смещение
            Arg.PTR := Variable;
          end;
        end;
      end;
      {аргумент - процедура}
      ARG_PROC, ARG_METHOD: begin
        // сохраняем в аргументе адрес описания процедуры
        // позже в процедуре CorrectOffsets адрес будет заменен на смещение
        case Arg.ArgScope of
          // вложенная процедура
          ARG_SCOPE_LOCAL: begin
            Assert(Arg.I32 < Length(Context.Proc.NestedProcs));
            Arg.AsProcedure := Context.Proc.NestedProcs[Arg.U32];
            Arg.ArgumentType := atLocal;
          end;
          // глобальная процедура
          ARG_SCOPE_GLOBAL: begin
            Arg.AsProcedure := Context.PUnit.Procs[Arg.U32];
            Arg.ArgumentType := atGlobal;
            Arg.TypeInfo := FSystemTypes[Ord(dtInt32)].Offset;
          end;
          // метод
          ARG_SCOPE_STRUCT: LoadILMethodArg(Context, PrevArg, ARG);
          ARG_SCOPE_UNIT: begin
            Arg.AsProcedure := FUnits[UnitID].Procs[Arg.U32];
            Arg.ArgumentType := atGlobal;
            Arg.TypeInfo := FSystemTypes[Ord(dtInt32)].Offset;
          end;
        end;
      end;
      {аргумент - тип}
      ARG_TYPE: begin
        case Arg.ArgScope of
          ARG_SCOPE_LOCAL, ARG_SCOPE_GLOBAL, ARG_SCOPE_STRUCT: Arg.TypeInfo := Context.PUnit.Types[Arg.U32 - TSystemTypes.Count].Offset;
          ARG_SCOPE_UNIT: if Arg.U32 < TSystemTypes.Count then
            Arg.TypeInfo := FSystemTypes[Arg.U32].Offset
          else
            Arg.TypeInfo := FUnits[UnitID].Types[Arg.U32 - TSystemTypes.Count].Offset;
        else
          AbortWork('not supported', []);
        end;
        Arg.AsTypeInfo := GetTypeInfo(Arg.TypeInfo);
        Arg.ArgumentType := atImmConst;
      end;
      {аргумент - строковая константа}
      ARG_SCONST: begin
        Arg.U64 := FStrLiterals[Arg.U32].Offset;
        Arg.TypeInfo := FSystemTypes[Ord(dtString)].Offset;
        Arg.ArgScope := ARG_SCOPE_GLOBAL;
        Arg.ArgumentType := atImmConst;
      end;
      {аргумент - бинарная константа}
      ARG_TCONST: begin
        Idx := Arg.U32;
        Arg.U64 := Context.PUnit.Consts[Idx].Value;
        Arg.TypeInfo := Context.PUnit.Consts[Idx].TypeInfo;
        Arg.ArgScope := ARG_SCOPE_GLOBAL;
        Arg.ArgumentType := atImmConst;
      end;
      {аргумент - константа-размер типа}
      ARG_SIZEOF: begin
        Arg.TypeInfo := FSystemTypes[Ord(dtInt32)].Offset;
        Arg.ArgumentType := atImmConst;
        if Arg.U32 >= TSystemTypes.Count then
          TypeInfo := GetTypeInfo(FUnits[UnitID].Types[Arg.U32 - TSystemTypes.Count].Offset)
        else
          TypeInfo := GetTypeInfo(FSystemTypes[Arg.U32].Offset);
        Arg.U64 := TypeInfo.DataSize;
      end;
    else
      AbortWork('Argument class %d is not supported', [Integer(Arg.ArgClass)]);
    end;
  end;
  // если цепочка,- загружаем следующий аргумент
  if (DataPrefix and ARG_NEXT) = ARG_NEXT then
    LoadILArgumentInternal(Context, Arg.Next, Arg);
end;

procedure TILTranslator.LoadILCode(Stream: TStream);

  procedure WriteAsAnsiString(Index: Integer; const s: RawByteString);
  var
    Len: Integer;
    UStr: string;
  begin
    Len := Length(s);
    {$IFDEF CPUX64}IMG.WriteInt32(0);{$ENDIF} // padding for  64 bit compilers
    IMG.WriteInt16(0);     // кодировка
    IMG.WriteInt16(1);     // размер символа
    IMG.WriteInt32(-1);    // refcount (-1 - константа)
    IMG.WriteInt32(Len);   // str length
    UStr := UnicodeString(s);
    FStrLiterals[Index].DTID := dtAnsiString;
    FStrLiterals[Index].Offset := IMG.Position;
    FStrLiteralsValues[Index] := UStr;
    if Len > 0 then
      IMG.WriteBuffer(s[Low(s)], Len*SizeOf(Byte)); // str data
    IMG.WriteUInt8(0);     // ending zero
  end;

  procedure WriteAsUnicodeString(Index: Integer; const s: string);
  var
    Len: Integer;
    UStr: string;
  begin
    Len := Length(s);
    {$IFDEF CPUX64}IMG.WriteInt32(0);{$ENDIF} // padding for  64 bit compilers
    IMG.WriteInt16(1200);  // кодировка
    IMG.WriteInt16(2);     // размер символа
    IMG.WriteInt32(-1);    // refcount (-1 - константа)
    IMG.WriteInt32(Len);   // str length
    UStr := UnicodeString(s);
    FStrLiterals[Index].DTID := dtString;
    FStrLiterals[Index].Offset := IMG.Position;
    FStrLiteralsValues[Index] := UStr;
    if Len > 0 then
      IMG.WriteBuffer(UStr[Low(UStr)], Len*SizeOf(Word)); // str data
    IMG.WriteUInt16(0);    // ending zero
  end;

  procedure ReadStrLiterals(Stream: TStream);
  var
    DTID: TDataTypeID;
    i, c: Integer;
    RawStr: RawByteString;
  begin
    c := Stream.ReadStretchUInt;
    SetLength(FStrLiterals, c);
    SetLength(FStrLiteralsValues, c);
    for i := 0 to c - 1 do begin
      DTID := TDataTypeID(Stream.ReadUInt8);
      RawStr := Stream.ReadRawByteString;
      case DTID of
        dtAnsiString: WriteAsAnsiString(i, RawStr);
        dtString: WriteAsUnicodeString(i, UTF8ToString(RawStr));
      else
        raise Exception.Create('Invalid literal type');
      end;
    end;
  end;
var
  i, c: Integer;
  SysTypes: PSystemTypes;
  ILUnit: TILUnit;
begin
  IMG.Size := 1024*1024*4;    // начальное значение образа 4MB
  FRTTI.Size := 1024*1024*1;  // 1MB
  FRTTIProcs.Size := 1024*1024*1;  // 1MB

  {заголовок}
  FIMGHeader := IMG.MemoryPosition;
  FIMGHeader.Signature := VM_IMG_SIGNATURE_V1;
  IMG.IncPosition(SizeOf(TIMGHeader));
  IncludeDebugInfo := Stream.ReadBoolean;

  {табличные строковые литералы}
  FIMGHeader.ConstsOffset := IMG.Position;
  ReadStrLiterals(Stream);
  FConstsSize := IMG.Position - FIMGHeader.ConstsOffset;

  {системный RTTI}
  FRTTI.WriteUInt32(VM_IMG_RTTI_SIGNATURE);
  SysTypes := FRTTI.MemoryPosition;
  // инициализируем встроенные типы
  InitSystemTypes(SysTypes^);
  FRTTI.IncPosition(SizeOf(TSystemTypes));
  SetLength(FSystemTypes, TSystemTypes.Count);
  // инициализируем встроенные типы (задаем смещения)
  SetSystemTypesOffsets(SysTypes, FSystemTypes);

  {модули}
  c := Stream.ReadStretchUInt;
  if c = 0 then
    raise Exception.Create('The program has no units');
  SetLength(FUnits, c);

  {пишем дин массив RTTI модулей}
  WriteUnitsRTTIArray(c);

  VMUnits := IMG.MemoryPosition;
  IMG.IncPosition(SizeOf(TRTTIUnit)*c);

  {декларации модулей}
  for i := 0 to c - 1 do
    LoadILUnitDecl(i, Stream);

  {связвание RTTI классов}
  MapRTTIClassTypes;

  {таблица импорта}
  if FImportTableCount > 0 then
    MakeImportTable;
  FIMGHeader.ImportTableCount := FImportTableCount;

  {тела модулей}
  for i := 0 to c - 1 do begin
    ILUnit := FUnits[i];
    LoadILUnitBody(ILUnit, Stream);
  end;

  {коррекция смещений в образе}
  CorrectOffsets;
end;

procedure TILTranslator.LoadILCode(const Path: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Path, fmOpenRead);
  try
    LoadILCode(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TILTranslator.LoadILGlobalVars(ILUnit: TILUnit; Stream: TStream);
var
  i, c, Idx: Integer;
  pVar: PILVariable;
  Prefix: Integer;
begin
  c := Stream.ReadStretchUInt;
  SetLength(ILUnit.Vars, c);
  for i := 0 to c - 1 do
  begin
    Prefix := Stream.ReadUInt8;
    pVar := addr(ILUnit.Vars[i]);
    pVar.RTTI.Offset := 0;
    pVar.VarScope := ARG_SCOPE_GLOBAL;
    pVar.RTTI.FClassID := rttiVar;
    pVar.RTTI.TypeInfo := nil;
    pVar.RTTI.RefCount := -1;
    pVar.RTTI.WeekInfo := nil;
    pVar.RTTI.SyncInfo := nil;
    pVar.RTTI.DataType := ReadVarType(ILUnit, Stream, Prefix);
    if Check(Prefix, ILVAR_HASINDEX) then begin
      Idx := Stream.ReadStretchUInt;
      pVar.AbsoluteTo := addr(ILUnit.Vars[Idx]);
      pVar.AbsoluteOffset := Stream.ReadStretchUInt;
    end;
    pVar.RTTI.IsReference := Check(Prefix, ILVAR_REFERENCE);
    pVar.RTTI.IsConstant := Check(Prefix, ILVAR_CONSTPARAM);
    if IncludeDebugInfo then
    begin
      Idx := Stream.ReadStretchUInt;
      pVar.RTTI.Name := FStrLiterals[Idx].Offset;
      pVar.Name := FStrLiteralsValues[Idx];
    end else
      pVar.Name := '$' + IntToStr(i);
    pVar.DefaultValue := nil;
    if Check(Prefix, ILVAR_HASVALUE) then
      pVar.DefaultValue := ReadGlobalVarValue(Stream, pVar);
  end;
  AfterLoadILGlobalVars(ILUnit);
end;

procedure TILTranslator.LoadILLocalVars(Proc: TILProc; Stream: TStream; var MemSize: Integer; StartVarIndex, VarCount: Integer);
var
  i, Idx: Integer;
  LocalVar: PILVariable;
  TypeInfo: PRTTIType;
  Variables: TILVariables;
begin
  Variables := Proc.Vars;
  for i := StartVarIndex to VarCount - 1 do
  try
    LocalVar := addr(Variables[i]);
    LocalVar.Flags := Stream.ReadUInt8;
    LocalVar.VarScope := ARG_SCOPE_LOCAL;
    LocalVar.RTTI.DataType := ReadVarType(Proc.ILUnit, Stream, LocalVar.Flags);
    LocalVar.DefaultValue := nil;
    LocalVar.RTTI.IsReference := Check(LocalVar.Flags, ILVAR_REFERENCE);
    LocalVar.RTTI.IsConstant := Check(LocalVar.Flags, ILVAR_CONSTPARAM);
    LocalVar.IsResult := False;
    LocalVar.IsParam := False;
    if Check(LocalVar.Flags, ILVAR_HASINDEX) then
    begin
      Idx := Stream.ReadStretchUInt;
      LocalVar.AbsoluteOffset := Stream.ReadStretchUInt;
      LocalVar.RTTI.Offset := Variables[Idx].RTTI.Offset;
    end else begin
      LocalVar.RTTI.Offset := MemSize;
      TypeInfo := GetTypeInfo(LocalVar.RTTI.DataType);
      if (TypeInfo.DataTypeID = dtClass) or LocalVar.RTTI.IsReference then
        Inc(MemSize, MemAlign(PTR_SIZE))
      else
        Inc(MemSize, MemAlign(TypeInfo.DataSize));
    end;
    if IncludeDebugInfo then
    begin
      Idx := Stream.ReadStretchUInt;
      LocalVar.RTTI.Name := FStrLiterals[Idx].Offset;
      LocalVar.Name := FStrLiteralsValues[IDx];
    end else
      LocalVar.Name := '$' + IntToStr(i);
  except
    on e: exception do
      raise Exception.CreateFmt('Read local variable ERROR[unit: %s proc: %s varidx: %d]: %s',
                                [Proc.ILUnit.Name, Proc.Name, i, e.Message]);
  end;
end;

procedure TILTranslator.LoadILMethodArg(const Context: TILTContext; SelfArg: TILArgument; var Arg: TILArgument);
var
  StructInfo: PRTTIType;
  Method: TILProc;
  M: TILMethod;
begin
  if not Assigned(SelfArg) then
  begin
    StructInfo := GetTypeInfo(Context.Proc.Struct.Offset);
    Assert(StructInfo.DataTypeID in [dtRecord, dtClass, dtInterface]);
    SelfArg := Context.Proc.SelfArg;
  end else
  case SelfArg.ArgClass of
    ARG_VAR: StructInfo := GetTypeInfo(SelfArg.AsVariable.RTTI.DataType);
    ARG_TYPE: StructInfo := SelfArg.AsTypeInfo;
  else
    AbortWork('Self argument class is not supported');
    StructInfo := nil;
  end;
  Method := GetILMethod(PRTTIStruct(StructInfo), Arg.U32);

  M.Self := SelfArg;
  M.Proc := Method;
  Arg.AsMethod := M;
end;

procedure TILTranslator.LoadILMethodsBodies(ILUnit: TILUnit; Stream: TStream);
var
  Index, TI, MI, TypesCount, MethodsCount: Integer;
  Methods: TILProcedures;
  ILType: PILType;
  Proc: TILProc;
begin
  TypesCount := Stream.ReadStretchUInt;
  for TI := 0 to TypesCount - 1 do
  begin
    // читаем индекс типа
    Index := Stream.ReadStretchUInt;
    Index := Index - TSystemTypes.Count;
    if (Index < 0) or (Index >= Length(ILUnit.Types)) then
      AbortWork('Invalid type index: %d', [Index]);
    // выделяем необходимое кол-во структур для методов
    MethodsCount := Stream.ReadStretchUInt;
    ILType := addr(ILUnit.Types[Index]);
    Methods := ILType.Methods;
    for MI := 0 to MethodsCount - 1 do
    begin
      Proc := Methods[MI];
      {читаем тело метода}
      LoadILProcBody(Stream, Proc);
    end;
  end;
end;

procedure TILTranslator.LoadILParameters(Stream: TStream; Proc: TILProc);
var
  c: Integer;
begin
  c := Stream.ReadStretchUInt;
  SetLength(Proc.Params, c);
  if c > 0 then
    LoadILParameter(Proc.ILUnit, Stream, c, PRTTIParams(addr(Proc.Params[0])));
end;

procedure TILTranslator.LoadILParameter(ILUnit: TILUnit; Stream: TStream; ParamsCount: Integer; Params: PRTTIParams);
var
  i, Idx: Integer;
  Param: PRTTIParameter;
  Prefix: Integer;
  //PRTTI: PRTTIType;
begin
  for i := 0 to ParamsCount - 1 do
  begin
    Param := addr(Params[i]);
    Prefix := Stream.ReadUInt8;
    Param.DataType := ReadVarType(ILUnit, Stream, Prefix);
    Param.IsReference := Check(Prefix, ILVAR_REFERENCE);
    Param.IsConstant := Check(Prefix, ILVAR_CONSTPARAM);
    //PRTTI := GetTypeInfo(Param.DataType);
    if IncludeDebugInfo then begin
      Idx := Stream.ReadStretchUInt;
      Param.Name := FStrLiterals[Idx].Offset;
    end else
      Param.Name := 0;
  end;
end;

procedure TILTranslator.LoadILProcBodies(Stream: TStream; ILUnit: TILUnit; ParentProc: TILProc; const Procedures: TILProcedures);
var
  i: Integer;
  Proc: TILProc;
begin
  // Загрузка тел процедур
  for i := 0 to Length(Procedures) - 1 do begin
    Proc := Procedures[i];
    if not Proc.IsImported then
      LoadILProcBody(Stream, Proc);
  end;
end;

procedure TILTranslator.LoadILProcBody(Stream: TStream; Proc: TILProc);
var
  i, ParamIndex, TotalVarsCnt,  ParamsCnt, LocalVarsCnt, ExtraVarsCnt: Integer;
  LocalVar: PILVariable;
  Param: PRTTIParameter;
  SelfTypeInfo: TOffset;
  MemOffset: Integer;
  StructInfo: PRTTIStruct;
begin
  ParamsCnt := Length(Proc.Params);
  // читаем кол-во локальных переменных
  LocalVarsCnt := Stream.ReadStretchUInt;

  if Assigned(Proc.Struct) then
  begin
    SelfTypeInfo := Proc.Struct.Offset;
    StructInfo := PRTTIStruct(GetTypeInfo(SelfTypeInfo));
  end else begin
    StructInfo := nil;
    SelfTypeInfo := 0;
  end;

  if (SelfTypeInfo <> 0) and (ILPROC_HASSELFPTR and Proc.Flags <> 0) then
  begin
    ExtraVarsCnt := 1;
  end else begin
    ExtraVarsCnt := 0;
  end;

  MemOffset := Proc.StackSize;
  // так как в IL коде параметры и локальные переменные суть одно и тоже (единое пространство индексов),
  // то переносим спиок параметров в список переменных, проставляя смещение в стеке
  TotalVarsCnt := ParamsCnt + LocalVarsCnt + ExtraVarsCnt;
  SetLength(Proc.Vars, TotalVarsCnt);
  if TotalVarsCnt > 0 then
    FillChar(Proc.Vars[0], TotalVarsCnt, 0);

  // перенос, параметров в локальные переменные
  ParamIndex := 0;
  for i := 0 to ParamsCnt + ExtraVarsCnt - 1 do
  begin
    LocalVar := addr(Proc.Vars[i]);
    if (((ILPROC_HASRESULT and Proc.Flags) <> 0) and
       ((ILPROC_HASSELFPTR and Proc.Flags) <> 0) and (i = 1)) or
       (((ILPROC_HASRESULT and Proc.Flags) = 0) and
       ((ILPROC_HASSELFPTR and Proc.Flags) <> 0) and (i = 0)) then
    begin
      LocalVar.RTTI.DataType := SelfTypeInfo;
      LocalVar.RTTI.Offset := MemOffset;
      Inc(MemOffset, MemAlign(PTR_SIZE));
      LocalVar.RTTI.IsReference := (StructInfo.DataTypeID = dtRecord);
      LocalVar.RTTI.Name := 0;
      LocalVar.VarClass := VarConst; // константный параметр
      LocalVar.Name := 'Self';
      LocalVar.AbsoluteTo := nil;
      LocalVar.IsResult := False;
    end else begin
      Param := Addr(Proc.Params[ParamIndex]);
      LocalVar.RTTI.DataType := Param.DataType;
      LocalVar.RTTI.Offset := MemOffset;
      LocalVar.IsResult := (Proc.ProcType = ptFunction) and (i = 0);
      Inc(MemOffset, MemAlign(GetParamDataSize(Param)));
      LocalVar.RTTI.IsReference := Param.IsReference;
      LocalVar.RTTI.Name := Param.Name;
      if Param.IsConstant then
        LocalVar.VarClass := VarConst
      else
        LocalVar.VarClass := VarParam;
      LocalVar.Name := GetString(Param.Name);
      LocalVar.AbsoluteTo := nil;
      Inc(ParamIndex);
    end;
    LocalVar.IsParam := True;
  end;

  {если это метод, добавляем аргумент - self}
  if (Proc.Flags and ILPROC_HASSELFPTR) <> 0 then
  begin
    Proc.SelfArg := ArgGet();
    Proc.SelfArg.ArgumentType := atLocal;
    Proc.SelfArg.ArgClass := ARG_VAR;
    Proc.SelfArg.ArgScope := ARG_SCOPE_LOCAL;
    Proc.SelfArg.TypeInfo := SelfTypeInfo;
    if (Proc.Flags and ILPROC_HASRESULT) <> 0 then
      Proc.SelfArg.AsVariable := addr(Proc.Vars[1])
    else
      Proc.SelfArg.AsVariable := addr(Proc.Vars[0]);
  end;

  // загружаем RTTI локальных переменных
  LoadILLocalVars(Proc, Stream, MemOffset, ParamsCnt + ExtraVarsCnt, TotalVarsCnt);

{  if FIncludeDebugInfo then
    WriteLocalVarDebugInfo(Proc);}

  Proc.StackSize := MemOffset;

  // загружаем локальные процедуры
  if (Proc.Flags and ILPROC_HASLOCALPROCS) <> 0 then
  begin
    LoadILProcDecls(Stream, Proc.ILUnit, Proc, Proc.NestedProcs);
    LoadILProcBodies(Stream, Proc.ILUnit, Proc, Proc.NestedProcs);
  end;

  // задаем смещение в памяти начала кода процедуры
  Proc.Offset := MemAlign(IMG.Position, FCodeAlign);
  IMG.Position := Proc.Offset;

  // читаем и транслируем IL-код процедуры
  ReadILCode(Stream, Proc);

  // корректируем размер стека + размер для временных переменных
  Proc.StackSize := Proc.GetStackSize;
end;

procedure TILTranslator.WriteProcLocalVarDebugInfo(Proc: TILProc);
var
  i, c: Integer;
  PVar: PILVariable;
  LVar: PRTTILocalVar;
  ProcInfo: PRTTIProcedure;
  LVars: PRTTILocalVarsSArray;
begin
  c := Length(Proc.Vars);
  FRTTI.WriteNativeInt(-1); // refcnt
  FRTTI.WriteNativeInt(c);  // length
  ProcInfo := GetProcInfo(Proc);
  if c = 0 then
  begin
    ProcInfo.LocalVars := 0;
    Exit;
  end;
  ProcInfo.LocalVars := FRTTI.Position;
  LVars := FRTTI.MemoryPosition;
  FRTTI.IncPosition(SizeOf(TRTTILocalVar)*C);
  for i := 0 to c - 1 do
  begin
    PVar := @(Proc.Vars[i]);
    LVar := @(LVars[i]);
    LVar.Name := PVar.RTTI.Name;
    LVar.DataType := PVar.RTTI.DataType;
    LVar.Offset := PVar.RTTI.Offset;
    LVar.IsReference := PVar.RTTI.IsReference;
    LVar.IsParam := PVar.IsParam;
  end;
end;

procedure TILTranslator.LoadILProcDecls(Stream: TStream; ILUnit: TILUnit; ParentProc: TILProc; out Procedures: TILProcedures);
var
  i, c: Integer;
  Proc: TILProc;
  NameIndex: Integer;
  TypeInfo: PRTTIType;
  ProcInfo: PRTTIProcedure;
begin
  // Загрузка деклараций
  c := Stream.ReadStretchUInt;
  SetLength(Procedures, c);
  for i := 0 to c - 1 do
  begin
    Proc := CreateILUnitProc(ILUnit);
    Procedures[i] := Proc;

    {выделяем память в RTTI секции}
    Proc.ProcInfo := FRTTIProcs.Position;
    ProcInfo := FRTTIProcs.MemoryPosition;
    FRTTIProcs.IncPosition(SizeOf(TRTTIProcedure));
    FillChar(ProcInfo^, Sizeof(TRTTIProcedure), 0);
    ProcInfo.RefCount := -1;
    ProcInfo.FClassID := rttiProc;

    Proc.ILUnit := ILUnit;
    Proc.StackSize := 0;
    Proc.Offset := 0;
    Proc.Flags := Stream.ReadUInt8;
    ProcInfo.Flags := Proc.Flags;
    Proc.CallConvention := TCallConvention(Stream.ReadUInt8);
    ProcInfo.CallConvention := Proc.CallConvention;
    Proc.ProcType := TProcType(Proc.Flags and ILPROC_HASRESULT);

    {экспортируемая процедура}
    if Check(Proc.Flags, ILPROC_EXPORT) then
    begin
      Proc.ExportIndex := ILUnit.ExportProcsCount;
      Inc(ILUnit.ExportProcsCount);
    end else begin
      Proc.ExportIndex := -1;
    end;

    {чтение имени процедуры}
    if Check(Proc.Flags, ILPROC_EXPORT) or IncludeDebugInfo then
    begin
      NameIndex := Stream.ReadStretchUInt;
      ProcInfo.Name := FStrLiterals[NameIndex].Offset;
      Proc.Name := GetString(ProcInfo.Name);
    end;

    {импортируемая процедура}
    if Check(Proc.Flags, ILPROC_IMPORT) then begin
      NameIndex := Stream.ReadStretchUInt;
      Proc.ImportLib := FStrLiterals[NameIndex].Offset;
      ProcInfo.ImportLib := Proc.ImportLib;
      NameIndex := Stream.ReadStretchUInt;
      Proc.ImportName := FStrLiterals[NameIndex].Offset;
      ProcInfo.ImportName := Proc.ImportName;
      Inc(FImportTableCount);
    end;

    if Check(Proc.Flags, ILPROC_VIRTUAL) then
    begin
      Proc.VirtualIndex := Stream.ReadStretchUInt;
      ProcInfo.VirtualIndex := Proc.VirtualIndex;
    end;

    {читаем парметры}
    LoadILParameters(Stream, Proc);

    if Check(Proc.Flags, ILPROC_HASRESULT) then
    begin
      ProcInfo.ResultType := Proc.Params[0].DataType;
      TypeInfo := GetTypeInfo(ProcInfo.ResultType);
      if TypeInfo.DataTypeID in [dtRecord, dtGuid, dtStaticArray] then
        Proc.Params[0].IsReference := True;
    end;

    {поиск импортируемой процедуры}
    if (Proc.Flags and ILPROC_IMPORT) <> 0 then
      MapImportProc(Proc);
  end;
end;

procedure TILTranslator.LoadILSimpleConst(Stream: TStream; DataType: PRTTIType);
begin

end;

procedure TILTranslator.LoadILRecordConst(ILUnit: TILUnit; Stream: TStream; CItem: PConstInfoRec);
var
  i: Integer;
  Fld: PRTTIField;
  Fields: TRTTIFields;
  FldDataType: PRTTIType;
  StructInfo: PRTTIStruct;
begin
  StructInfo := PRTTIStruct(GetTypeInfo(CItem.TypeInfo));
  CItem.Value := IMG.Position;
  CItem.Size := StructInfo.DataSize;
  Fields := TRTTIFields(GetTypeInfo(StructInfo.Fields));
  for i := 0 to Length(Fields) - 1  do
  begin
    Fld := @(Fields[i]);
    FldDataType := GetTypeInfo(Fld.DataType);
    if FldDataType.DataTypeID in [dtString, dtAnsiString] then
      AddFixOffset(IMG.Position);
    LoadILSimpleConst(Stream, FldDataType);
  end;
end;

procedure TILTranslator.LoadILTypes(ILUnit: TILUnit; Stream: TStream);
var
  i, c, RTTISize: Integer;
  dt: UInt8;
  TypeInfo: PRTTIType;
  Idx: Integer;
  Context: TReadTypeContext;
  IMGType: ^TIMGType;
begin
  c := Stream.ReadStretchUInt;
  SetLength(ILUnit.Types, c);
  Context.Stream := Stream;
  Context.ILUnit := ILUnit;

  // пердварительная аллокация
  for i := 0 to c - 1 do begin
    dt := Stream.ReadUInt8;
    IMGType := addr(ILUnit.Types[i]);
    IMGType.ID := TDataTypeID(dt and 127);
    IMGType.Offset := FRTTI.Position;
    case IMGType.ID of
      dtEnum: RTTISize := SizeOf(TRTTIOrdinal);
      dtRange: RTTISize := SizeOf(TRTTIOrdinal);
      dtSet: RTTISize := SizeOf(TRTTIOrdinal);
      dtStaticArray: RTTISize := SizeOf(TRTTIArray);
      dtDynArray, dtOpenArray: RTTISize := SizeOf(TRTTIDynArray);
      dtPointer: RTTISize := SizeOf(TRTTIPointer);
      dtWeakRef: RTTISize := SizeOf(TRTTIPointer);
      dtRecord: RTTISize := SizeOf(TRTTIRecord);
      dtClass: RTTISize := SizeOf(TRTTIClass);
      dtClassOf: RTTISize := SizeOf(TRTTIPointer);
      dtProcType: RTTISize := SizeOf(TRTTIProcType);
      dtInterface: begin
        RTTISize := SizeOf(TRTTIInterface);
        Inc(FInterfaceCnt);
      end;
    else
      RTTISize := 0;
      Assert(False);
    end;
    FRTTI.IncPosition(RTTISize);
    TypeInfo := GetTypeInfo(IMGType.Offset);
    FillChar(TypeInfo^, RTTISize, #0);
    TypeInfo.RefCount := -1;
    TypeInfo.DataTypeID := IMGType.ID;
    if IMGType.ID = dtInterface then
    begin
      PRTTIInterface(TypeInfo).InterfaceID := FInterfaceID;
      FInterfaces.Add(PRTTIInterface(TypeInfo));
      Inc(FInterfaceID);
    end;
  end;

  // второй проход
  for i := 0 to c - 1 do begin
    dt := Stream.ReadUInt8; // читаем DataTypeID
    if IncludeDebugInfo then begin
      Idx := Stream.ReadStretchUInt;
      Context.TypeName := FStrLiterals[Idx].Offset;
    end else
      Context.TypeName := 0;
    Context.ILType := Addr(ILUnit.Types[i]);
    Assert(TDataTypeID(dt) = Context.ILType.ID);
    TypeInfo := GetTypeInfo(Context.ILType.Offset);
    TypeInfo.Name := Context.TypeName;
    TypeInfo.UnitID := ILUnit.Index;
    TypeInfo.Index := i;
    Context.ILType.Name := GetString(TypeInfo.Name);
    case Context.ILType.ID of
      dtEnum: ReadOrdinalTypeInfo(Context);
      dtRange: ReadOrdinalTypeInfo(Context);
      dtSet: ReadSetTypeInfo(Context);
      dtStaticArray: ReadArrayTypeInfo(Context);
      dtDynArray, dtOpenArray: ReadDynArrayTypeInfo(Context);
      dtPointer: ReadPointerType(Context);
      dtWeakRef: ReadWeakRefType(Context);
      dtRecord: ReadRecordTypeInfo(Context);
      dtClass: ReadClassTypeInfo(Context);
      dtClassOf: ReadClassOfType(Context);
      dtProcType: ReadProcTypeInfo(Context);
      dtInterface: ReadIntfTypeInfo(Context);
    else
      AbortWork('Unknown type info: %s', [GetDataTypeName(Context.ILType.ID)]);
    end;
  end;
  WriteTypesRTTIArray(ILUnit);
end;

procedure TILTranslator.LoadILUnitBody(ILUnit: TILUnit; Stream: TStream);
var
  i, c, SrcTextLine: Integer;
begin
  {глобальные процедуры}
  LoadILProcBodies(Stream, ILUnit, nil, ILUnit.Procs);
  {методы}
  LoadILMethodsBodies(ILUnit, Stream);
  WriteILUnitProlog(Stream, ILUnit);
  WriteILUnitEpilog(Stream, ILUnit);
  if IncludeDebugInfo then
  begin
    c := Stream.ReadStretchUInt;
    SetLength(ILUnit.BreakPoints, c);
    for i := 0 to c - 1 do begin
      SrcTextLine := Stream.ReadStretchUInt;
      ILUnit.BreakPoints[i].SrcTextLine := SrcTextLine;
      ILUnit.BreakPoints[i].VMTextLine := 0;  // установка позже
      ILUnit.BreakPoints[i].Offset := 0; // установка позже
    end;
  end;
  WriteVMExportProcs(ILUnit);
  WriteProcsRTTI(ILUnit);
end;

procedure TILTranslator.LoadILUnitDecl(Index: Integer; Stream: TStream);
var
  NameIndex: UInt32;
  ProcInfo: PRTTIProcedure;
  UFlags: UInt32;
  UName: string;
  ILUnit: TILUnit;
begin
  UFlags := Stream.ReadStretchUInt;

  {RTTI имя}
  NameIndex := Stream.ReadStretchUInt;
  UName := FStrLiteralsValues[NameIndex];

  ILUnit := CreateILUnit(UName);
  ILUnit.Name := UName;
  ILUnit.Flags := UFlags;
  ILUnit.Index := Index;
  FUnits[Index] := ILUnit;

  ILUnit.VMUnit := addr(VMUnits[Index]);
  ILUnit.VMUnit.FClassID := rttiUnit;
  ILUnit.VMUnit.RefCount := -1;
  ILUnit.VMUnit.Name := FStrLiterals[NameIndex].Offset;
  ILUnit.VMUnit.Procs := FRTTIProcs.Position;

  {типы}
  LoadILTypes(ILUnit, Stream);

  {константы}
  LoadILConsts(ILUnit, Stream);

  {глобальные переменные}
  LoadILGlobalVars(ILUnit, Stream);

  {глобальные процедуры}
  LoadILProcDecls(Stream, ILUnit, nil, ILUnit.Procs);

  ILUnit.InitProc := CreateILUnitProc(ILUnit);
  ILUnit.InitProc.ProcInfo := FRTTIProcs.Position;
  ProcInfo := FRTTIProcs.MemoryPosition;
  FRTTIProcs.IncPosition(SizeOf(TRTTIProcedure));
  FillChar(ProcInfo^, Sizeof(TRTTIProcedure), 0);
  ProcInfo.RefCount := -1;
  ProcInfo.FClassID := rttiProc;

  ILUnit.FinalProc := CreateILUnitProc(ILUnit);
  ILUnit.FinalProc.ProcInfo := FRTTIProcs.Position;
  ProcInfo := FRTTIProcs.MemoryPosition;
  FRTTIProcs.IncPosition(SizeOf(TRTTIProcedure));
  FillChar(ProcInfo^, Sizeof(TRTTIProcedure), 0);
  ProcInfo.RefCount := -1;
  ProcInfo.FClassID := rttiProc;
end;

procedure TILTranslator.MapImportType(aType: PRTTIType);
var
  LibName, TypeName: UnicodeString;
  RegType: TTypeRegInfo;
begin
  LibName := GetString(aType.ImportLib);
  TypeName := GetString(aType.ImportName);
  RegType := FindType(LibName, TypeName);
  if not Assigned(RegType) then
    raise Exception.CreateFmt('Import type "%s" in library "%s" is not found', [TypeName, LibName]);
end;

procedure TILTranslator.MapRTTIClassTypes;
var
  i, j, pi: Integer;
  UN: TILUnit;
  TD: PILType;
  TI: PRTTIType;
  ProcInfo: PRTTIProcedure;
  TypeName: string;
begin
  for i := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[i];
    if LowerCase(UN.Name) = cRttiUnitName then
    begin
      for j := 0 to Length(UN.Types) - 1 do
      begin
        TD := addr(UN.Types[j]);
        TypeName := LowerCase(TD.Name);
        if TypeName = 'trtti' then
          FRTTIClassTypes._trtti := TD.Offset
        else
        if TypeName = 'trttiunit' then
          FRTTIClassTypes._trttiunit := TD.Offset
        else
        if TypeName = 'trttiordinal' then
          FRTTIClassTypes._trttiordinal := TD.Offset
        else
        if TypeName = 'trttiarray' then
          FRTTIClassTypes._trttiarray := TD.Offset
        else
        if TypeName = 'trttiset' then
          FRTTIClassTypes._trttiset := TD.Offset
        else
        if TypeName = 'trttidynarray' then
          FRTTIClassTypes._trttidynarray := TD.Offset
        else
        if TypeName = 'trttifloat' then
          FRTTIClassTypes._trttifloat := TD.Offset
        else
        if TypeName = 'trttipointer' then
          FRTTIClassTypes._trttipointer := TD.Offset
        else
        if TypeName = 'trttivariant' then
          FRTTIClassTypes._trttivariant := TD.Offset
        else
        if TypeName = 'trttirecord' then
          FRTTIClassTypes._trttirecord := TD.Offset
        else
        if TypeName = 'trtticlass' then
          FRTTIClassTypes._trtticlass := TD.Offset
        else
        if TypeName = 'trttiinterface' then
          FRTTIClassTypes._trttiinterface := TD.Offset
        else
        if TypeName = 'trttiproctype' then
          FRTTIClassTypes._trttiproctype := TD.Offset
        else
        if TypeName = 'trttiprocedure' then
          FRTTIClassTypes._trttiprocedure := TD.Offset;
      end;
      break;
    end;
  end;

  // RTTI классы не найдены
  if FRTTIClassTypes._trtti = 0 then
    Exit;

  {проставляем classinfo для системных типов}
  for i := 0 to Length(FSystemTypes) - 1 do
  begin
    TD := addr(FSystemTypes[i]);
    TI := GetTypeInfo(TD.Offset);
    case TI.DataTypeID of
      dtInt8, dtInt16, dtInt32, dtInt64,
      dtUInt8, dtUInt16, dtUInt32, dtUInt64,
      dtNativeInt, dtNativeUInt,
      dtBoolean, dtChar, dtAnsiChar: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiordinal);
      dtString, dtAnsiString: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiarray);
      dtFloat32, dtFloat64: TI.TypeInfo := Pointer(FRTTIClassTypes._trttifloat);
      dtPointer: TI.TypeInfo := Pointer(FRTTIClassTypes._trttipointer);
      dtVariant: TI.TypeInfo := Pointer(FRTTIClassTypes._trttivariant);
      dtGuid: TI.TypeInfo := Pointer(FRTTIClassTypes._trtti);
    else
      AbortWork('Invalid RTTI for system type: ' + GetDataTypeName(TI.DataTypeID));
    end;
  end;

  {проставляем classinfo для пользовательских типов}
  for i := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[i];
    UN.VMUnit.TypeInfo := Pointer(FRTTIClassTypes._trttiunit);
    for j := 0 to Length(UN.Types) - 1 do
    begin
      TD := addr(UN.Types[j]);
      TI := GetTypeInfo(TD.Offset);
      case TI.DataTypeID of
        dtInt8, dtInt16, dtInt32, dtInt64,
        dtUInt8, dtUInt16, dtUInt32, dtUInt64,
        dtNativeInt, dtNativeUInt,
        dtBoolean, dtEnum, dtRange, dtChar, dtAnsiChar: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiordinal);
        dtSet: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiset);
        dtStaticArray, dtString, dtAnsiString: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiarray);
        dtDynArray, dtOpenArray: TI.TypeInfo := Pointer(FRTTIClassTypes._trttidynarray);
        dtFloat32, dtFloat64: TI.TypeInfo := Pointer(FRTTIClassTypes._trttifloat);
        dtPointer: TI.TypeInfo := Pointer(FRTTIClassTypes._trttipointer);
        dtVariant: TI.TypeInfo := Pointer(FRTTIClassTypes._trttivariant);
        dtGuid: TI.TypeInfo := Pointer(FRTTIClassTypes._trtti);
        dtRecord: TI.TypeInfo := Pointer(FRTTIClassTypes._trttirecord);
        dtClass: TI.TypeInfo := Pointer(FRTTIClassTypes._trtticlass);
        dtInterface: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiinterface);
        dtProcType: TI.TypeInfo := Pointer(FRTTIClassTypes._trttiproctype);
      else
        AbortWork('Invalid RTTI for type: ' + GetDataTypeName(TI.DataTypeID));
      end;
      // methods
      for pi := 0 to Length(TD.Methods) - 1 do
      begin
        ProcInfo := GetProcInfo(TD.Methods[pi].ProcInfo);
        ProcInfo.TypeInfo := Pointer(FRTTIClassTypes._trttiprocedure);
      end;
    end;
    // global procs
    for pi := 0 to Length(UN.Procs) - 1 do
    begin
      ProcInfo := GetProcInfo(UN.Procs[pi].ProcInfo);
      ProcInfo.TypeInfo := Pointer(FRTTIClassTypes._trttiprocedure);
    end;
    // init
    ProcInfo := GetProcInfo(UN.InitProc.ProcInfo);
    ProcInfo.TypeInfo := Pointer(FRTTIClassTypes._trttiprocedure);
    // final
    ProcInfo := GetProcInfo(UN.FinalProc.ProcInfo);
    ProcInfo.TypeInfo := Pointer(FRTTIClassTypes._trttiprocedure);
  end;
end;

procedure TILTranslator.ReadArrayTypeInfo(var Context: TReadTypeContext);
var
  i, c: Integer;
  TotalElCnt, ElCnt: Int32;
  DimTypeInfo: PRTTIOrdinal;
  TI: TOffset;
  Dimenstions: PRTTIDimensions;
  Result: PRTTIArray;
begin
  with Context do begin
    c := Stream.ReadStretchUInt;
    Result := PRTTIArray(GetTypeInfo(Context.ILType.Offset));
    Result.Dimensions := FRTTI.Position;
    FRTTI.IncPosition(c*SizeOf(TRTTIOrdinal));
    Dimenstions := PRTTIDimensions(GetTypeInfo(Result.Dimensions));
    FillChar(Dimenstions^, c*SizeOf(TRTTIOrdinal), #0);

    Result.DimensionsCount := c;
    Result.DataTypeID := dtStaticArray;
    TotalElCnt := 1;
    for i := 0 to c - 1 do begin
      TI := ReadTypeSpecInfo(Stream);
      Dimenstions[i] := TI;
      DimTypeInfo := PRTTIOrdinal(GetTypeInfo(TI));
      ElCnt := DimTypeInfo.HiBound - DimTypeInfo.LoBound + 1;
      TotalElCnt := TotalElCnt * ElCnt;
    end;
    Result.ElementTypeInfoOffset := ReadTypeSpecInfo(Stream);
    Result.DataSize := TotalElCnt * GetTypeInfo(Result.ElementTypeInfoOffset).DataSize;
  end;
end;

procedure TILTranslator.ReadClassIntfaces(var Context: TReadTypeContext; var ClassType: PRTTIClass);
var
  i, j, UnitID, TypeID, IntfCnt, MCnt, MIdx: Integer;
  IMTables: PIMTS;
  IntfType: PILType;
  IntfRTTI: PRTTIInterface;
  Method: TILProc;
  IMT: PIMT;
begin
  with Context do begin
    // кол-во интерфейсов которые имплементирует класс
    IntfCnt := Stream.ReadStretchUInt;
    ClassType.IMTS := IMG.Position;
    IMTables := IMG.MemoryPosition;
    IMG.IncPosition(FInterfaceCnt*PTR_SIZE);
    FillChar(IMTables^, FInterfaceCnt*PTR_SIZE, #0);
    // читаем список интерфейсов
    for i := 0 to IntfCnt - 1 do
    begin
      UnitID := Stream.ReadStretchUInt;
      TypeID := Stream.ReadStretchUInt;
      MCnt := Stream.ReadStretchUInt;
      IntfType := GetILType(UnitID, TypeID - TSystemTypes.Count);
      Assert(Assigned(IntfType));
      IntfRTTI := PRTTIInterface(GetTypeInfo(IntfType.Offset));
      // выделяем память под IMT
      IMTables[IntfRTTI.InterfaceID] := IMG.Position;
      IMT := IMG.MemoryPosition;
      IMG.IncPosition(MCnt*PTR_SIZE);
      // читаем список методотов класса для данного интерфейса
      for j := 0 to MCnt - 1 do begin
        MIdx := Stream.ReadStretchUInt;
        Method := GetILMethod(ClassType, MIdx);
        IMT[j] := TOffset(Method); // сохраняем для последующей корректировки
      end;
    end;
  end;
end;

procedure TILTranslator.ReadClassOfType(var Context: TReadTypeContext);
var
  Result: PRTTIPointer;
begin
  with Context do begin
    Result := PRTTIPointer(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := dtClassOf;
    Result.DataSize := PTR_SIZE;
    Result.RefTypeInfoOffset := ReadTypeSpecInfo(Stream);
  end;
end;

procedure TILTranslator.ReadClassTypeInfo(var Context: TReadTypeContext);
var
  i: Integer;
  Ancestor: PRTTIClass;
  Flags: UInt8;
  Methods: TILProcedures;
  VMTCount, MaxVirtIndex, VIdx: Integer;
  AncestorVMTCount: Integer;
  AVMT, VMT: PVMT;
  Result: PRTTIClass;
begin
  Result := PRTTIClass(GetTypeInfo(Context.ILType.Offset));
  Result.DataTypeID := dtClass;
  Result.DataSize := 0;
  with Context do
  begin
    Flags := Stream.ReadUInt8;
    if Check(Flags, ILTYPE_HAS_ANCESTOR) then
    begin
      Result.Ancestor := ReadTypeSpecInfo(Stream);
      Ancestor := PRTTIClass(GetTypeInfo(Result.Ancestor));
      AncestorVMTCount := Ancestor.VMTCount;
      Result.TotalMethodsCount := Ancestor.TotalMethodsCount;
      Result.TotalFieldsCount := Ancestor.TotalFieldsCount;
    end else begin
      Ancestor := nil;
      Result.Ancestor := 0;
      AncestorVMTCount := 0;
      Result.TotalMethodsCount := 0;
      Result.TotalFieldsCount := 0;
    end;

    Result.TypePacked := Check(Flags, ILTYPE_PACKED);

    if Check(Flags, ILTYPE_IMPORT) then
    begin
      ReadImportInfo(Stream, Result);
      MapImportType(Result);
    end else begin
      Result.ImportLib := 0;
      Result.ImportName := 0;
    end;

    {загрузка полей}
    ReadILFields(ILUnit, Stream, Result, ILType);
    Inc(Result.TotalFieldsCount, Length(ILType.Fields));

    {загрузка деклараций методов}
    ReadILMethodsDecls(ILUnit, Stream, Result, Methods);
    Inc(Result.TotalMethodsCount, Length(Methods));

    MaxVirtIndex := -1;
    for i := 0 to Length(Methods) - 1 do
      if (Methods[i].Flags and ILPROC_VIRTUAL) > 0 then
        MaxVirtIndex := Max(MaxVirtIndex, Methods[i].VirtualIndex);

    if MaxVirtIndex < AncestorVMTCount then
      VMTCount := AncestorVMTCount
    else
      VMTCount := MaxVirtIndex + 1;

    // тут необходимо скопировать основной образ данные по VMT

    VMT := PVMT(IMG.MemoryPosition);
    Result.VMT := IMG.Position;
    Result.VMTCount := VMTCount;
    IMG.IncPosition(VMTCount*PTR_SIZE);

    if Assigned(Ancestor) then
    begin
      AVMT := GetIMGPtr(Ancestor.VMT);
      Move(AVMT^, VMT^, AncestorVMTCount*PTR_SIZE);
    end;

    {заполняем VMT элементами TILProc}
    for i := 0 to Length(Methods) - 1 do
      if (Methods[i].Flags and ILPROC_VIRTUAL) > 0 then
      begin
        VIdx := Methods[i].VirtualIndex;
        VMT[VIdx] := NativeUInt(Methods[i]);
      end;

    ILType.Methods := Methods;

    if Check(Flags, ILTYPE_IMPL_INTERFACES) then
      ReadClassIntfaces(Context, Result);
  end;
end;

procedure TILTranslator.ReadDynArrayTypeInfo(var Context: TReadTypeContext);
var
  ElDt: TOffset;
  Result: PRTTIDynArray;
  ProcIndex: Integer;
begin
  with Context do begin
    Result := PRTTIDynArray(GetTypeInfo(Context.ILType.Offset));
    Result.DimensionsCount := 1;
    Result.DataTypeID := ILType.ID;
    Result.Flags := Stream.ReadUInt8;
    if Check(Result.Flags, ILARRAY_HAS_INIT) then
    begin
      ProcIndex := Stream.ReadStretchUInt;
      Result.InitProc := ProcIndex;
    end;
    if Check(Result.Flags, ILARRAY_HAS_FINAL) then
    begin
      ProcIndex := Stream.ReadStretchUInt;
      Result.FinalUIdx := ProcIndex;
      ProcIndex := Stream.ReadStretchUInt;
      Result.FinalProc := ProcIndex;
    end;
    ElDt := ReadTypeSpecInfo(Stream);
    Result.Dimensions := FSystemTypes[Ord(dtUInt32)].Offset;
    Result.ElementTypeInfoOffset := ElDt;
    Result.DataSize := PTR_SIZE;
  end;
end;

function TILTranslator.ReadVarStaticArrayValue(Stream: TStream; const Variable: PILVariable; ArrType: PRTTIArray): Pointer;
  function GetArrayElementCount(ArrType: PRTTIArray): Integer;
  var
    i: Integer;
    Dimentions: PRTTIDimensions;
    DimTypeInfo: PRTTIOrdinal;
  begin
    Result := 1;
    for i := 0 to ArrType.DimensionsCount - 1 do
    begin
      Dimentions := PRTTIDimensions(GetTypeInfo(ArrType.Dimensions));
      DimTypeInfo := PRTTIOrdinal(GetTypeInfo(Dimentions[i]));
      Result := Result * ((DimTypeInfo.HiBound - DimTypeInfo.LoBound) + 1);
    end;
  end;
var
  i, c: Integer;
  ElDt: PRTTIType;
  Ptr: PByte;
begin
  c := GetArrayElementCount(ArrType);
  ElDt := GetTypeInfo(ArrType.ElementTypeInfoOffset);
  Result := GetMemory(ElDt.DataSize*c);
  Ptr := Result;
  for i := 1 to c do
  begin
    Stream.Read(Ptr^, ElDt.DataSize);
    Inc(Ptr, ElDt.DataSize);
  end;
end;

function TILTranslator.ReadGlobalVarValue(Stream: TStream; const Variable: PILVariable): Pointer;
var
  VarType: PRTTIType;
  Idx: Integer;
begin
  VarType := GetTypeInfo(Variable.RTTI.DataType);
  case VarType.DataTypeID of
    dtStaticArray: Result := ReadVarStaticArrayValue(Stream, Variable, PRTTIArray(VarType));
    dtString, dtAnsiString: begin
      Result := GetMemory(VarType.DataSize);
      Idx := Stream.ReadStretchUInt();
      PNativeInt(Result)^ := FStrLiterals[Idx].Offset;
    end;
  else
    Result := GetMemory(VarType.DataSize);
    Stream.Read(Result^, VarType.DataSize);
  end;
end;

procedure TILTranslator.ReadILCode(Stream: TStream; Proc: TILProc);
var
  i,
  ILCount: Integer;
  ILCode: TILCode;
  Context: TILTContext;
  InstrByte, CondByte: UInt8;
  StartIMGPosition: Integer;
  Condition: TILCondition;
  ProcInfo: PRTTIProcedure;
  IL: TILInstructions;
  ILInstr: PILInstr;
begin
  StartIMGPosition := IMG.Position;
  ProcInfo := GetProcInfo(Proc.ProcInfo);
  ILCode := icRet;
  if Assigned(ProcInfo) then
    ProcInfo.ADDR := StartIMGPosition;

  ILCount := Stream.ReadStretchUInt;
  SetLength(IL, ILCount);
  FillChar(IL[0], Sizeof(TILInstruction)*ILCount, 0);
  Context.PUnit := Proc.ILUnit;
  Context.Proc := Proc;
  Context.Stream := Stream;
  Context.ILCount := ILCount;

  {вычитываем IL инструкции и их аргументы}
  for i := 0 to ILCount - 1 do begin
    InstrByte := Stream.ReadUInt8;
    ILCode := TILCode(InstrByte and 127);
    if (InstrByte and 128) <> 0 then
      CondByte := Stream.ReadUInt8
    else
      CondByte := 0;
    Condition := TILCondition(CondByte and 15);
    Context.Cond := Condition;
    Context.ILCode := ILCode;
    ILInstr := @(IL[i]);
    ILInstr.Cond := Condition;
    ILInstr.Code := ILCode;
    Context.Args := nil;
    Context.ILIndex := i;
    try
      case ILCode of
        icNope: Translate_NOPE(Context);
        icNeg,
        icNot: Read_NOT(Context);
        icCmp: Read_CMP(Context);
        icCmpJmp: Read_CMPJ(Context);
        icTest: Read_TEST(Context);
        icCovert: Read_CONVERT(Context);
        icCheckBound: Read_CHKBND(Context);
        icGetBit: Read_GETBIT(Context);
        icSetBit: Read_SETBIT(Context);
        icSetBool: Read_SETBOOL(Context);
        icMove: Read_MOVE(Context);
        icMoveZero: Read_MOVEZERO(Context);
        icLea, icLea2: Read_LEA(Context);
        icAdd2, icSub2, icMul2, icIntDiv2, icDiv2, icModDiv2,
        icAnd2, icOr2, icXor2, icShl2, icShr2, icRol2,
        icRor2: begin
          {преобразуем двух-адресную в трех-адресную команду}
          Context.ILCode := TILCode(Ord(ILCode) - (Ord(icRor2) - Ord(icAdd2) + 1));
          Read_2OperandOpCode(Context);
        end;
        icInc, icDec: Read_INC(Context);
        icAdd, icSub, icMul, icIntDiv, icDiv, icModDiv,
        icAnd, icOr, icXor, icShl, icShr, icRol, icRor: Read_3OperandOpCode(Context);
        icGetPtr: Read_GETPTR(Context);
        icGetSelfPtr: Read_GETSPTR(Context);
        icLoadMethod: Read_LDMETHOD(Context);
        icLoadSelfMethod: Read_LDSMETHOD(Context);
        icReadDRef: Read_RDREF(Context);
        icWriteDRef: Read_WDREF(Context);
        icUnique: Read_UNIQUE(Context);
        icArrayDAlloc,
        icArrayRAlloc: Read_ARRDALLOC(Context);
        icArrayLength: Read_ARRAYLENGTH(Context);
        icArrayCopy: Read_ARRAYCOPY(Context);
        icMemMove: Read_ARRAYMOVE(Context);
        icMemSet: Read_MEMSET(Context);
        icTypeInfo: Read_TYPEINFO(Context);
        icQueryType: Read_QUERYTYPE(Context);
        icRefCount: Read_REFCNT(Context);
        icNow: Read_NOW(Context);
        icMemGet: Read_MEMGET(Context);
        icMemFree: Read_MEMFREE(Context);
        icDNewObj: Read_DNEWOBJ(Context);
        icTryBegin: Read_TRYBEGIN(Context);
        icTryEnd: Read_TRYEND(Context);
        icNearCall: Read_NEARCALL(Context);
        icEThrow: Read_ETHROW(Context);
        icInhtCall: Read_INHT_CALL(Context);
        icProcCall, icVirtCall: Read_PCALL(Context);
        icInit: Read_INIT(Context);
        icIncRef: Read_INCREF(Context);
        icDecRef: Read_DECREF(Context);
        icDecRefFinal: Read_DECREFFINAL(Context);
        icWeakRef: Read_WEAKREF(Context);
        icStrongRef: Read_STRONGREF(Context);
        icJmp: Read_JMP(Context);
        icRet: Read_RET(Context);
        icFMacro: Read_FMACRO(Context);
        icPlatform:
          AbortWork('Platform instructions are not supported by VM');
      else
        AbortWork('Unknown IL instruction: %s', [GetILCodeName(ILCode)]);
      end;
      if IncludeDebugInfo then
        ILInstr.Line := Stream.ReadStretchUInt;

      ILInstr.Args := Context.Args;
    except
      on e: exception do
        raise Exception.CreateFmt('Read IL code ERROR[unit: %s proc: %s; ilcode: %s; illine: %d]: %s',
                                  [Proc.ILUnit.Name, GetProcName(Proc), GetILCodeName(ILCode), i, e.message]);
    end;
  end;

  Proc.IL := IL;

  {пролог процедуры}
  WriteProcProlog(Proc);

  {трансляция IL инструкций}
  for i := 0 to ILCount - 1 do
  begin
    ILInstr := @(IL[i]);
    ILCode := ILInstr.Code;
    Context.ILIndex := i;
    Context.Cond := ILInstr.Cond;
    Context.ILCode := ILCode;
    Context.Args := ILInstr.Args;
    ILInstr.Offset := IMG.Position;
    try
      BeforeInstruction(Context);
      case ILCode of
        icNope: Translate_NOPE(Context);
        icNeg,
        icNot: Translate_NOT(Context, TIL_NOT_ARGS(Context.Args));
        icCmp: Translate_CMP(Context, TIL_CMP_ARGS(Context.Args));
        icCmpJmp: Translate_CMPJ(Context, TIL_CMP_ARGS(Context.Args));
        icTest: Translate_TEST(Context, TIL_CMP_ARGS(Context.Args));
        icCovert: Translate_CONVERT(Context, TIL_CONVERT_ARGS(Context.Args));
        icCheckBound: Translate_CHKBND(Context, TIL_CHCKBND_ARGS(Context.Args));
        icGetBit: Translate_GETBIT(Context, TIL_GETBIT_ARGS(Context.Args));
        icSetBit: Translate_SETBIT(Context, TIL_SETBIT_ARGS(Context.Args));
        icSetBool: Translate_SETBOOL(Context, TIL_SETBOOL_ARGS(Context.Args));
        icMove: Translate_MOVE(Context, TIL_MOVE_ARGS(Context.Args));
        icMoveZero: Translate_MOVEZERO(Context, TIL_MOVEZERO_ARGS(Context.Args));
        icLea, icLea2: Translate_LEA(Context, TIL_LEA_ARGS(Context.Args));
        icAdd2, icSub2, icMul2, icIntDiv2, icDiv2, icModDiv2,
        icAnd2, icOr2, icXor2, icShl2, icShr2, icRol2,
        icRor2: begin
          {преобразуем двух-адресную в трех-адресную команду}
          Context.ILCode := TILCode(Ord(ILCode) - (Ord(icRor2) - Ord(icAdd2) + 1));
          Translate_2OperandOpCode(Context, TIL_DS_ARGS(Context.Args));
        end;
        icInc, icDec: Translate_INC(Context, TIL_INC_ARGS(Context.Args));
        icAdd, icSub, icMul, icIntDiv, icDiv, icModDiv,
        icAnd, icOr, icXor, icShl, icShr, icRol, icRor: Translate_3OperandOpCode(Context, TIL_DSS_ARGS(Context.Args));
        icGetPtr: Translate_GETPTR(Context, TIL_GETPTR_ARGS(Context.Args));
        icGetSelfPtr: Translate_GETSPTR(Context, TIL_GETSPTR_ARGS(Context.Args));
        icLoadMethod: Translate_LDMETHOD(Context, TIL_LDMETHOD_ARGS(Context.Args));
        icLoadSelfMethod: Translate_LDSMETHOD(Context, TIL_LDSMETHOD_ARGS(Context.Args));
        icReadDRef: Translate_RDREF(Context, TIL_RDREF_ARGS(Context.Args));
        icWriteDRef: Translate_WDREF(Context, TIL_WDREF_ARGS(Context.Args));
        icUnique: Translate_UNIQUE(Context, TIL_D_ARGS(Context.Args));
        icArrayDAlloc,
        icArrayRAlloc: Translate_ARRDALLOC(Context, TIL_ARRDALLOC_ARGS(Context.Args));
        icArrayLength: Translate_ARRAYLENGTH(Context, TIL_ARRAYLENGTH_ARGS(Context.Args));
        icArrayCopy: Translate_ARRAYCOPY(Context, TIL_ARRAYCOPY_ARGS(Context.Args));
        icMemMove: Translate_ARRAYMOVE(Context, TIL_ARRAYMOVE_ARGS(Context.Args));
        icMemSet: Translate_MEMSET(Context, TIL_MEMSET_ARGS(Context.Args));
        icTypeInfo: Translate_TYPEINFO(Context, TIL_TYPEINFO_ARGS(Context.Args));
        icQueryType: Translate_QUERYTYPE(Context, TIL_QUERYTYPE_ARGS(Context.Args));
        icRefCount: Translate_REFCNT(Context, TIL_DS_ARGS(Context.Args));
        icNow: Translate_NOW(Context, TIL_D_ARGS(Context.Args));
        icMemGet: Translate_MEMGET(Context, TIL_DNEW_ARGS(Context.Args));
        icMemFree: Translate_MEMFREE(Context, TIL_DFREE_ARGS(Context.Args));
        icDNewObj: Translate_DNEWOBJ(Context, TIL_DNEWOBJ_ARGS(Context.Args));
        icTryBegin: Translate_TRYBEGIN(Context);
        icTryEnd: Translate_TRYEND(Context);
        icNearCall: Translate_NEARCALL(Context, TIL_D_ARGS(Context.Args));
        icEThrow: Translate_ETHROW(Context, TIL_ETHROW_ARGS(Context.Args));
        icInhtCall: Translate_PCALL(Context, TIL_PCALL_ARGS(Context.Args));
        icProcCall, icVirtCall: Translate_PCALL(Context, TIL_PCALL_ARGS(Context.Args));
        icInit: Translate_INIT(Context, TIL_INIT_ARGS(Context.Args));
        icIncRef: Translate_INCREF(Context, TIL_INCREF_ARGS(Context.Args));
        icDecRef: Translate_DECREF(Context, TIL_DECREF_ARGS(Context.Args));
        icDecRefFinal: Translate_DECREFFINAL(Context, TIL_DECREFFINAL_ARGS(Context.Args));
        icWeakRef: Translate_WEAKREF(Context, TIL_WEAKREF_ARGS(Context.Args));
        icStrongRef: Translate_STRONGREF(Context, TIL_STRONGREF_ARGS(Context.Args));
        icJmp: Translate_JMP(Context, TIL_JMP_ARGS(Context.Args));
        icRet: Translate_RET(Context);
        icFMacro: Translate_FMACRO(Context, TIL_FMACRO_ARGS(Context.Args));
        icPlatform:
          AbortWork('Platform instructions are not supported by VM');
      else
        AbortWork('Unknown IL instruction: %s', [GetILCodeName(ILCode)]);
      end;
      AfterInstruction(Context);
    except
      on e: exception do
        raise Exception.CreateFmt('Translate IL code ERROR[unit: %s proc: %s; ilcode: %s; illine: %d]: %s',
                                  [Proc.ILUnit.Name, GetProcName(Proc), GetILCodeName(ILCode), i, e.message]);
    end;
  end;

  // добавляем финальный _RET
  WriteProcEpilog(Proc, ILCode);

  // размер кода
  Proc.CodeSize := IMG.Position - StartIMGPosition;

  // корректировка размера стека
  PNativeUInt(PByte(GetIMGPtr(StartIMGPosition)) + PTR_SIZE)^ := Proc.GetStackSize;

  // Коррекция адресов джампов
  CorrectProcJMPOffsets(Proc);
end;

procedure TILTranslator.ReadILFields(ILUnit: TILUnit; Stream: TStream; Struct: PRTTIStruct; ILType: PILType);
var
  i, CNT, Idx: Integer;
  FieldOffset, FieldSize: Integer;
  Prefix: Integer;
  Fields: TRTTIFields;
  Field: PRTTIField;
  TypeInfo: PRTTIType;
  Variables: TILVariables;
  AbsoluteIndex: Integer;
  AbsoluteOffset: Integer;
  AVar: PILVariable;
  FieldsDataSize: Integer;   // общий размер всех полей (с учетом предка)
  Ancestor: PRTTIStruct;
begin
  {кол-во полей}
  CNT := Stream.ReadStretchUInt;
  Struct.FieldsCount := CNT;
  if CNT > 0 then
  begin
    // пишем заголовок дин. массива
    FRTTI.WriteNativeInt(1);  // refcnt
    FRTTI.WriteNativeInt(CNT); // len
    Struct.Fields := FRTTI.Position;
    Fields := TRTTIFields(FRTTI.MemoryPosition);
    {выделяем память для RTTI полей}
    FRTTI.IncPosition(SizeOf(TRTTIField)*CNT);
    FillChar(Fields[0], SizeOf(TRTTIField)*CNT, #0);
    SetLength(Variables, CNT);
  end else begin
    Struct.Fields := 0;
    Fields := nil;
  end;

  if Struct.Ancestor > 0 then
  begin
    Ancestor := PRTTIStruct(GetTypeInfo(Struct.Ancestor));
    FieldsDataSize := Ancestor.DataSize;
    if Struct.DataTypeID = dtClass then
      FieldOffset := FieldsDataSize - 4*PTR_SIZE
    else
    FieldOffset := FieldsDataSize;
  end else begin
    FieldsDataSize := 0;
    if Struct.DataTypeID = dtClass then
      FieldOffset := -4*PTR_SIZE
    else
    FieldOffset := 0;
  end;

  for i := 0 to CNT - 1 do begin
    Prefix := Stream.ReadUInt8;
    Field := Addr(Fields[i]);
    {тип поля}
    Field.DataType := ReadVarType(ILUnit, Stream, Prefix);

    {absolute индекс}
    if (ILVAR_HASINDEX and Prefix) <> 0 then begin
      AbsoluteIndex := Stream.ReadStretchUInt;
      AbsoluteOffset := Stream.ReadStretchUInt;
    end else begin
      AbsoluteIndex := -1;
      AbsoluteOffset := -1;
    end;

    {имя поля}
    if IncludeDebugInfo then begin
      Idx := Stream.ReadStretchUInt;
      Field.Name := FStrLiterals[Idx].Offset;
    end;
    TypeInfo := GetTypeInfo(Field.DataType);
    if AbsoluteIndex = -1 then begin
      Field.Offset := FieldOffset;

      if cDataTypeManaged[TypeInfo.DataTypeID] then
        FieldSize := PTR_SIZE
      else
        FieldSize := TypeInfo.DataSize;

      if Struct.TypePacked then
      begin
        FieldOffset := FieldOffset + FieldSize;
        Inc(FieldsDataSize, FieldSize);
      end else begin
        FieldOffset := FieldOffset + MemAlign(FieldSize);
        Inc(FieldsDataSize, MemAlign(FieldSize));
      end;
    end else begin
      AVar := addr(Variables[AbsoluteIndex]);
      Field.Offset := AVar.RTTI.Offset;
      Variables[i].AbsoluteTo := AVar;
      Variables[i].AbsoluteOffset := AbsoluteOffset;
      // необходимо правильно расчитать размер структуры, сейчас код работает только если размер absolute поля <= dest поля
    end;
    Variables[i].Name := GetString(Field.Name);
    Variables[i].RTTI := Field^;
  end;
  Inc(Struct.DataSize, FieldsDataSize);
  ILType.Fields := Variables;
end;

procedure TILTranslator.ReadILMethodDecl(ILUnit: TILUnit; Stream: TStream; ProcInfo: PRTTIProcedure);
var
  Flags: UInt8;
  NameIndex: Integer;
  Params: PRTTIParams;
  TypeInfo: PRTTIType;
begin
  ProcInfo.RefCount := -1;
  ProcInfo.FClassID := rttiProc;

  Flags := Stream.ReadUInt8;
  ProcInfo.Flags := Flags;

  {call convention}
  ProcInfo.CallConvention := TCallConvention(Stream.ReadUInt8);

  {имя процедуры}
  if IncludeDebugInfo then
    ProcInfo.Name := FStrLiterals[Stream.ReadStretchUInt].Offset;

  {импортируемая процедура}
  if (Flags and ILPROC_IMPORT) <> 0 then begin
    NameIndex := Stream.ReadStretchUInt;
    ProcInfo.ImportLib := FStrLiterals[NameIndex].Offset;
    NameIndex := Stream.ReadStretchUInt;
    ProcInfo.ImportName := FStrLiterals[NameIndex].Offset;
    Inc(FImportTableCount);
  end;

  if (Flags and ILPROC_VIRTUAL) <> 0 then
    ProcInfo.VirtualIndex := Stream.ReadStretchUInt
  else
    ProcInfo.VirtualIndex := -1;

  {парметры}
  ProcInfo.ParamsCount := Stream.ReadStretchUInt;

  if ProcInfo.ParamsCount > 0 then
  begin
    ProcInfo.Params := FRTTI.Position;
    Params := FRTTI.MemoryPosition;
    {$IFDEF DEBUG}FillChar(Params^, ProcInfo.ParamsCount*SizeOf(TRttiParameter), #0);{$ENDIF}
    FRTTI.IncPosition(ProcInfo.ParamsCount*SizeOf(TRttiParameter));
    LoadILParameter(ILUnit, Stream, ProcInfo.ParamsCount, Params);
    if (Flags and ILPROC_HASRESULT) > 0 then
    begin
      ProcInfo.ResultType := Params[0].DataType;
      {если структурный value-тип то передаем его по ссылке}
      TypeInfo := GetTypeInfo(ProcInfo.ResultType);
      if TypeInfo.DataTypeID in [dtRecord, dtGuid, dtStaticArray] then
        Params[0].IsReference := True;
    end;
  end else
    ProcInfo.Params := 0;
end;

procedure TILTranslator.ReadILMethodsDecls(ILUnit: TILUnit; Stream: TStream; Struct: PRTTIStruct; var Procedures: TILProcedures);
var
  i, MCount: Integer;
  Methods: PRTTIProcedures;
  Method: PRTTIProcedure;
  StructOffset: TOffset;
  Proc: TILProc;
begin
  MCount := Stream.ReadStretchUInt;
  Struct.MethodsCount := MCount;
  SetLength(Procedures, MCount);

  if MCount > 0 then
  begin
    Struct.Methods := FRTTIProcs.Position;
    Methods := PRTTIProcedures(FRTTIProcs.MemoryPosition);
    FRTTIProcs.IncPosition(SizeOf(IL.TypeInfo.TRTTIProcedure)*MCount);
    FillChar(Methods^, SizeOf(IL.TypeInfo.TRTTIProcedure)*MCount, #0);

    StructOffset := GetOffset(FRTTI.Memory, Struct);
    for i := 0 to MCount -  1 do begin
      Method := addr(Methods[i]);
      Proc := CreateILUnitProc(ILUnit);
      Procedures[i] := Proc;
      Method.StructInfo := StructOffset;
      ReadILMethodDecl(ILUnit, Stream, Method);
      Proc.ILUnit := ILUnit;
      Proc.IsIntfMethod := Assigned(Struct) and (Struct.DataTypeID = dtInterface);

      if Assigned(Struct) then
        Proc.Struct := GetILType(ILUnit.Index, Struct.Index);

      {копируем необходимые поля в структру TILProc}
      RTTIProcToILProc(Method, Proc);

      if (Struct.ImportLib > 0) or (Proc.ImportLib > 0) then
        MapImportMethod(Struct, Proc);
    end;
  end else
    Struct.Methods := 0;
end;

procedure TILTranslator.ReadImportInfo(Stream: TStream; TypeInfo: PRTTIType);
var
  Index: Integer;
begin
  Index := Stream.ReadStretchUInt;
  TypeInfo.ImportLib := FStrLiterals[Index].Offset;
  Index := Stream.ReadStretchUInt;
  TypeInfo.ImportName := FStrLiterals[Index].Offset;
end;

procedure TILTranslator.ReadIntfTypeInfo(var Context: TReadTypeContext);
var
  Flags: UInt8;
  Procs: TILProcedures;
  i, FieldsCount: Integer;
  ARTTI: PRTTIInterface;
  StartMethodIdx: Integer;
  Result: PRTTIInterface;
begin
  Result := PRTTIInterface(GetTypeInfo(Context.ILType.Offset));
  with Context do begin
    Flags := Stream.ReadUInt8;
    if (Flags and 1) = 1 then //STRUCT_HAS_ANCESTOR
    begin
      Result.Ancestor := ReadTypeSpecInfo(Stream);
      ARTTI := PRTTIInterface(GetTypeInfo(Result.Ancestor));
      StartMethodIdx := ARTTI.MethodsCount;
      Result.TotalMethodsCount := ARTTI.TotalMethodsCount;
    end else begin
      Result.Ancestor := 0;
      StartMethodIdx := 0;
      Result.TotalMethodsCount := 0;
    end;
    Result.TotalFieldsCount := 0;

    if (Flags and 2) = 2 then //STRUCT_IMPORT
    begin
      ReadImportInfo(Stream, Result);
      // MapImportType(Result);
    end else begin
      Result.ImportLib := 0;
      Result.ImportName := 0;
    end;

    Result.DataTypeID := dtInterface;
    Result.DataSize := PTR_SIZE;
    FieldsCount := Stream.ReadStretchUInt;
    Assert(FieldsCount = 0); // кол-во полей, для интерфейса всегда ноль!
    ReadILMethodsDecls(ILUnit, Stream, Result, Procs);
    ILType.Methods := Procs;
    Inc(Result.TotalMethodsCount, Length(Procs));

    for i := 0 to Length(Procs) - 1 do
      Procs[i].Offset := StartMethodIdx + i;

    // читаем GUID
    Result.GUID := Stream.ReadGuid();
  end;
end;

procedure TILTranslator.ReadOrdinalTypeInfo(var Context: TReadTypeContext);
var
  Result: PRTTIOrdinal;
begin
  with Context do begin
    Result := PRTTIOrdinal(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := ILType.ID;
    Result.Signed := Stream.ReadBoolean;
    Result.LoBound := Stream.ReadInt64;
    Result.HiBound := Stream.ReadInt64;
    Result.DataSize := GetRangeByteSize(Result.HiBound, Result.LoBound);
  end;
end;

procedure TILTranslator.ReadPointerType(var Context: TReadTypeContext);
var
  Result: PRTTIPointer;
begin
  with Context do begin
    Result := PRTTIPointer(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := dtPointer;
    Result.DataSize := PTR_SIZE;
    Result.RefTypeInfoOffset := ReadTypeSpecInfo(Stream);
  end;
end;

procedure TILTranslator.ReadProcTypeInfo(var Context: TReadTypeContext);
type
  TFlags = set of (ProcHasResult, ProcStatic);
var
  c: Integer;
  Flags: Byte;
  Params: PRTTIParams;
  Result: PRTTIProcType;
begin
  with Context do begin
    Result := PRTTIProcType(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := dtProcType;
    {флаги}
    Flags := Stream.ReadStretchUInt;
    Result.ProcStatic := (ProcStatic in TFlags(Flags));

    if Result.ProcStatic then
      Result.DataSize := PTR_SIZE
    else
      Result.DataSize := PTR_SIZE*2;

    {параметры}
    c := Stream.ReadStretchUInt;
    Result.ParamsCount := c;
    Result.Params := FRTTI.Position;
    FRTTI.IncPosition(c*SizeOf(TRTTIParameter));
    Params := PRTTIParams(GetTypeInfo(Result.Params));
    FillChar(Params^, c*SizeOf(TRTTIParameter), #0);
    LoadILParameter(ILUnit, Stream, c, Params);

    if Check(Flags, ILPROC_HASRESULT) then
      Result.ResultType := Params[0].DataType;
  end;
end;

procedure TILTranslator.ReadRecordTypeInfo(var Context: TReadTypeContext);
var
  Ancestor: PRTTIStruct;
  Flags: UInt8;
  Procs: TILProcedures;
  Result: PRTTIRecord;
begin
  Result := PRTTIRecord(GetTypeInfo(Context.ILType.Offset));
  Result.DataTypeID := dtRecord;
  with Context do begin
    Flags := Stream.ReadUInt8;
    if (Flags and ILTYPE_HAS_ANCESTOR) <> 0 then begin
      Result.Ancestor := ReadTypeSpecInfo(Stream);
      Ancestor := PRTTIStruct(GetTypeInfo(Result.Ancestor));
      Result.DataSize := Ancestor.DataSize;
      Result.TotalMethodsCount := Ancestor.TotalMethodsCount;
      Result.TotalFieldsCount := Ancestor.TotalFieldsCount;
    end else begin
      Result.Ancestor := 0;
      Result.TotalMethodsCount := 0;
      Result.TotalFieldsCount := 0;
    end;

    if (Flags and ILTYPE_IMPORT) <> 0 then //STRUCT_IMPORT
    begin
      ReadImportInfo(Stream, Result);
    end else begin
      Result.ImportLib := 0;
      Result.ImportName := 0;
    end;

    Result.TypePacked := (Flags and ILTYPE_PACKED) <> 0;

    {загрузка полей}
    ReadILFields(ILUnit, Stream, Result, ILType);
    Inc(Result.TotalFieldsCount, Length(ILType.Fields));

    {загрузка методов}
    ReadILMethodsDecls(ILUnit, Stream, Result, Procs);
    Inc(Result.TotalMethodsCount, Length(Procs));

    ILType.Methods := Procs;
  end;
end;

procedure TILTranslator.ReadSetTypeInfo(var Context: TReadTypeContext);
var
  DimInfoOffset: TOffset;
  DimInfo: PRTTIOrdinal;
  BitsCount: Integer;
  Result: PRTTISet;
begin
  with Context do begin
    Result := PRTTISet(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := dtSet;
    Result.DimensionsCount := 1;
    Result.ElementTypeInfoOffset := 0; // bit
    DimInfoOffset := ReadTypeSpecInfo(Stream);
    Result.Dimensions := DimInfoOffset;
    DimInfo := PRTTIOrdinal(GetTypeInfo(DimInfoOffset));
    // вычисляем размер сета
    BitsCount := Int64(DimInfo.HiBound) - DimInfo.LoBound + 1;
    Result.DataSize := (BitsCount div 8) + ifthen((BitsCount mod 8) <> 0, 1, 0);
  end;
end;

function TILTranslator.ReadTypeSpecInfo(Stream: TStream): TOffset;
var
  UnitIdx,
  TypeIdx: Integer;
  U: TILUnit;
begin
  TypeIdx := Stream.ReadStretchUInt;

  if TypeIdx < TSystemTypes.Count then
    Exit(FSystemTypes[TypeIdx].Offset);

  UnitIdx := Stream.ReadStretchUInt;
  U := FUnits[UnitIdx];
  Result := U.Types[TypeIdx - TSystemTypes.Count].Offset;
end;

function TILTranslator.ReadVarType(ILUnit: TILUnit; Stream: TStream; VarFlags: Integer): TOffset;
var
  UnitIdx,
  TypeIdx: Integer;
  U: TILUnit;
begin
  if (VarFlags and ILVAR_UNITTYPE) <> 0  then
  begin
    UnitIdx := Stream.ReadStretchUInt;
    U := FUnits[UnitIdx];
  end else
    U := ILUnit;

  TypeIdx := Stream.ReadStretchUInt;

  if TypeIdx < TSystemTypes.Count then
    Exit(FSystemTypes[TypeIdx].Offset);

  TypeIdx := TypeIdx - TSystemTypes.Count;
  if TypeIdx < Length(U.Types) then
    Result := U.Types[TypeIdx].Offset
  else begin
    AbortWork('Ivalid type index[unit: %s]: %d', [ILUnit.Name, TypeIdx]);
    Result := 0;
  end;
end;

procedure TILTranslator.ReadWeakRefType(var Context: TReadTypeContext);
var
  Result: PRTTIPointer;
begin
  with Context do begin
    Result := PRTTIPointer(GetTypeInfo(Context.ILType.Offset));
    Result.DataTypeID := dtWeakRef;
    Result.DataSize := PTR_SIZE;
    Result.RefTypeInfoOffset := ReadTypeSpecInfo(Stream);
  end;
end;

procedure TILTranslator.Read_2OperandOpCode(var Ctx: TILTContext);
var
  Args: TIL_DS_Args;
begin
  Args := TIL_DS_Args.Create;
  // приемник (он же источник 1)
  LoadILArgument(Ctx, Args.D);
  // источник 2
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_3OperandOpCode(var Ctx: TILTContext);
var
  Args: TIL_DSS_Args;
begin
  Args := TIL_DSS_Args.Create;
  LoadILArgument(Ctx, Args.D);  // приемник
  LoadILArgument(Ctx, Args.L);  // источник 1
  LoadILArgument(Ctx, Args.R);  // источник 2
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_ARRAYCOPY(var Ctx: TILTContext);
var
  Args: TIL_ARRAYCOPY_ARGS;
begin
  Args := TIL_ARRAYCOPY_ARGS.Create;
  LoadILArgument(Ctx, Args.D); // dst
  LoadILArgument(Ctx, Args.S); // src
  LoadILArgument(Ctx, Args.F); // from
  LoadILArgument(Ctx, Args.L); // length
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_ARRAYLENGTH(var Ctx: TILTContext);
var
  Args: TIL_ARRAYLENGTH_ARGS;
begin
  Args := TIL_ARRAYLENGTH_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_ARRAYMOVE(var Ctx: TILTContext);
var
  Args: TIL_ARRAYMOVE_ARGS;
begin
  Args := TIL_ARRAYMOVE_ARGS.Create;
  LoadILArgument(Ctx, Args.SrcArr);
  LoadILArgument(Ctx, Args.SrcIdx);
  LoadILArgument(Ctx, Args.DstArr);
  LoadILArgument(Ctx, Args.DstIdx);
  LoadILArgument(Ctx, Args.Cnt);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_ARRDALLOC(var Ctx: TILTContext);
var
  Args: TIL_ARRDALLOC_ARGS;
begin
  Args := TIL_ARRDALLOC_ARGS.Create;
  LoadILArgument(Ctx, Args.D); // dest
  LoadILArgument(Ctx, Args.S); // length
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_CHKBND(var Ctx: TILTContext);
var
  Args: TIL_CHCKBND_ARGS;
begin
  Args := TIL_CHCKBND_ARGS.Create;
  LoadILArgument(Ctx, Args.AArray);
  LoadILArgument(Ctx, Args.AIndex);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_CMP(var Ctx: TILTContext);
var
  Args: TIL_CMP_ARGS;
begin
  Args := TIL_CMP_ARGS.Create;
  LoadILArgument(Ctx, Args.L);
  LoadILArgument(Ctx, Args.R);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_CMPJ(var Ctx: TILTContext);
var
  Args: TIL_CMPJ_ARGS;
begin
  Args := TIL_CMPJ_ARGS.Create;
  LoadILArgument(Ctx, Args.L);
  LoadILArgument(Ctx, Args.R);
  //LoadILArgument(Ctx, Args.D);
  //  todo
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_CONVERT(var Ctx: TILTContext);
var
  Args: TIL_CONVERT_ARGS;
begin
  Args := TIL_CONVERT_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_DECREF(var Ctx: TILTContext);
var
  Args: TIL_DECREF_ARGS;
begin
  Args := TIL_DECREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_DECREFFINAL(var Ctx: TILTContext);
var
  Args: TIL_DECREFFINAL_ARGS;
  TypeInfo: PRTTIType;
begin
  Args := TIL_DECREFFINAL_ARGS.Create;
  // загружаем первый аргумент - переменную
  LoadILArgument(Ctx, Args.Dst);
  TypeInfo := GetTypeInfo(Args.Dst);
  Assert(Args.Dst.ArgClass = ARG_VAR);
  // загружаем аргумет - процедуру/метод финализации
  if TypeInfo.DataTypeID = dtClass then
    LoadILArgumentInternal(Ctx, Args.FinalProc, Args.Dst)
  else
    LoadILArgument(Ctx, Args.FinalProc);

  Ctx.Args := Args;
end;

procedure TILTranslator.Read_MEMFREE(var Ctx: TILTContext);
var
  Args: TIL_DFREE_ARGS;
begin
  Args := TIL_DFREE_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_MEMGET(var Ctx: TILTContext);
var
  Args: TIL_DNEW_ARGS;
begin
  Args := TIL_DNEW_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_DNEWOBJ(var Ctx: TILTContext);
var
  Args: TIL_DNEWOBJ_ARGS;
begin
  Args := TIL_DNEWOBJ_ARGS.Create;
  LoadILArgument(Ctx, Args.S);
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_ETHROW(var Ctx: TILTContext);
var
  Args: TIL_ETHROW_ARGS;
begin
  Args := TIL_ETHROW_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_FMACRO(var Ctx: TILTContext);
var
  Args: TIL_FMACRO_ARGS;
begin
  Args := TIL_FMACRO_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.M);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_GETBIT(var Ctx: TILTContext);
var
  Args: TIL_GETBIT_ARGS;
begin
  Args := TIL_GETBIT_ARGS.Create;
  LoadILArgument(Ctx, Args.Dst);
  LoadILArgument(Ctx, Args.BitArray);
  LoadILArgument(Ctx, Args.BitIndex);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_GETPTR(var Ctx: TILTContext);
var
  Args: TIL_GETPTR_ARGS;
begin
  Args := TIL_GETPTR_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgumentInternal(Ctx, Args.S, nil);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_GETSPTR(var Ctx: TILTContext);
var
  Args: TIL_GETSPTR_ARGS;
begin
  Args := TIL_GETSPTR_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Args.B := ArgGet();
  Args.B.ArgClass := ARG_VAR;
  Args.B.ArgScope := ARG_SCOPE_LOCAL;
  if Ctx.Proc.ProcType = ptProcedure then
    Args.B.AsVariable := Addr(Ctx.Proc.Vars[0])
  else
    Args.B.AsVariable := Addr(Ctx.Proc.Vars[1]);
  Args.B.TypeInfo := Args.B.AsVariable.RTTI.DataType;
  if Args.B.AsVariable.RTTI.IsReference then
    Args.B.ArgumentType := atReference
  else
    Args.B.ArgumentType := atLocal;

  LoadILArgumentInternal(Ctx, Args.S, Args.B);
  Args.B.Next := Args.S;

  Ctx.Args := Args;
end;

procedure TILTranslator.Read_INC(var Ctx: TILTContext);
var
  Args: TIL_INC_ARGS;
begin
  Args := TIL_INC_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_INCREF(var Ctx: TILTContext);
var
  Args: TIL_INCREF_ARGS;
begin
  Args := TIL_INCREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_INHT_CALL(var Ctx: TILTContext);
var
  Args: TIL_PCALL_ARGS;
  MethodIdx, i, Cnt: Integer;
  Struct: PRTTIStruct;
  IMGType: ^TIMGType;
  M: TILMethod;
  TypeArg: TILArgument;
  SelfArg: TILArgument;
begin
  Args := TIL_PCALL_ARGS.Create;
  LoadILArgument(Ctx, TypeArg);
  Struct := PRTTIStruct(TypeArg.AsTypeInfo);
  IMGType := addr(FUnits[Struct.UnitID].Types[Struct.Index]);
  Assert(Struct.DataTypeID = dtClass);
  MethodIdx := Ctx.Stream.ReadStretchUInt;
  LoadILArgumentInternal(Ctx, SelfArg, nil);
  Args.PArg := ArgGet();
  Args.PArg.ArgumentType := atImmConst;
  Args.PArg.ArgClass := ARG_METHOD;
  Args.PArg.ArgScope := ARG_SCOPE_STRUCT;
  M.Self := SelfArg;
  M.Proc := IMGType.Methods[MethodIdx];
  Args.PArg.AsMethod := M;

  // читаем аргументы вызова
  cnt := Ctx.Stream.ReadStretchUInt;
  SetLength(Args.CallArgs, cnt);
  for i := 0 to cnt - 1 do
    LoadILArgument(Ctx, Args.CallArgs[i]);

  Ctx.Args := Args;
end;

procedure TILTranslator.Read_INIT(var Ctx: TILTContext);
var
  Args: TIL_INIT_ARGS;
begin
  Args := TIL_INIT_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_JMP(var Ctx: TILTContext);
var
  Args: TIL_JMP_ARGS;
begin
  Args := TIL_JMP_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_LDMETHOD(var Ctx: TILTContext);
var
  Args: TIL_LDMETHOD_ARGS;
begin
  Args := TIL_LDMETHOD_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgumentInternal(Ctx, Args.S, nil);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_LDSMETHOD(var Ctx: TILTContext);
var
  Args: TIL_LDSMETHOD_ARGS;
begin
  Args := TIL_LDSMETHOD_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Args.B := ArgGet();
  Args.B.ArgClass := ARG_VAR;
  Args.B.ArgScope := ARG_SCOPE_LOCAL;
  if Ctx.Proc.ProcType = ptProcedure then
    Args.B.AsVariable := Addr(Ctx.Proc.Vars[0])
  else
    Args.B.AsVariable := Addr(Ctx.Proc.Vars[1]);
  Args.B.TypeInfo :=  Args.B.AsVariable.RTTI.DataType;
  if Args.B.AsVariable.RTTI.IsReference then
    Args.B.ArgumentType := atReference
  else
    Args.B.ArgumentType := atLocal;

  LoadILArgumentInternal(Ctx, Args.S, Args.B);
  Args.B.Next := Args.S;

  Ctx.Args := Args;
end;

procedure TILTranslator.Read_LEA(var Ctx: TILTContext);
var
  Args: TIL_LEA_Args;
begin
  Args := TIL_LEA_Args.Create;
  LoadILArgument(Ctx, Args.D); // приемник
  LoadILArgument(Ctx, Args.B); // база
  if Ctx.ILCode = icLea then
    LoadILArgument(Ctx, Args.Offset); // смещение
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_MEMSET(var Ctx: TILTContext);
var
  Args: TIL_MEMSET_ARGS;
begin
  Args := TIL_MEMSET_ARGS.Create;
  LoadILArgument(Ctx, Args.Dst);
  LoadILArgument(Ctx, Args.Bpt);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_MOVE(var Ctx: TILTContext);
var
  Args: TIL_MOVE_ARGS;
begin
  Args := TIL_MOVE_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_MOVEZERO(var Ctx: TILTContext);
var
  Args: TIL_MOVEZERO_ARGS;
begin
  Args := TIL_MOVEZERO_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_NEARCALL(var Ctx: TILTContext);
var
  Args: TIL_NEARCALL_ARGS;
begin
  Args := TIL_NEARCALL_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_NOT(var Ctx: TILTContext);
var
  Args: TIL_NOT_ARGS;
begin
  Args := TIL_NOT_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_NOW(var Ctx: TILTContext);
var
  Args: TIL_D_Args;
begin
  Args := TIL_D_Args.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_PCALL(var Ctx: TILTContext);
var
  Args: TIL_PCALL_ARGS;
  i, cnt: Integer;
  PArg: TILArgument;
begin
  Args := TIL_PCALL_ARGS.Create;
  LoadILArgumentInternal(Ctx, PArg, nil);
  if Assigned(PArg.Next) then
    Args.PArg := PArg.Next
  else
    Args.PArg := PArg;

  // читаем аргументы вызова
  cnt := Ctx.Stream.ReadStretchUInt;
  SetLength(Args.CallArgs, cnt);
  for i := 0 to cnt - 1 do
    LoadILArgument(Ctx, Args.CallArgs[i]);

  Ctx.Args := Args;
end;

procedure TILTranslator.Read_QUERYTYPE(var Ctx: TILTContext);
var
  Args: TIL_QUERYTYPE_ARGS;
begin
  Args := TIL_QUERYTYPE_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.L);
  LoadILArgument(Ctx, Args.R);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_RDREF(var Ctx: TILTContext);
var
  Args: TIL_RDREF_ARGS;
begin
  Args := TIL_RDREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_REFCNT(var Ctx: TILTContext);
var
  Args: TIL_DS_Args;
begin
  Args := TIL_DS_Args.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_RET(var Ctx: TILTContext);
begin
  Ctx.Args := nil;
end;

procedure TILTranslator.Read_SETBIT(var Ctx: TILTContext);
var
  Args: TIL_SETBIT_ARGS;
begin
  Args := TIL_SETBIT_ARGS.Create;
  LoadILArgument(Ctx, Args.Dst);
  LoadILArgument(Ctx, Args.BitIndex);
  LoadILArgument(Ctx, Args.BitValue);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_SETBOOL(var Ctx: TILTContext);
var
  Args: TIL_SETBOOL_ARGS;
begin
  Args := TIL_SETBOOL_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_STRONGREF(var Ctx: TILTContext);
var
  Args: TIL_STRONGREF_ARGS;
begin
  Args := TIL_STRONGREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_TEST(var Ctx: TILTContext);
var
  Args: TIL_TEST_ARGS;
begin
  Args := TIL_TEST_ARGS.Create;
  LoadILArgument(Ctx, Args.L);
  LoadILArgument(Ctx, Args.R);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_TRYBEGIN(var Ctx: TILTContext);
var
  A: TILArgument;
begin
  LoadILArgument(Ctx, A);
  Translate_TRYBEGIN(Ctx);
end;

procedure TILTranslator.Read_TRYEND(var Ctx: TILTContext);
var
  A: TILArgument;
begin
  LoadILArgument(Ctx, A);
  Translate_TRYEND(Ctx);
end;

procedure TILTranslator.Read_TYPEINFO(var Ctx: TILTContext);
var
  Args: TIL_TYPEINFO_ARGS;
begin
  Args := TIL_TYPEINFO_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_UNIQUE(var Ctx: TILTContext);
var
  Args: TIL_D_Args;
begin
  Args := TIL_D_Args.Create;
  LoadILArgument(Ctx, Args.D);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_WDREF(var Ctx: TILTContext);
var
  Args: TIL_WDREF_ARGS;
begin
  Args := TIL_WDREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.Read_WEAKREF(var Ctx: TILTContext);
var
  Args: TIL_WEAKREF_ARGS;
begin
  Args := TIL_WEAKREF_ARGS.Create;
  LoadILArgument(Ctx, Args.D);
  LoadILArgument(Ctx, Args.S);
  Ctx.Args := Args;
end;

procedure TILTranslator.RTTIProcToILProc(RTTIProc: PRTTIProcedure; Proc: TILProc);
var
  RTTIParams: PRTTIParams;
begin
  Proc.ProcInfo := GetOffset(FRTTIProcs.Memory, RTTIProc);

  SetLength(Proc.Params, RTTIProc.ParamsCount);
  if RTTIProc.ParamsCount > 0 then
  begin
    RTTIParams := PRTTIParams(GetTypeInfo(RTTIProc.Params));
    Move(RTTIParams[0], Proc.Params[0], SizeOf(TRttiParameter)*RTTIProc.ParamsCount);
  end;

  Proc.Flags := RTTIProc.Flags;
  Proc.Name := GetString(RTTIProc.Name);
  if RTTIProc.ResultType <> 0 then
    Proc.ProcType := ptFunction
  else
    Proc.ProcType := ptProcedure;

  Proc.VirtualIndex := RTTIProc.VirtualIndex;
  Proc.ImportLib := RTTIProc.ImportLib;
  Proc.ImportName := RTTIProc.ImportName;
  Proc.CallConvention := RTTIProc.CallConvention;

  {экспортируемая процедура}
  if Check(RTTIProc.Flags, ILPROC_EXPORT) then
  begin
    Proc.ExportIndex := Proc.ILUnit.ExportProcsCount;
    Inc(Proc.ILUnit.ExportProcsCount);
  end else begin
    Proc.ExportIndex := -1;
  end;
end;

procedure TILTranslator.SaveTargetCode(const Path: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Path, fmCreate);
  try
    SaveTargetCode(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TILTranslator.SetRTTICharset(const Value: TRTTICharset);
begin
  FRTTICharset := Value;
end;

procedure TILTranslator.SetSystemTypesOffsets(Types: PSystemTypes; Offsets: TIMGTypes);
var
  B: Pointer;
  off: UInt32;
begin
  B := Types;
  off := SizeOf(UInt32); // RTTI signature
  Offsets[Ord(dtInt8)].Offset := off + GetOffset(B, @Types._Int8);
  Offsets[Ord(dtInt8)].ID := dtInt8;

  Offsets[Ord(dtInt16)].Offset := off + GetOffset(B, @Types._Int16);
  Offsets[Ord(dtInt16)].ID := dtInt16;

  Offsets[Ord(dtInt32)].Offset := off + GetOffset(B, @Types._Int32);
  Offsets[Ord(dtInt32)].ID := dtInt32;

  Offsets[Ord(dtInt64)].Offset := off + GetOffset(B, @Types._Int64);
  Offsets[Ord(dtInt64)].ID := dtInt64;

  Offsets[Ord(dtUInt8)].Offset := off + GetOffset(B, @Types._UInt8);
  Offsets[Ord(dtUInt8)].ID := dtUInt8;

  Offsets[Ord(dtUInt16)].Offset := off + GetOffset(B, @Types._UInt16);
  Offsets[Ord(dtUInt16)].ID := dtUInt16;

  Offsets[Ord(dtUInt32)].Offset := off + GetOffset(B, @Types._UInt32);
  Offsets[Ord(dtUInt32)].ID := dtUInt32;

  Offsets[Ord(dtUInt64)].Offset := off + GetOffset(B, @Types._UInt64);
  Offsets[Ord(dtUInt64)].ID := dtUInt64;

  Offsets[Ord(dtNativeInt)].Offset := off + GetOffset(B, @Types._NativeInt);
  Offsets[Ord(dtNativeInt)].ID := dtNativeInt;

  Offsets[Ord(dtNativeUInt)].Offset := off + GetOffset(B, @Types._NativeUInt);
  Offsets[Ord(dtNativeUInt)].ID := dtNativeUInt;

  Offsets[Ord(dtFloat32)].Offset := off + GetOffset(B, @Types._Float32);
  Offsets[Ord(dtFloat32)].ID := dtFloat32;

  Offsets[Ord(dtFloat64)].Offset := off + GetOffset(B, @Types._Float64);
  Offsets[Ord(dtFloat64)].ID := dtFloat64;

  Offsets[Ord(dtBoolean)].Offset := off + GetOffset(B, @Types._Boolean);
  Offsets[Ord(dtBoolean)].ID := dtBoolean;

  Offsets[Ord(dtAnsiChar)].Offset := off + GetOffset(B, @Types._AnsiChar);
  Offsets[Ord(dtAnsiChar)].ID := dtAnsiChar;

  Offsets[Ord(dtChar)].Offset := off + GetOffset(B, @Types._Char);
  Offsets[Ord(dtChar)].ID := dtChar;

  Offsets[Ord(dtAnsiString)].Offset := off + GetOffset(B, @Types._AnsiString);
  Offsets[Ord(dtAnsiString)].ID := dtAnsiString;

  Offsets[Ord(dtString)].Offset := off + GetOffset(B, @Types._String);
  Offsets[Ord(dtString)].ID := dtString;

  Offsets[Ord(dtVariant)].Offset := off + GetOffset(B, @Types._Variant);
  Offsets[Ord(dtVariant)].ID := dtVariant;

  Offsets[Ord(dtGuid)].Offset := off + GetOffset(B, @Types._GUID);
  Offsets[Ord(dtGuid)].ID := dtGuid;

  Offsets[Ord(dtPointer)].Offset := off + GetOffset(B, @Types._Pointer);
  Offsets[Ord(dtPointer)].ID := dtPointer;
end;

procedure TILTranslator.WriteILUnitEpilog(Stream: TStream; ILUnit: TILUnit);
begin
  if (ILUnit.Flags and ILUNIT_HASFINAL) = 0 then
    Exit;

  ILUnit.FinalProc.ILUnit := ILUnit;
  ILUnit.FinalProc.Name := '$finalization';
  ILUnit.FinalProc.ProcType := ptProcedure;
  LoadILProcBody(Stream, ILUnit.FinalProc);
  ILUnit.VMUnit.FinalProc := ILUnit.FinalProc.Offset;
end;

procedure TILTranslator.WriteILUnitProlog(Stream: TStream; ILUnit: TILUnit);
begin
  if (ILUnit.Flags and ILUNIT_HASINIT) = 0 then
    Exit;

  ILUnit.InitProc.ILUnit := ILUnit;
  ILUnit.InitProc.Name := '$initialization';
  ILUnit.InitProc.ProcType := ptProcedure;
  LoadILProcBody(Stream, ILUnit.InitProc);
  ILUnit.VMUnit.InitProc := ILUnit.InitProc.Offset;
end;

procedure TILTranslator.WriteProcEpilog(Proc: TILProc; LastILCode: TILCode);
begin

end;

procedure TILTranslator.WriteProcProlog(Proc: TILProc);
begin

end;

procedure TILTranslator.WriteProcsRTTI(ILUnit: TILUnit);
begin

end;

procedure TILTranslator.WriteUnitsRTTIArray(UnitsCount: Integer);
begin

end;

procedure TILTranslator.WriteVMExportProcs(ILUnit: TILUnit);
begin

end;

function TILTranslator.PassByRef(const Param: PRTTIParameter): Boolean;
var
  ti: PRTTIType;
begin
  Result := Param.IsReference;
  if not Result and Param.IsConstant then
  begin
    ti := GetTypeInfo(Param.DataType);
    Result := ti.DataTypeID in [dtStaticArray, dtRecord, dtGuid, dtVariant];
  end;
end;

function TILTranslator.PrepareCALLInfo(const ProcArg: TILArgument; out ParamsCount: Integer; out Params: PRTTIParams;
                                       out ResultType: PRTTIType; out ADDR: NativeUInt): TCallType;
var
  TypeInfo: PRTTIType;
  Proc: TILProc;
  Struct: PRTTIType;
begin
  if ProcArg.ArgClass = ARG_VAR then
  begin
    {косвенный вызов через переменная процедурного типа}
    TypeInfo := GetTypeInfo(ProcArg.TypeInfo);
    Assert(TypeInfo.DataTypeID = dtProcType);
    Params := PRTTIParams(GetTypeInfo(PRTTIProcType(TypeInfo).Params));
    ParamsCount := PRTTIProcType(TypeInfo).ParamsCount;
    if PRTTIProcType(TypeInfo).ResultType > 0 then
      ResultType := GetTypeInfo(PRTTIProcType(TypeInfo).ResultType)
    else
      ResultType := nil;
    ADDR := NativeUInt(ProcArg.PTR);
    if TypeInfo.DataSize = PTR_SIZE*2 then
      Result := CallMethodIndirect
    else
      Result := CallIndirect;
  end else begin
    {прямой вызов процедуры}
    if ProcArg.ArgScope <> ARG_SCOPE_STRUCT then
    begin
      Proc := ProcArg.AsProcedure;
      if not Proc.IsImported then
        Result := CallStatic
      else
        Result := CallStaticExternal;
      ADDR := NativeUInt(ProcArg.PTR);
      ParamsCount := Length(Proc.Params);
      if ParamsCount > 0 then
        Params := PRTTIParams(@Proc.Params[0])
      else
        Params := nil;
      if Proc.ProcType = ptFunction then
        ResultType := GetTypeInfo(Params[0].DataType)
      else
        ResultType := nil;
    end else
   {прямой вызов метода}
    begin
      Proc := ProcArg.AsMethod.Proc;
      ADDR := NativeUInt(Proc);
      ParamsCount := Length(Proc.Params);
      if ParamsCount > 0 then
        Params := PRTTIParams(@Proc.Params[0])
      else
        Params := nil;

      if Proc.ProcType = ptFunction then
      begin
        ResultType := GetTypeInfo(Params[0].DataType);
        //Dec(ParamsCount);
      end else
        ResultType := nil;

      if ProcArg.AsMethod.Self.ArgClass = ARG_TYPE then
        Struct := GetTypeInfo(ProcArg.AsMethod.Self.TypeInfo)
      else
        Struct := GetTypeInfo(ProcArg.AsMethod.Self.AsVariable.RTTI.DataType);

      if Struct.ImportLib > 0 then begin
        if (ILPROC_HASSELFPTR and Proc.Flags) <> 0 then
          Result := CallMethodExternal
        else
          Result := CallStaticExternal;
      end else
      if (ILPROC_HASSELFPTR and Proc.Flags) <> 0 then
      begin
        if Struct.DataTypeID = dtInterface then
          Result := CallMethodInterface
        else
          Result := CallMethod;
      end else
        Result := CallStatic;
    end;
  end;
end;

{ TILUnit }

destructor TILUnit.Destroy;
var
  ti, pi: Integer;
begin
  for pi := 0 to Length(Procs) - 1 do
    Procs[pi].Free;

  for ti := 0 to Length(Types) - 1 do
  begin
    for pi := 0 to Length(Types[ti].Methods) - 1 do
      Types[ti].Methods[pi].Free;
  end;

  InitProc.Free;
  FinalProc.Free;
  inherited;
end;

{ TILBranchContext }

procedure TILBranchContext.Add(const Args: TIL_LR_Args);
var
  Item: ^TILBranch;
begin
  Item := FItems.Add();
  Item.CmpArgs := Args;
  Item.ElseIdx := -1;
  Item.EndIdx := -1;
end;

procedure TILBranchContext.BrClose;
begin
  FItems.Position := FItems.Position - 1;
end;

function TILBranchContext.GetCount: Integer;
begin
  Result := FItems.Position + 1;
end;

function TILBranchContext.GetLast: PILBranch;
begin
  if FItems.Position >= 0 then
    Result := addr(FItems.Items[FItems.Position])
  else begin
    AbortWork('Branch stack is empty');
    Result := nil;
  end;
end;

procedure TILBranchContext.Init;
begin
  FItems := TILBranchStack.Create(4);
end;

end.
