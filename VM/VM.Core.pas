unit VM.Core;

{$i compilers.inc}

interface

uses SysUtils, Classes, Generics.Defaults, Generics.Collections, {$IFDEF WINDOWS}Windows, {$ENDIF}StrUtils, Variants, TypInfo, Math,
     AnsiStrings, NPCompiler.Utils, IL.Types, IL.TypeInfo, NPCompiler.DataTypes, VM.Types, VM.Variant; // system, variants, rtti

{=================================================================}
{                  IL MACHINE BINARY IMAGE FORMAT                 }
{=================================================================}

(*

--------------------------------------
|          TYPES SECTION              |
--------------------------------------

--------------------------------------
|          CONSTS SECTION            |
--------------------------------------

--------------------------------------
|          UNITS SECTION            |
--------------------------------------
  UNITS COUNT: UINT32;
  ARRAY [UNITS COUNT] OF TMOFFSET; -- массив элементов TILMUnit


*)
{$I compilers.inc}


type

 // коды инструкций виртуальной машины
  TVMCode =
  (
    VM_NOPE,         // no opearation [args: none]
    VM_STACK,        // set stack [args: imm const - Int32 StackSize]
    //---------------------------------------------------
    LD_C_I32,        // load const [args: сonst: Int32]
    LD_C_U32,        // load const [args: сonst: UInt32]
    LD_C_I64,        // load const [args: сonst: Int64]
    LD_C_F32,        // load const [args: сonst: Float32]
    LD_C_F64,        // load const [args: сonst: Float64]
    LD_C_ZERO,       // load zero const [args: none]
    LD_C_ONE,        // load one const [args: none]
    //---------------------------------------------------
    LD_L_I8,         // load local [args: local offset]
    LD_L_U8,         // load local [args: local offset]
    LD_L_I16,        // load local [args: local offset]
    LD_L_U16,        // load local [args: local offset]
    LD_L_I32,        // load local [args: local offset]
    LD_L_I64,        // load local [args: local offset]
    LD_L_F32,        // load local [args: local offset]
    LD_L_PTR,        // load local [args: local offset]
    //---------------------------------------------------
    LD_R_I8,         // load local reference with deref [args: local offset]
    LD_R_U8,         // load local reference with deref [args: local offset]
    LD_R_I16,        // load local reference with deref [args: local offset]
    LD_R_U16,        // load local reference with deref [args: local offset]
    LD_R_I32,        // load local reference with deref [args: local offset]
    LD_R_I64,        // load local reference with deref [args: local offset]
    LD_R_F32,        // load local reference with deref [args: local offset]
    //---------------------------------------------------
    LD_G_I8,         // load global [аргументы: global offset]
    LD_G_U8,         // load global [аргументы: global offset]
    LD_G_I16,        // load global [аргументы: global offset]
    LD_G_U16,        // load global [аргументы: global offset]
    LD_G_I32,        // load global [аргументы: global offset]
    LD_G_I64,        // load global [аргументы: global offset]
    LD_G_F32,        // load global [аргументы: global offset]
    LD_G_PTR,        // load global [аргументы: global offset]
    LD_G_PROC,       // загрузка в Dst адреса процедуры указанной в памяти
    //---------------------------------------------------
    LD_D_I8,         // Dst.I8  = (Src1.PTR + offset)^
    LD_D_U8,         // Dst.U8  = (Src1.PTR + offset)^
    LD_D_I16,        // Dst.I16 = (Src1.PTR + offset)^
    LD_D_U16,        // Dst.U16 = (Src1.PTR + offset)^
    LD_D_I32,        // Dst.I16 = (Src1.PTR + offset)^
    LD_D_I64,        // Dst.I64 = (Src1.PTR + offset)^
    LD_D_F32,        // Dst.F64 = (Src1.PTR + offset)^
    //---------------------------------------------------
    MOVE_L_I32,      // copy I32 from local to local [args: Dst.offset, Src.offset]
    ST_C_I32,        // store I32 const to local [args: Dst.offset, imm const]
    //---------------------------------------------------
    ST_L_I8,         // save reg->local [аргументы: local offset]
    ST_L_I16,        // save reg->local [аргументы: local offset]
    ST_L_I32,        // save reg->local [аргументы: local offset]
    ST_L_I64,        // save reg->local [аргументы: local offset]
    ST_L_F32,        // save reg->local [аргументы: local offset]
    ST_L_VAR,        // save reg->local [аргументы: local offset]
    //---------------------------------------------------
    ST_R_I8,
    ST_R_I16,
    ST_R_I32,
    ST_R_I64,
    ST_R_F32,
    //---------------------------------------------------
    ST_G_I8,         // save reg->global [аргументы: global offset]
    ST_G_I16,        // save reg->global [аргументы: global offset]
    ST_G_I32,        // save reg->global [аргументы: global offset]
    ST_G_I64,        // save reg->global [аргументы: global offset]
    ST_G_F32,        // save reg->global [аргументы: global offset]
    ST_G_VAR,        // save reg->global [аргументы: global offset]
    //---------------------------------------------------
    ST_D_I8,         // (Dst.Ptr + [const offset])^ := Src1.I8
    ST_D_I16,        // (Dst.Ptr + [const offset])^ := Src1.I16
    ST_D_I32,        // (Dst.Ptr + [const offset])^ := Src1.I32
    ST_D_I64,        // (Dst.Ptr + [const offset])^ := Src1.I64
    ST_D_F32,        // (Dst.Ptr + [const offset])^ := Src1.F32
    //---------------------------------------------------
    CLR_L_I8,        // clear local [аргументы: local offset]
    CLR_L_I16,       // clear local [аргументы: local offset]
    CLR_L_I32,       // clear local [аргументы: local offset]
    CLR_L_I64,       // clear local [аргументы: local offset]
    CLR_L_F32,       // clear local [аргументы: local offset]
    CLR_L_F64,       // clear local [аргументы: local offset]
    //---------------------------------------------------
    CLR_R_I8,
    CLR_R_I16,
    CLR_R_I32,
    CLR_R_I64,
    CLR_R_F32,
    CLR_R_F64,
    //---------------------------------------------------
    CLR_G_I8,        // clear global [аргументы: global offset]
    CLR_G_I16,       // clear global [аргументы: global offset]
    CLR_G_I32,       // clear global [аргументы: global offset]
    CLR_G_I64,       // clear global [аргументы: global offset]
    CLR_G_F32,       // clear global [аргументы: global offset]
    CLR_G_F64,       // clear global [аргументы: global offset]
    //---------------------------------------------------
    CLR_D_I8,        // обнуление в памяти (Dst.PTR + [const offset])^.I8 := 0;
    CLR_D_I16,       // обнуление в памяти (Dst.PTR + [const offset])^.I16 := 0;
    CLR_D_I32,       // обнуление в памяти (Dst.PTR + [const offset])^.I32 := 0;
    CLR_D_I64,       // обнуление в памяти (Dst.PTR + [const offset])^.I64 := 0;
    CLR_D_F32,       // обнуление в памяти (Dst.PTR + [const offset])^.F32 := 0;
    CLR_D_F64,       // обнуление в памяти (Dst.PTR + [const offset])^.F64 := 0;
    //---------------------------------------------------
    MOVE_REG,        // Dst := Src
    MOVE_REG_TO_MEM, // копирование заданного кол-ва байт (не больше 8) из регистра в память
    //---------------------------------------------------
    CMP_I32_C,       // R0.I32 - CONST
    CMP_L_C32,       // сравнение в локальной памяти с константой  [args: Left.offset, Const: Int32]
    CMP_I32,         // сравнение INT32 в регистрах R0 и R1
    CMP_L_I32,       // сравнение в локальной памяти [args: Left.offset, Right.offset]
    CMP_I64,         // сравнение INT64 в регистрах R0 и R1
    CMP_F64,         // сравнение FLT64 в регистрах R0 и R1
    CMP_ASTR,        // сравнение ansi-строк
    CMP_USTR,        // сравнение unicode-строк
    CMP_VAR,         // сравнение variant-значений
    CMP_TEST32,      // логический AND
    CMP_TEST64,      // логический AND
    CMP_MEM_VS_REG,  // сравнение любого блока побайтово, cо значением в  регистре (макс 8 байт) размер задан в команде
    CMP_MEM,         // сравнение любого блока побайтово, размер задан в команде
    //---------------------------------------------------
    INC_L_I32,       // LocalVar.I32 := LocalVar.I32 + 1
    ADD_I32_C,       // Dst.I32 := Src.I32 + CONST
    ADD_L_I32_C,     // сложение в локальной памяти [аргументы: Dst.offset, Const: Int32]
    ADD_NUINT_C,     // Dst.NUInt := Src.NUInt + CONST Int32
    ADD_U32,
    ADD_U64,
    ADD_I32,
    ADD_L_I32,
    ADD_I64,
    ADD_F64,
    ADD_F64_CI32,    // Dst.F64 := Src.F64 + CONST Int32
    ADD_ASTR,
    ADD_USTR,
    //---------------------------------------------------
    SUB_I32_C,       // Dst.I32 := Src.I32 - CONST Int32
    SUB_F64_CI32,    // Dst.F64 := Src.F64 - CONST Int32
    SUB_I32,         // Dst.I32 := Src1.I32 - Src2.I32
    SUB_I64,         // Dst.I64 := Src1.I64 - Src2.I64
    SUB_F64,         // Dst.F64 := Src1.F64 - Src2.F64
    //---------------------------------------------------
    MUL_C32,
    MUL_I32,
    MUL_I64,
    MUL_F64,
    //---------------------------------------------------
    FMA_U32,         // Dst.U32 := Src1.U32 + Src2.U32 * Data1
    FMA_U64,         // Dst.U64 := Src1.U64 + Src2.U64 * Data1
    //---------------------------------------------------
    IDIV_U32_C,
    IDIV_I32,
    IDIV_I64,
    //---------------------------------------------------
    IMOD_U32_C,
    IMOD_I32,
    IMOD_I64,
    //---------------------------------------------------
    DIV_I32,
    DIV_I64,
    DIV_F64,
    //---------------------------------------------------
    NEG_I32,
    NEG_I64,
    NEG_F64,
    //---------------------------------------------------
    BIN_SHL32,
    BIN_SHL64,
    BIN_SHR32,
    BIN_SHR64,
    //---------------------------------------------------
    BIN_AND32,
    BIN_AND64,
    BIN_AND32_C,
    BIN_AND64_C,
    //---------------------------------------------------
    BIN_OR32,
    BIN_OR64,
    BIN_OR32_C,
    BIN_OR64_C,
    //---------------------------------------------------
    BIN_XOR32,
    BIN_XOR64,
    //---------------------------------------------------
    BIN_NOT32,
    BIN_NOT64,
    //---------------------------------------------------
    STRU_CREATE,                 // выделяет память под unicode-строку
    STRA_CREATE,                 // выделяет память под ansi-строку
    UNIQUE_USTR,
    UNIQUE_ASTR,

    STR_DECREF,                  // декремент счетчика строки
    {$IFDEF CPUX64}STR_INCREF,{$ENDIF}
    {$IFDEF CPUX64}STR_LENGTH,{$ENDIF}
    {$IFDEF CPUX64}STR_MOVE,{$ENDIF}

    ARRAY_LENGTH,                // вычислят длинну дин. массива/строки
    ARRAY_INCREF,                // инкремент счетчика дин. массива/строки
    ARRAY_DECREF,                // декремент счетчика дин. массива
    ARRAY_MEM_ALLOC,             // выделяет память под дин. массив (Dst - массив, Src1 - кол-во элементов, Data1 - размер элемента)
    ARRAY_INIT,                  // инициализирует внутреннюю структуру дин. массива

    VIRT_METHOD_PTR,             // инструкция вычисляет адрес виртуального метода  R0 - self, R1 - VMT offset
    VM_SET_METHOD_PTR,           // инструкция записывает адрес метода. [Dst - адрес TMethod, Src1 - Self, Src2 - Адерс процедуры]

    MEM_ALLOC,                   // выделение памяти заданного размера
    MEM_FREE,                    // освобождение памяти
    MEM_SET,                     // заполнение памяти

    OBJ_CREATE,                  // создание экземпляра класса (экземляр в R0)
    OBJ_INCREF,                  // инкремент счетчика (экземляр в R0)
    OBJ_DECREF,                  // декремент счетчика (экземляр в R0), если счетчик достигает 0 то вызывает деструктор

    OBJ_WEAKREF,                 // получение слабой ссылки на обьект
    OBJ_STRONGREF,               // получение сильной ссылки на обьект

    WEAK_INCREF,                 // инкремент счетчика слабой ссылки
    WEAK_DECREF,                 // декремент счетчика слабой ссылки

    DINF_DECREF,                 // Delphi-интрефейс
    DINF_INCREF,                 // Delphi-интрефейс

    ETHROW,                      // выбрас исключения
    SCHECKB,                     // проверка диаппазона статического массива (индекс в R0, диаппазон массива в коде инструкции)
    DCHECKB,                     // проверка диаппазона динамического массива (индекс в R0, размер массива в R1)
    ACHECKB,                     // проверка диаппазона динамического массива (массив в R0, индекса R1)

    MOVE_MEM,                    // копирование памяти (Dst, Src1, Src2 - кол-во байт)
    MOVE_MEM_C,                  // копирование памяти, константный объем (Dst, Src1, Data1 - кол-во байт)
    MOVE_ARRAY,                  // копирование массива Copy(Dst, Src1)
    //---------------------------------------------------
    CNV_F64_S32,
    CNV_F64_U32,
    CNV_F64_S64,
    CNV_F64_U64,
    CNV_UCHR_TO_USTR,            // конвертирует unicode-символ в unicode-строку
    CNV_ACHR_TO_ASTR,            // конвертирует ansi-символ в ansi-строку

    CNV_ASTR_TO_USTR,            // конвертирует ansi-строку в unicode-строку
    CNV_USTR_TO_ASTR,            // конвертирует unicode-строку в ansi-строку
    //-----------
    CNV_VAR_TO_VALUE,            // конвертирует variant в заданный тип (Dst = Src1, CONST Int32 - dst data type)
    CNV_VALUE_TO_VAR,            // конвертирует заданный тип в variant (Dst = Src1, CONST Int32 - src data type)
    VAR_RELEASE,                 // финализация варианта
    //---------------------------------------------------

    CALL_PROC,                   // вызов обычной процедуры/метода (параметры: размер стека текущей процедуры, адрес вызываемой процедуры)
    CALL_NEAR,
    CALL_VIRT,                   // вызов виртуального метода (параметры: размер стека текущей процедуры, адрес VMT, индекс метода)
    CALL_INTF,                   // вызов интерфейсного метода (Data1 - ID интервейса, Data2 - ID метода, Data3 - размер стека текущей процедуры)
    CALL_INDIRECT,               // косвенный вызов процедуры, (адрес которой загружен в R0)

    CALL_EXT_FAST,               // оптимизированная версия для наиболее распространенных вызовов
    CALL_EXT_FASTV,              // оптимизированная версия для наиболее распространенных вызовов (виртуальных)
    CALL_EXT_FAST_INTF_PROC,     // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
    CALL_EXT_FAST_INTF_FUNC,     // оптимизированная версия для наиболее распространенных вызовов (интерфейсных)
    CALL_EXT_COMMON_PROC,        // общая версия для всех возможны натаций и наборов параметров
    CALL_EXT_COMMON_FUNC,        // общая версия для всех возможны натаций и наборов параметров (с результатом)
    //---------------------------------------------------
    VM_SYSMACRO,                 // различные системные вызовы (макроинструкции)
    VM_JMP,
    PROC_RET
    //---------------------------------------------------
  );

  TILMemoryStream = class(TMemoryStream)
  strict private
    function GetMemoryPosition: Pointer; inline;
  public
    procedure IncPosition(Value: Integer); inline;
    property MemoryPosition: Pointer read GetMemoryPosition;
  end;

  // типы
  _TVMTypesSArray = array [0..65535] of TRTTYType;
  PVMTypes = ^_TVMTypesSArray;

  // переменные
  TVMVariable = packed object(TRTTI)
    DataType: PRTTIType;
    Addr: Pointer;
    Reference: Boolean;
    IsConstant: Boolean;
  end;
  PVMVariable = ^TVMVariable;

  _TVMVarsSArray = array [0..65535] of TVMVariable;
  PVMVariables = ^_TVMVarsSArray;

  TVMProcVars = array of TVMVariable;

  {структура-описатель параметра процедуры}
  TVMParam = packed record
    Name: TOffset;
    DataType: PRTTIType;
    Offset: TOffset;
    Reference: Boolean;
  end;
  PVMParam = ^TVMParam;

  _TVMParamsSArray = array [0..65535] of TVMParam;
  PVMParams = ^_TVMParamsSArray;

  TVMParamsArray = array of TVMParam;


  {описатель экспортируемой процедуры}
  TVMExportProc = packed record
    Name: TOffset;         // название процедуры
    Offset: TOffset;       // адрес
    Struct: TOffset;       // ссылка на структру (если это метод)
    ProcType: TProcType;   // тип (процедура/функция)
    Params: PVMParams;     // ссылка на список параметров
  end;
  PVMExportProc = ^TVMExportProc;

  _TVMExportProcsSArray = array [0..65535] of TVMExportProc;
  PVMExportProcs = ^_TVMExportProcsSArray;

  TVMExportProcsArray = array of TVMExportProc;
  {структура описывающая RTTI процедуры и размер ее стека}
  TVMIMGProcFrame = packed record
    ProcAddr: Pointer;  // адрес размещения процедуры в образе
    ProcInfo: Pointer;  // RTTI процедуры
  end;
  {список всех процедур сборки в отсортированном виде}
  TVMIMGProcFrames = array [0..65535] of TVMIMGProcFrame;
  PVMIMGProcFrames = ^TVMIMGProcFrames;

  {кадр стека вызова}
  TCallStackFrame = packed record
    CallAddr: Pointer;
    ProcInfo: Pointer;
  end;
  {стек вызовов}
  TCallStack = TArray<TCallStackFrame>;

  {модуль}
  TVMUnit = packed object(TRTTI)
  private
    FTypes: TOffset;               // список типов (RTTI)
    FVarsCount: Int32;             // кол-во глобальных переменных
    FVars: PVMVariables;           // список RTTI глобальных переменных
    FProcs: Pointer;               // список RTTI процедур
    FExportProcs: PVMExportProcs;  // список экспортируемых процедур модуля (RTTI)
    FInitProc: TOffset;            // процедура инициализации (Address)
    FFinalProc: TOffset;           // процедура финализации (Address)
    function GetTypesCount: Integer; inline;
    function GetProcsCount: Int32;
    function GetExportProcsCount: Int32;
  public
    property TypesCount: Integer read GetTypesCount;
    property Types: TOffset read FTypes;
    property VarsCount: Int32 read FVarsCount;
    property Variables: PVMVariables read FVars;
    property ProcsCount: Int32 read GetProcsCount;
    property Procs: Pointer read FProcs;
    property ExportProcsCount: Int32 read GetExportProcsCount;
    property ExportProcs: PVMExportProcs read FExportProcs;
    property InitProc: TOffset read FInitProc;
    property FinalProc: TOffset read FFinalProc;
  end;
  PVMUnit = ^TVMUnit;

  TVMUnits = array [0..C1G div SizeOf(TVMUnit)] of TVMUnit;
  PVMUnits = ^TVMUnits;

  TVMReg = record case Integer of
    0: (I64: Int64);
    1: (U64: UInt64);
    2: (I32: Int32);
    3: (U32: UInt32);
    4: (I16: Int16);
    5: (U16: UInt16);
    6: (I8: Int8);
    7: (U8: UInt8);
    8: (F64: Float64);
    9: (PTR: Pointer);
   10: (NUInt: NativeUInt);
   11: (NInt: NativeInt);
   12: (PVrnt: PVMVariant);
   13: (Bool: Boolean);
   14: (UChar: Char);
   15: (AChar: AnsiChar);
  end;
  PVMReg = ^TVMReg;

  TVMRegisters = array[0..15] of TVMReg;
  PVMRegisters = ^TVMRegisters;

  TVMDebugContext = record
    //IMGPtr: Pointer;           // начало образа в памяти
    Stack: PByte;
    Terminate: Boolean;        // признак что режим отладки закончен
    IMGOffset: NativeUInt;     // текущее смещение в образе
    Registers: PVMRegisters;   // указатель на регистры VM
  end;

  TVMBreakEvent = procedure(var Context: TVMDebugContext) of object;
  TVMCallEvent = procedure(Stack: PByte; ProcOffset: TOffset) of object;
  TVMRetEvent = procedure(Stack: PByte) of object;

  TILMachine = class
  private
  type
    TPtrSArray = array [0..65535] of Pointer;
    PPtrSArray = ^TPtrSArray;
  var
    FMem: TILMemoryStream;
    FHeader: PIMGHeader;
    FUnitsCount: Uint32;
    FUnits: PVMUnits;
    FRttiUnits: PPtrSArray;
    FStack: PByte;
    FStackSize: UInt32;
    FFreeStackWhenDestroy: Boolean;
    FOnBreak: TVMBreakEvent;
    FOnVMCall: TVMCallEvent;
    FOnVMRet: TVMRetEvent;
    //FDebugEnabled: Boolean;
    function GetIMGPtr(const Offset: Integer): Pointer; overload; inline;
    function GetIMGPtr(const Offset: Pointer): Pointer; overload; inline;
    function GetRTTIPtr(const Offset: TOffset): Pointer; overload; inline;
    function GetRTTIPtr(const Offset: Pointer): Pointer; overload; inline;
    function ReadSimple(var P: Pointer; TypeInfo: PRTTIType; Align: Integer = 1): string;
    function ReadPointer(var P: Pointer; TypeInfo: PRTTIPointer): string;
    function ReadClass(var P: Pointer; TypeInfo: PRTTIClass): string;
    function ReadIntf(var P: Pointer; TypeInfo: PRTTIInterface): string;
    function ReadProcType(var P: Pointer; TypeInfo: PRTTIProcType): string;
    function ReadArray(var P: Pointer; DimIndex: UInt32; TypeInfo: PRTTIArray): string;
    function ReadDynArray(var P: Pointer; DimIndex: UInt32; TypeInfo: PRTTIArray): string;
    function ReadRecord(var P: Pointer; TypeInfo: PRTTIRecord): string;
    function ReadEnum(var P: Pointer; TypeInfo: PRTTIOrdinal): string;
    function ReadSet(var P: Pointer; TypeInfo: PRTTISet): string;
    function ReadVarinat(var P: Pointer; TypeInfo: PRTTIVariant): string;
    function GetString(const Ptr: Pointer): string;
    function GetRTTICharset: TRTTICharset;
    function FindTypeProc(Struct: PRTTIStruct; const MethodName: string): PVMExportProc; // todo: isnt work now
    procedure MapImportProcedures;
    //////////////////////////////////////////////////////////////////
    procedure CallVMProc(MP {указатель на код процедры}: PNativeUInt;
                         SP {Указатель на стек процедуры}: PByte;
                         GP {указатель на память глобальных переменных}: PByte);

    procedure DebugCallILProc(MP {указатель на код процедры}: PNativeUInt;
                              SP {Указатель на стек процедуры}: PByte;
                              GP {указатель на память глобальных переменных}: PByte);
    //////////////////////////////////////////////////////////////////
    function _VM_OBJ_CREATE(const M: PNativeUInt): Pointer;
    function _GetCallStack(CurProc: Pointer; SP: PByte): TCallStack;
    procedure _VM_ETHROW(const IMG: TILMemoryStream; const EPtr: Pointer);
    procedure _VM_SYSMACRO(const MacroID: TVMMacroID; const Dst, Src1, Src2: PVMReg; SP: PByte);
    procedure _VM_OBJ_DECREF(PTR: Pointer; StackSize: Integer; GP: PByte; SP: PByte; PR: PNativeUInt);
    procedure _VM_ARRAY_DECREF_FINAL(Dst: PVMReg; StackSize: Integer; GP, SP: PByte; PR: PNativeUInt);
    //////////////////////////////////////////////////////////////////
  protected
    procedure DebugRunInitSections(Stack: PByte);
    procedure DebugRunFinalSections(Stack: PByte);
  public
    constructor Create(Stack: PByte; StackSize: UInt32; FreeStackWhenDestroy: Boolean = False); reintroduce; overload;
    constructor Create(StackSize: UInt32 = 65536); reintroduce; overload;
    destructor Destroy; override;
    ////////////////////////////////////////
    procedure LoadVMImage(Stream: TStream);
    procedure PrepareExportProcs(UN: PVMUnit);
    procedure PrepareProcs(UN: PVMUnit);
    procedure PrepareProcLocalVars(Proc: PRTTIProcedure);
    procedure PrepareFixTable(BasePtr: PByte);
    procedure RunInitSections(Stack: PByte = nil);
    procedure RunFinalSections(Stack: PByte = nil);

    procedure Run(); overload;
    procedure DebugRun(); overload;
    procedure Run(StackSize: Integer); overload;
    procedure RunProc(const ProcName: string; const Params: array of Variant; StackSize: Integer = 1024*1024); overload;
    procedure RunFunc(const ProcName: string; const Params: array of Variant; out Result: Variant; StackSize: Integer = 1024*1024); overload;
    procedure RunProc(Proc: PVMExportProc); overload;
    procedure RunProc(Proc: PVMExportProc; Param0: Int32); overload;
    procedure RunFunc(Proc: PVMExportProc; Param0: NativeUInt; ResultDataType: TDataTypeID; Result: pointer); overload;
    procedure RunFunc(Proc: PVMExportProc; Param0, Param1: NativeUInt; ResultDataType: TDataTypeID; Result: pointer); overload;

    procedure WriteParamsToStack(Stack: PByte; const Params: array of Variant);
    procedure ReadResultFromStack(Stack: PByte; ResultDataType: TDataTypeID; Result: Pointer);
    property UnitsCount: UInt32 read FUnitsCount;
    property Units: PVMUnits read FUnits;
    property RTTICharset: TRTTICharset read GetRTTICharset;
    property OnBreakEvent: TVMBreakEvent read FOnBreak write FOnBreak;
    property OnVMCallEvent: TVMCallEvent read FOnVMCall write FOnVMCall;
    property OnVMRetEvent: TVMRetEvent read FOnVMRet write FOnVMRet;
    function ReadVarValue(const Variable: PVMVariable): string;
    function ReadValue(var P: Pointer; TypeInfo: PRTTIType; Align: Integer = 1): string;
    function FindType(const TypeName: string; const UnitName: string = ''): PRTTIType;
    function FindProc(const ProcName: string; const UnitName: string = ''): PVMExportProc;
    function FindVar(const VarName: string; const UnitName: string = ''): PVMVariable;
    function FindMethod(const TypeName, MethodName: string; const UnitName: string = ''): PVMExportProc;
    function GetUnitName(const pUnit: PVMUnit): string; inline;
    function GetProcName(const pProc: PVMExportProc): string; inline;
    function GetTypeName(const pType: PRTTIType): string; inline;
    function GetProcOffset(const pProc: PVMExportProc): NativeUInt; overload; inline;
    function GetProcOffset(const Proc: PRTTIProcedure): NativeUInt; overload; inline;
    function GetVarName(const pVar: PVMVariable): string; inline;
    function GetParamName(const Param: PVMParam): string; inline;
    function GetLocalVarName(const LVar: PRTTILocalVar): string; inline;
    function GetImportProcLib(ImportProc: PImportEntry): string; inline;
    function GetImportProcName(ImportProc: PImportEntry): string; inline;
    function GetInitProcOffset(pUnit: PVMUnit): NativeUInt; inline;
    function GetFinalProcOffset(pUnit: PVMUnit): NativeUInt; inline;

    procedure SetVarAsPointer(const Variable: PVMVariable; const Value: Pointer); inline;
    procedure SetVarAsFloat32(const Variable: PVMVariable; const Value: Float32); inline;
    procedure SetVarAsFloat64(const Variable: PVMVariable; const Value: Float64); inline;
    procedure SetVarAsInt64(const Variable: PVMVariable; const Value: Int64); inline;
    procedure SetVarAsInt32(const Variable: PVMVariable; const Value: Int32); inline;
    procedure SetVarAsInt16(const Variable: PVMVariable; const Value: Int16); inline;
    procedure SetVarAsInt8(const Variable: PVMVariable; const Value: Int8); inline;
    procedure SetVarAsBool(const Variable: PVMVariable; const Value: boolean); inline;
  end;

implementation

uses VM.Invoke, VM.Core.Managed;

{ TILMemoryStream }

function TILMemoryStream.GetMemoryPosition: Pointer;
begin
  Result := PByte(Memory) + Position;
end;

procedure TILMemoryStream.IncPosition(Value: Integer);
begin
  Position := Position + Value;
end;

// platform Windows
function AnsiCompareStrA(const S1, S2: AnsiString): Integer;
begin
  {$IFDEF CPUARM}
  Result := CompareStr(S1, S2);
  {$ELSE}
  Result := AnsiStrings.CompareStr(S1, S2);
  {$ENDIF}
end;

{ TILMachine }

type
  TVMFlags = set of TILCondition;

procedure SetCondition(Result: NativeInt; var F: TVMFlags); overload;
begin
  if Result = 0 then
    F := [cEqual, cGreaterOrEqual, cLessOrEqual, cZero]
  else begin
    if Result > 0 then
      F := [cGreater, cGreaterOrEqual, cNotEqual, cNonZero]
    else
      F := [cLess, cLessOrEqual, cNotEqual, cNonZero];
  end;
end;

{$IFNDEF CPUX64}
procedure SetCondition(Result: Int64; var F: TVMFlags); overload;
begin
  if Result = 0 then
    F := [cEqual, cGreaterOrEqual, cLessOrEqual, cZero]
  else begin
    if Result > 0 then
      F := [cGreater, cGreaterOrEqual, cNotEqual, cNonZero]
    else
      F := [cLess, cLessOrEqual, cNotEqual, cNonZero];
  end;
end;
{$ENDIF}

procedure SetCondition(Result: Double; var F: TVMFlags); overload;
begin
  if Result = 0 then
    F := [cEqual, cGreaterOrEqual, cLessOrEqual]
  else begin
    if Result > 0 then
      F := [cGreater, cGreaterOrEqual, cNotEqual, cNonZero]
    else
      F := [cLess, cLessOrEqual, cNotEqual, cNonZero];
  end;
end;

procedure SetCondition(const Left, Right: Variant; var F: TVMFlags); overload;
begin
  if Left < Right then
    F := [cLess, cLessOrEqual, cNotEqual]
  else
  if Left > Right then
    F := [cGreater, cGreaterOrEqual, cNotEqual]
  else
  if Left >= Right then
    F := [cGreater, cGreaterOrEqual, cEqual]
  else
  if Left <= Right then
    F := [cLess, cLessOrEqual, cEqual]
  else
  if Left = Right then
    F := [cEqual]
  else
    F := [cNotEqual];
end;

procedure _CNV_TO_VARIANT(Dst, Src: PVMReg; DataTypeID: TDataTypeID);
var
  VDst: PVMVariant;
begin
  VDst := Dst.PVrnt;
  case DataTypeID of
    dtInt8: VDst.FromInt8(Src.I8);
    dtInt16: VDst.FromInt16(Src.I16);
    dtInt32: VDst.FromInt32(Src.I32);
    dtInt64: VDst.FromInt64(Src.I64);
    dtUInt8: VDst.FromUInt8(Src.U8);
    dtUInt16: VDst.FromUInt16(Src.U16);
    dtUInt32: VDst.FromUInt32(Src.U32);
    dtUInt64: VDst.FromUInt64(Src.U64);
    dtNativeInt: VDst.FromNativeInt(Src.NInt);
    dtNativeUInt: VDst.FromNativeUInt(Src.NUInt);
    dtBoolean: VDst.FromBoolean(Src.Bool);
    dtChar: VDst.FromChar(Src.UChar);
    dtAnsiChar: VDst.FromAnsiChar(Src.AChar);
    dtFloat32: VDst.FromFloat32(Src.F64);
    dtFloat64: VDst.FromFloat64(Src.F64);
    dtString: VDst.FromString(string(Src.PTR));
    dtAnsiString: VDst.FromAnsiString(AnsiString(Src.PTR));
  end;
end;

procedure _CNV_FROM_VARIANT(Dst, Src: PVMReg; DataTypeID: TDataTypeID);
var
  VSrc: PVMVariant;
begin
  VSrc := Src.PVrnt;
  case DataTypeID of
    dtInt8: VSrc.ToInt8(Dst.I8);
    dtInt16: VSrc.ToInt16(Dst.I16);
    dtInt32: VSrc.ToInt32(Dst.I32);
    dtInt64: VSrc.ToInt64(Dst.I64);
    dtUInt8: VSrc.ToUInt8(Dst.U8);
    dtUInt16: VSrc.ToUInt16(Dst.U16);
    dtUInt32: VSrc.ToUInt32(Dst.U32);
    dtUInt64: VSrc.ToUInt64(Dst.U64);
    dtNativeInt: VSrc.ToNativeInt(Dst.NInt);
    dtNativeUInt: VSrc.ToNativeUInt(Dst.NUInt);
    dtBoolean: VSrc.ToBoolean(Dst.Bool);
    dtChar: VSrc.ToChar(Dst.UChar);
    dtAnsiChar: VSrc.ToAnsiChar(Dst.AChar);
    dtFloat32: VSrc.ToFloat32(Dst.F64);
    dtFloat64: VSrc.ToFloat64(Dst.F64);
    dtString: VSrc.ToString(Dst.PTR);
    dtAnsiString: VSrc.ToAnsiString(Dst.PTR);
  end;
end;

procedure _VAR_RELEASE(Dst: PVMReg);
begin
  DSt.PVrnt.Clear;
end;

function IncPtr(P: PNativeUInt): PNativeUInt; inline;
begin
  Result := PNativeUInt(PByte(P) + SizeOf(P));
end;


type
  TVMWeakRef = record
    Reference: Pointer;    // указатель на обьект
    RefCount: NativeInt;   // счетчик ссылок
  end;
  PVMWeakRef = ^TVMWeakRef;

  TVMObject = record
    TypeInfo: PRTTIClass;  // указатель на RTTI класса
    RefCount: Integer;     // счетчик сильных ссылок  (integer - потому что InterlocedInc работает только с int32)
    WeakInfo: Pointer;     // указатель на мягкую ссылку
    SyncInfo: Pointer;     // указатель блок синхронизации
  end;
  PVMObject = ^TVMObject;


procedure _VM_OBJ_INCREF(PTR: Pointer);
begin

  if Assigned(PTR) then begin
    Ptr := PByte(Ptr) - SizeOf(TVMObjHeader);
    if PVMObject(PTR).RefCount > 0 then
      AtomicIncrement(PVMObject(PTR).RefCount);
  end;
end;

procedure _VM_ARRAY_INCREF(PTR: Pointer);
var
  pRefCnt: PInt32;
begin
  if Assigned(PTR) then begin
    pRefCnt := PInt32(NativeUInt(PTR) - ARR_REFCNT_OFFSET);
    if pRefCnt^ > -1 then
      AtomicIncrement(pRefCnt^);
  end;
end;

procedure _VM_FREE_MEM(Ptr: Pointer);
begin
  FreeMemory(Ptr);
end;

procedure _VM_ARRAY_DECREF(Dst: PVMReg);
var
  pRefCnt: PInt32;
  PTR: Pointer;
begin
  PTR := Dst.PTR;
  if Assigned(PTR) then
  begin
    pRefCnt := PInt32(PByte(PTR) - ARR_REFCNT_OFFSET);
    if pRefCnt^ > -1 then begin
      AtomicDecrement(pRefCnt^);
      if pRefCnt^ = 0 then
        _VM_FREE_MEM(PByte(PTR) - (4 + PTR_SIZE + STR_REC_PADDING)); // освобождаем память массива
    end;
  end;
end;

procedure TILMachine._VM_ARRAY_DECREF_FINAL(Dst: PVMReg; StackSize: Integer; GP, SP: PByte; PR: PNativeUInt);
var
  pRefCnt: PInt32;
  PTR, NewSP: PByte;
begin
  PTR := Dst.PTR;
  if Assigned(PTR) then
  begin
    pRefCnt := PInt32(PByte(PTR) - ARR_REFCNT_OFFSET);
    if pRefCnt^ > -1 then
    begin
      AtomicDecrement(pRefCnt^);
      if pRefCnt^ > 0 then
        Exit;
      // по смещению StackSize сохраняем указатель на свой стек
      PPtr(SP + StackSize)^ := SP;
      // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
      PPtr(SP + StackSize + PTR_SIZE)^ := nil; // возврат будет в паскаль код!!!
      // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
      NewSP := SP + StackSize + PTR_SIZE*2;
      PPtr(NewSP)^ := PTR;                            // передаем единственные параметр - массив
      CallVMProc(PNativeUInt(GP + PR^), NewSP, GP);   // вызываем финализатор
      _VM_FREE_MEM(PByte(PTR) - ARR_REFCNT_OFFSET); // освобождаем память массива
    end;
  end;
end;

procedure _VM_WEAK_DECREF(PTR: Pointer);
var
  pRefCnt: PInt32;
begin
  if Assigned(PTR) then
  begin
    pRefCnt := Addr(PVMWeakRef(PTR).RefCount);
    if pRefCnt^ > -1 then begin
      AtomicDecrement(pRefCnt^);
      if pRefCnt^ = 0 then
      begin
        // если обьект еще сущетвует, удаляем у него ссылку на weak
        if Assigned(PVMWeakRef(PTR).Reference) then
          PVMObject(PByte(PVMWeakRef(PTR).Reference) - SizeOf(TVMObjHeader)).WeakInfo := nil;
        FreeMemory(PTR);
      end;
    end;
  end;
end;

procedure _VM_WEAK_INCREF(PTR: Pointer);
var
  pRefCnt: PInt32;
begin
  if Assigned(PTR) then
  begin
    pRefCnt := Addr(PVMWeakRef(PTR).RefCount);
    if pRefCnt^ > -1 then
      AtomicIncrement(pRefCnt^);
  end;
end;

function TILMachine._VM_OBJ_CREATE(const M: PNativeUInt): Pointer;
var
  ClassInfo: PRTTIClass;
begin
  ClassInfo := PRTTIClass(GetRTTIPtr(M^));
  Result := GetMemory(ClassInfo.DataSize);
  FillChar(Result^, ClassInfo.DataSize, 0);
  PVMObject(Result).TypeInfo := ClassInfo;
  PVMObject(Result).RefCount := 1;
  Result := PByte(Result) + SizeOf(TVMObjHeader);
end;

procedure _VM_STRU_CONCAT(const Dst, SrcL, SrcR: PVMReg);
var
  L1, L2, Size: Integer;
  Ptr: PByte;
begin
  if Assigned(SrcL.PTR) then L1 := PInt32(PByte(SrcL.PTR) - 4)^ else L1 := 0;
  if Assigned(SrcR.PTR) then L2 := PInt32(PByte(SrcR.PTR) - 4)^ else L2 := 0;
  Size := STR_REC_SIZE + L1*2 + L2*2 + 2;

  Ptr := GetMemory(Size);
  PInt16(Ptr + STR_REC_PADDING + 0)^ := 1200;        // code page (utf-16)
  PInt16(Ptr + STR_REC_PADDING + 2)^ := 2;           // char size
  PInt32(Ptr + STR_REC_PADDING + 4)^ := 1;           // refcount = 1
  PInt32(Ptr + STR_REC_PADDING + 8)^ := L1 + L2;     // length
  PInt16(Ptr + Size - 2)^ := 0;    // null-term

  Ptr := Ptr + STR_REC_SIZE;
  Move(SrcL.PTR^, Ptr^, L1*2);
  Move(SrcR.PTR^, (Ptr + L1*2)^, L2*2);

  Dst.PTR := Ptr;
end;

procedure _VM_STRA_CONCAT(const Dst, SrcL, SrcR: PVMReg);
var
  L1, L2, Size: Integer;
  Ptr: PByte;
begin
  if Assigned(SrcL.PTR) then L1 := PInt32(PByte(SrcL.PTR) - 4)^ else L1 := 0;
  if Assigned(SrcR.PTR) then L2 := PInt32(PByte(SrcR.PTR) - 4)^ else L2 := 0;
  Size := STR_REC_SIZE + L1 + L2 + 1;

  Ptr := GetMemory(Size);
  PInt16(Ptr + STR_REC_PADDING + 0)^ := 1251;        // code page (ansi)
  PInt16(Ptr + STR_REC_PADDING + 2)^ := 1;           // char size
  PInt32(Ptr + STR_REC_PADDING + 4)^ := 1;           // refcount = 1
  PInt32(Ptr + STR_REC_PADDING + 8)^ := L1 + L2;     // length
  PInt8(Ptr + Size - 1)^ := 0;     // null-term
  Ptr := Ptr + STR_REC_SIZE;

  Move(SrcL.PTR^, Ptr^, L1);
  Move(SrcR.PTR^, (Ptr + L1)^, L2);

  Dst.PTR := Ptr;
end;

function _VM_STRU_COMPARE(const SrcL, SrcR: PVMReg): Integer;
begin
  Result := AnsiCompareStr(string(SrcL.PTR), string(SrcR.PTR));
end;

function _VM_STRA_COMPARE(const SrcL, SrcR: PVMReg): Integer;
begin
  Result := AnsiCompareStrA(AnsiString(SrcL.PTR), AnsiString(SrcR.PTR));
end;

procedure _VM_MEM_COMPARE(Left, Right: Pointer; Size: Integer; var Flags: TVMFlags);
begin
  if CompareMem(Left, Right, Size) then
    Flags := Flags + [cEqual, cGreaterOrEqual, cLessOrEqual, cZero]
  else
    Flags := Flags + [cNotEqual, cNonZero];
end;

procedure _VM_DYNARRAY_CREATE(const Dst, Src: PVMReg; ElSize: NativeUInt);
begin
  Dst.PTR := GetMemory(STR_REC_PADDING + 4{refcnt} + PTR_SIZE{length} + Src.U32*ElSize);
  PInt32(PByte(Dst.PTR) + STR_REC_PADDING)^ := 1;              // refcount = 1
  PNUInt(PByte(Dst.PTR) + 4 + STR_REC_PADDING)^ := Src.U32;    // length
  Dst.PTR := PByte(Dst.PTR) + 4 + PTR_SIZE + STR_REC_PADDING;  // adjust the pointer
  FillChar(Dst.PTR^, Src.U32*ElSize, #0);                      // clear memory
end;

function _VM_STRU_CREATE(const Len: UInt32): Pointer;
var
  Size: Integer;
  Ptr: PByte;
begin
  Size := STR_REC_SIZE + Len*2 + 2;
  Ptr := GetMemory(Size);
  PUInt16(Ptr + STR_REC_PADDING + 0)^ := 1200;        // code page (utf-16)
  PUInt16(Ptr + STR_REC_PADDING + 2)^ := 2;           // char size
  PUInt32(Ptr + STR_REC_PADDING + 4)^ := 1;           // refcount = 1
  PUInt32(Ptr + STR_REC_PADDING + 8)^ := Len;         // length
  PUInt16(Ptr + Size - 2)^ := 0;    // null-term
  Result := Ptr + STR_REC_SIZE;
end;

function _VM_STRA_CREATE(const Len: UInt32): Pointer;
var
  Size: Integer;
  Ptr: PByte;
begin
  Size := STR_REC_SIZE + Len + 1;
  Ptr := GetMemory(Size);
  PUInt16(Ptr + STR_REC_PADDING + 0)^ := 1251;        // code page (ansi)
  PUInt16(Ptr + STR_REC_PADDING + 2)^ := 1;           // char size
  PUInt32(Ptr + STR_REC_PADDING + 4)^ := 1;           // refcount = 1
  PUInt32(Ptr + STR_REC_PADDING + 8)^ := Len;         // length
  PUInt8(Ptr + Size - 1)^ := 0;     // null-term
  Result := Ptr + STR_REC_SIZE;
end;

function _STRU_FROM_STRA(const Src: PVMReg): Pointer;
var
  Len: UInt32;
  Bytes: TBytes;
begin
  if Assigned(Src.PTR) then
  begin
    Len := PNUInt(PByte(Src.PTR) - STR_LEN_OFFSET)^;
    Result := _VM_STRU_CREATE(Len);
    // converting
    Bytes := TEncoding.Convert(TEncoding.ANSI, TEncoding.Unicode, TBytes(Src.PTR));
    Move(Bytes[0], Result^, Len*SizeOF(WideChar));
  end else
    Result := nil;
end;

function _STRA_FROM_STRU(const Src: PVMReg): Pointer;
var
  Len: UInt32;
  UStr: UnicodeString;
  AStr: AnsiString;
begin
  if Assigned(Src.PTR) then
  begin
    Len := PNUInt(PByte(Src.PTR) - STR_LEN_OFFSET)^;
    Result := _VM_STRA_CREATE(Len);
    UStr := string(Src.PTR);
    AStr := AnsiString(UStr); // converting
    Move(AStr[Low(string)], Result^, Len*SizeOF(AnsiChar));
  end else
    Result := nil;
end;

procedure _VM_OBJ_WEAKREF(var Ptr: Pointer);
var
  WeakPtr: Pointer;
  RAWPtr: PVMObject;
begin
  if Assigned(Ptr) then
  begin
    RAWPtr := PVMObject(PByte(Ptr) - SizeOf(TVMObjHeader));
    WeakPtr := RAWPtr.WeakInfo;
    // если у обьекта RefCount = 0 то слабую ссылку на него уже не получить.
    if not Assigned(WeakPtr) and (RAWPtr.RefCount > 0) then
    begin
      WeakPtr := GetMemory(SizeOF(TVMWeakRef));
      PVMWeakRef(WeakPtr).RefCount := 1;
      PVMWeakRef(WeakPtr).Reference := Ptr;
      RAWPtr.WeakInfo := WeakPtr;
    end;
    Ptr := WeakPtr;
  end;
end;

procedure _VM_OBJ_STRONGREF(var Ptr: Pointer);
var
  Ref: PByte;
  RAWPtr: PVMObject;
begin
  if Assigned(Ptr) then
  begin
    Ref := PVMWeakRef(Ptr).Reference;
    if Assigned(Ref) then begin
      // если слабая ссылка - валидный обьект, то икриментируем счетчик и возвращаем
      RAWPtr := PVMObject(Ref - SizeOf(TVMObjHeader));
      AtomicIncrement(RAWPtr.RefCount);
      Ptr := Ref;
      Exit;
    end;
  end;
  Ptr := nil;
end;

function _VM_PROC_CALL_VIRTUAL(PTR: Pointer; var SP: PByte; const PR: PNativeUInt; const GP: PByte): Pointer;
const
  CMD_DATA_CNT = 2;
var
  StackSize: Integer;
  VMT: PVMT;
begin
  // вычитываем из инструкции вызова размер своего стека
  StackSize := PNInt(PByte(PR) + PTR_SIZE)^;
  // по смещению StackSize сохраняем указатель на свой стек
  PPtr(SP + StackSize)^ := SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  PPtr(SP + StackSize + PTR_SIZE)^ := PByte(PR) + PTR_SIZE*CMD_DATA_CNT;
  // формируем новый стек равный StackSize + PTR_SIZE*2 (SP + MP)
  SP := SP + StackSize + PTR_SIZE*2;
  // загружаем указатель на код вызываемой процедуры
  Ptr := PByte(Ptr) - SizeOf(TVMObjHeader); // смещение на TypeInfo
  VMT := PVMT(GP + PVMObject(PTR).TypeInfo.VMT);
  Result := PNativeUInt(GP + VMT[PR^]);
end;

function _VM_PROC_CALL_INTERFACE(Self: Pointer; var SP: PByte; const PR: PNativeUInt; const GP: PByte): Pointer;
const
  CMD_DATA_CNT = 3;
var
  StackSize, InterfaceID, MethodID: Integer;
  IMTS: PIMTS;
  IMT: PIMT;
begin
  // вычитываем из инструкции вызова размер своего стека
  StackSize := PNInt(PByte(PR) + PTR_SIZE*2)^;
  // по смещению StackSize сохраняем указатель на свой стек
  PPtr(SP + StackSize)^ := SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  PPtr(SP + StackSize + PTR_SIZE)^ := PByte(PR) + PTR_SIZE*CMD_DATA_CNT;
  // формируем новый стек равный StackSize + PTR_SIZE*2 (SP + MP)
  SP := SP + StackSize + PTR_SIZE*2;
  // загружаем указатель на код вызываемой процедуры
  Self := PByte(Self) - SizeOf(TVMObjHeader);       // смещение на TypeInfo
  IMTS := PIMTS(GP + PVMObject(Self).TypeInfo.IMTS); // получаем спикос таблиц IMT
  InterfaceID := PNInt(PR)^;
  IMT := PIMT(GP + IMTS[InterfaceID]);               // получаем IMT для данного интерфейса
  MethodID := PNInt(PByte(PR) + PTR_SIZE)^;
  Result := PNativeUInt(GP + IMT[MethodID]);
end;

procedure _VM_GET_METHOD_PTR(Dst, Src: PVMReg; const Params: PNativeUInt; const GP: PByte);
var
  Ptr: Pointer;
  VMT: PVMT;
begin
  Ptr := PByte(Src.PTR) - SizeOf(TVMObjHeader); // смещение на TypeInfo
  VMT := PVMT(GP + PVMObject(PTR).TypeInfo.VMT);
  Dst.PTR := PNativeUInt(GP + VMT[Params^]);
end;

procedure TILMachine._VM_OBJ_DECREF(PTR: Pointer; StackSize: Integer; GP: PByte; SP: PByte; PR: PNativeUInt);
var
  P: PInt32;
  VMT: PVMT;
  ObjPtr: PVMObject;
  WeakRef: PVMWeakRef;
  MethodPtr: PNativeUInt;
begin
  if PTR <> nil then
  begin
    ObjPtr := PVMObject(PByte(Ptr) - SizeOf(TVMObjHeader));
    P := addr(ObjPtr.RefCount);
    Assert(P^ <> 0);
    if P^ >= 1 then
      AtomicDecrement(P^);

    if P^ = 0 then
    begin
      // заниливаем ссылку на себя в слабой ссылке (если есть)
      WeakRef := ObjPtr.WeakInfo;
      if Assigned(WeakRef) then
        WeakRef.Reference := nil;
      // по смещению StackSize сохраняем указатель на свой стек
      PPtr(SP + StackSize)^ := SP;
      // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
      PPtr(SP + StackSize + PTR_SIZE)^ := nil; // возврат будет в паскаль код!!! иначе PByte(PR) + PTR_SIZE;
      // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
      SP := SP + StackSize + PTR_SIZE*2;
      // получаем указатель на VMT
      VMT := PVMT(GP + ObjPtr.TypeInfo.VMT);
      PPointer(SP)^ := PTR; // передаем self
      MethodPtr := PNativeUInt(GP + VMT[IL_VMT_DESTROY]);
      CallVMProc(MethodPtr, SP, GP);       // вызываем деструктор
      if PR^ > 0 then begin
        MethodPtr := PNativeUInt(GP + PR^);
        CallVMProc(MethodPtr, SP, GP);     // вызываем финализатор
      end;
      _VM_FREE_MEM(ObjPtr); // освобождаем память обьекта
    end;
  end;
end;

procedure _VM_PROC_CALL(var SP: PByte; var M: PNativeUInt; PS: PByte);
var
  StackSize: Integer;
begin
  // вычитываем из инструкции вызова размер стека вызывающей процедуры
  StackSize := PNInt(PByte(M) + PTR_SIZE)^;
  // по смещению StackSize сохраняем указатель на свой стек
  PPtr(SP + StackSize)^ := SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  PPtr(SP + StackSize + PTR_SIZE)^ := PByte(M) + PTR_SIZE*2;
  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
  SP := SP + StackSize + PTR_SIZE*2;
  // загружаем указатель на код вызываемой процедуры
  M := PNativeUInt(PS + M^);
end;

procedure TILMachine._VM_ETHROW(const IMG: TILMemoryStream; const EPtr: Pointer);
var
  Str: string;
begin
  // т.к смещение поля Message равно нулю
  Str := PString(EPtr)^;
  Str := Copy(Str, 1, Length(Str));
  // пока что (временно) освобождаем память
  _VM_FREE_MEM(PByte(EPtr) - SizeOf(TVMObject));
  raise EVMException.Create(Str);
end;

procedure _VM_SCHECKB(const R0: TVMReg; const PR: PNativeUInt);
begin
  if (R0.NInt < NativeInt(PR^)) or (R0.NInt > PNativeInt(PByte(PR) + PTR_SIZE)^) then
    raise EVMException.Create('Range check error');
end;

procedure _VM_DCHECKB(const R0, R1: PVMReg);
begin
  if (R0.NInt < 0) or (R0.NUInt >= R1.NUInt) then
    raise EVMException.Create('Range check error');
end;

procedure _VM_ACHECKB(const Arr, Idx: PVMReg);
begin
  if not Assigned(Arr.PTR) or (Idx.NInt < 0) or (Idx.NUInt >= PNativeUInt(PByte(Arr.PTR) - 4)^) then
    raise EVMException.Create('Range check error');
end;

procedure TILMachine._VM_SYSMACRO(const MacroID: TVMMacroID; const Dst, Src1, Src2: PVMReg; SP: PByte);
var
  DstClass, SrcClass: PRTTIClass;
  TI: ^TRTTI;
begin
  case MacroID of
    vmsmCheckClass: begin
      DstClass := GetRTTIPtr(Src1.NUInt);
      SrcClass := PVMObject(PByte(Dst.PTR) - SizeOf(TVMObjHeader)).TypeInfo;
      Dst.I64 := Integer(ClassInheritsFrom(SrcClass, DstClass));
    end;
    vmsmQueryClass: begin
      DstClass := GetRTTIPtr(Src1.NUInt);
      if DstClass = PRTTIClass(PByte(Dst.PTR) - SizeOf(TVMObjHeader)) then
        Exit;
      SrcClass := PVMObject(PByte(Dst.PTR) - SizeOf(TVMObjHeader)).TypeInfo;
      if not ClassInheritsFrom(SrcClass, DstClass) then
        raise EVMException.Create('Cannot cast object as ' + GetTypeName(DstClass));
    end;
    vmsmCheckIntf: begin
      SrcClass := PVMObject(PByte(Dst.PTR) - SizeOf(TVMObjHeader)).TypeInfo;
      Dst.I64 := Integer(PIMTS(GetIMGPtr(SrcClass.IMTS))[Src1.NUInt] > 0);
    end;
    vmsmQueryIntf: begin
      SrcClass := PVMObject(PByte(Dst.PTR) - SizeOf(TVMObjHeader)).TypeInfo;
      if PIMTS(GetIMGPtr(SrcClass.IMTS))[Src1.NUInt] = 0 then
        Dst.I64 := 0;
    end;
    vmsmStrRefCount: Dst.I64 := PInt32(PByte(Src1.PTR) - STR_REFCNT_OFFSET)^;
    vmsmArrRefCount: Dst.I64 := PInt32(PByte(Src1.PTR) - ARR_REFCNT_OFFSET)^;
    vmsmObjRefCount: Dst.I64 := PVMObject(PByte(Src1.PTR) - SizeOf(TVMObjHeader)).RefCount;
    vmsmNow: Dst.F64 := Now;
    vmsmQTypeInfo: begin
      TI := GetRTTIPtr(Src1.NUInt);
      Dst.PTR := PByte(TI) + SizeOf(TVMObjHeader);
    end;
    vmsmGetCallStack: Dst.PTR := _GetCallStack(Src1.PTR, SP);
    vmsmGetCurUnit: Dst.PTR := FRttiUnits[Src1.U32];
    vmsmGetUnitsList: Dst.PTR := FRttiUnits;
  end;
end;

procedure _VM_VARIANT_ASSIGN(Dst, Src: PVMVariant); inline;
begin
  Dst.Assign(Src^);
end;

//=============================================================================================|
//                                   ФОРМАТ КОМАНДЫ (32 bit)                                   |
//------------------------|-----------------------|-----------------------|--------------------|
//|31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|15|14|13|12|11|10|09|08|07|06|05|04|03|02|00|
//------------------------|-----------------------|-----------------------|--------------------|
// RR PC PC PC PC PC PC PC DR DR DR DR S1 S1 S1 S1|S2 S2 S2 S2 CC CC CC CC|IC IC IC IC IC IC IC|
//------------------------|-----------------------|-----------------------|--------------------|
//
// IC - код инсрукци (8 бит)
// СC - код условия исполнения (4 бит)
// PC - кол-во дополнительных слов инструкции (8 бит)
// DR - Destination Register - регистр приемник (4 бит)
// S1 - Source Register 1 - регистр источник (4 бит)
// S2 - Source Register 2 - регистр источник (4 бит)
// PC - кол-во слов данных инструкции (7 бит)
// RR - зарезервировано транслятором (1 бит)
//=============================================================================================|

procedure TILMachine.CallVMProc(MP: PNativeUInt; {указатель на код процедры}
                                SP: PByte;       {Указатель на стек процедуры}
                                GP: PByte        {указатель на память глобальных переменных} );
type TInstruction = array [0..3] of Byte;
     PInstruction = ^TInstruction;
var
  PR: PNativeUInt;          // прамерты инструкции
  Instruction: NativeUInt;  // инструкция (4 байта)
  Cond: TILCondition;       // условие исполнени инструкции
  Dst, Src1, Src2: PVMReg;  // регистры инструкции
  Registers: TVMRegisters;  // все регистры
  Idx: Integer;
  Flags: TVMFlags;          // флаги процесоров
  NewSP: PByte;
  StackSize: Integer;
begin
  Flags := [];
  StackSize := 0;
  {$IFDEF DEBUG}
  FillChar(Registers, SIZEOF(Registers), #0);
  {$ENDIF}
  while True do
  begin
    Instruction := MP^;

    Idx := (Instruction shr 20) and 15;
    Dst := @Registers[Idx];

    Idx := (Instruction shr 16) and 15;
    Src1 := @Registers[Idx];

    Idx := (Instruction shr 12) and 15;
    Src2 := @Registers[Idx];

    Idx := PInstruction(MP)[3];

    Inc(MP);
    PR := MP;

    Inc(MP, Idx);

    {$OVERFLOWCHECKS OFF}

    // во втором байте инструкции содержится условие
    Cond := TILCondition(Byte(Instruction shr 8) and 15);
    if (Cond = cNone) or (Cond in Flags) then
    begin
    case TVMCode(Instruction) of
      VM_NOPE: continue;
      VM_STACK: StackSize := PR^;
      //==================================================================================================
      // LOAD
      //==================================================================================================
      LD_C_I32: Dst.I64 := Int32(PR^);
      LD_C_U32: Dst.U64 := UInt32(PR^);
      LD_C_I64: Dst.I64 := PInt64(PR)^;
      LD_C_F32: Dst.F64 := PFlt32(PR)^;
      LD_C_F64: Dst.F64 := PFlt64(PR)^;
      LD_C_ZERO: Dst.I64 := 0;
      LD_C_ONE: Dst.I64 := 1;

      LD_L_I8 : Dst.I64 := PInt8(SP + PR^)^;
      LD_L_U8 : Dst.U64 := PUInt8(SP + PR^)^;
      LD_L_I16: Dst.I64 := PInt16(SP + PR^)^;
      LD_L_U16: Dst.U64 := PUInt16(SP + PR^)^;
      LD_L_I32: Dst.I64 := PInt32(SP + PR^)^;
      LD_L_I64: Dst.I64 := PInt64(SP + PR^)^;
      LD_L_F32: Dst.F64 := PFlt32(SP + PR^)^;
      LD_L_PTR: Dst.PTR := SP + PR^;

      LD_R_I8 : Dst.I64 := PInt8(PPtr(SP + PR^)^)^;
      LD_R_U8 : Dst.U64 := PUInt8(PPtr(SP + PR^)^)^;
      LD_R_I16: Dst.I64 := PInt16(PPtr(SP + PR^)^)^;
      LD_R_U16: Dst.U64 := PUInt16(PPtr(SP + PR^)^)^;
      LD_R_I32: Dst.I64 := PInt32(PPtr(SP + PR^)^)^;
      LD_R_I64: Dst.I64 := PInt64(PPtr(SP + PR^)^)^;
      LD_R_F32: Dst.F64 := PFlt32(PPtr(SP + PR^)^)^;

      LD_G_I8 : Dst.I64 := PInt8(GP + PR^)^;
      LD_G_U8 : Dst.U64 := PUInt8(GP + PR^)^;
      LD_G_I16: Dst.I64 := PInt16(GP + PR^)^;
      LD_G_U16: Dst.U64 := PUInt16(GP + PR^)^;
      LD_G_I32: Dst.I64 := PInt32(GP + PR^)^;
      LD_G_I64: Dst.I64 := PInt64(GP + PR^)^;
      LD_G_F32: Dst.F64 := PFlt32(GP + PR^)^;
      LD_G_PTR,
      LD_G_PROC : Dst.PTR := Pointer(GP + PR^);

      // load value by addr in Src1 + [imm offset]
      LD_D_I8 : if Idx > 0 then Dst.I32 := PInt8(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt8(Src1.PTR)^;
      LD_D_U8 : if Idx > 0 then Dst.U32 := PUInt8(PByte(Src1.PTR) + PR^)^ else Dst.U32 := PUInt8(Src1.PTR)^;
      LD_D_I16: if Idx > 0 then Dst.I32 := PInt16(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt16(Src1.PTR)^;
      LD_D_U16: if Idx > 0 then Dst.U32 := PUInt16(PByte(Src1.PTR) + PR^)^ else Dst.U32 := PUInt16(Src1.PTR)^;
      LD_D_I32: if Idx > 0 then Dst.I32 := PInt32(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt32(Src1.PTR)^;
      LD_D_I64: if Idx > 0 then Dst.I64 := PInt64(PByte(Src1.PTR) + PR^)^ else Dst.I64 := PInt64(Src1.PTR)^;
      LD_D_F32: if Idx > 0 then Dst.F64 := PFlt32(PByte(Src1.PTR) + PR^)^ else Dst.F64 := PFlt32(Src1.PTR)^;
      //==================================================================================================
      // STORE
      //==================================================================================================
      MOVE_L_I32: PInt32(SP + PR^)^ := PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^;
      ST_C_I32: PInt32(SP + PR^)^ := PInt32(PByte(PR) + PTR_SIZE)^;
      ST_L_I8 : PInt8(SP + PR^)^ := Src1.I8;
      ST_L_I16: PInt16(SP + PR^)^ := Src1.I16;
      ST_L_I32: PInt32(SP + PR^)^ := Src1.I32;
      ST_L_I64: PInt64(SP + PR^)^ := Src1.I64;
      ST_L_F32: PFlt32(SP + PR^)^ := Src1.F64;
      ST_L_VAR: _VM_VARIANT_ASSIGN(PVMVariant(SP + PR^), Src1.PVrnt);

      ST_R_I8 : PInt8(PPtr(SP + PR^)^)^ := Src1.I8;
      ST_R_I16: PInt16(PPtr(SP + PR^)^)^ := Src1.I16;
      ST_R_I32: PInt32(PPtr(SP + PR^)^)^ := Src1.I32;
      ST_R_I64: PInt64(PPtr(SP + PR^)^)^ := Src1.I64;
      ST_R_F32: PFlt32(PPtr(SP + PR^)^)^ := Src1.F64;

      ST_G_I8 : PInt8(GP + PR^)^ := Src1.I8;
      ST_G_I16: PInt16(GP + PR^)^ := Src1.I16;
      ST_G_I32: PInt32(GP + PR^)^ := Src1.I32;
      ST_G_I64: PInt64(GP + PR^)^ := Src1.I64;
      ST_G_F32: PFlt32(GP + PR^)^ := Src1.F64;
      ST_G_VAR: PVMVariant(GP + PR^).Assign(Src1.PVrnt^);

      ST_D_I8:  if Idx > 0 then PInt8(PByte(Dst.PTR) + PR^)^ := Src1.I8 else PInt8(Dst.PTR)^ := Src1.I8;
      ST_D_I16: if Idx > 0 then PInt16(PByte(Dst.PTR) + PR^)^ := Src1.I16 else PInt16(Dst.PTR)^ := Src1.I16;
      ST_D_I32: if Idx > 0 then PInt32(PByte(Dst.PTR) + PR^)^ := Src1.I32 else PInt32(Dst.PTR)^ := Src1.I32;
      ST_D_I64: if Idx > 0 then PInt64(PByte(Dst.PTR) + PR^)^ := Src1.I64 else PInt64(Dst.PTR)^ := Src1.I64;
      ST_D_F32: if Idx > 0 then PFlt32(PByte(Dst.PTR) + PR^)^ := Src1.F64 else PFlt32(Dst.PTR)^ := Src1.F64;

      MOVE_REG: Dst.U64 := Src1.U64;
      MOVE_REG_TO_MEM: Move(Src1.U64, Dst.PTR^, PR^);
      //==================================================================================================
      // CLEAR
      //==================================================================================================
      CLR_L_I8:  PInt8(SP + PR^)^  := 0;
      CLR_L_I16: PInt16(SP + PR^)^ := 0;
      CLR_L_I32: PInt32(SP + PR^)^ := 0;
      CLR_L_I64: PInt64(SP + PR^)^ := 0;
      CLR_L_F32: PFlt32(SP + PR^)^ := 0;
      CLR_L_F64: PFlt64(SP + PR^)^ := 0;
      //==================================================================================================
      CLR_R_I8 : PInt8(PPtr(SP + PR^)^)^  := 0;
      CLR_R_I16: PInt16(PPtr(SP + PR^)^)^ := 0;
      CLR_R_I32: PInt32(PPtr(SP + PR^)^)^ := 0;
      CLR_R_I64: PInt64(PPtr(SP + PR^)^)^ := 0;
      CLR_R_F32: PFlt32(PPtr(SP + PR^)^)^ := 0;
      CLR_R_F64: PFlt64(PPtr(SP + PR^)^)^ := 0;
      //==================================================================================================
      CLR_G_I8 : PInt8(GP + PR^)^  := 0;
      CLR_G_I16: PInt16(GP + PR^)^ := 0;
      CLR_G_I32: PInt32(GP + PR^)^ := 0;
      CLR_G_I64: PInt64(GP + PR^)^ := 0;
      CLR_G_F32: PFlt32(GP + PR^)^ := 0;
      CLR_G_F64: PFlt64(GP + PR^)^ := 0;
      //==================================================================================================
      CLR_D_I8 : if Idx > 0 then PInt8(PByte(Dst.PTR) + PR^)^ := 0 else PInt8(Dst.PTR)^ := 0;
      CLR_D_I16: if Idx > 0 then PInt16(PByte(Dst.PTR) + PR^)^ := 0 else PInt16(Dst.PTR)^ := 0;
      CLR_D_I32: if Idx > 0 then PInt32(PByte(Dst.PTR) + PR^)^ := 0 else PInt32(Dst.PTR)^ := 0;
      CLR_D_I64: if Idx > 0 then PInt64(PByte(Dst.PTR) + PR^)^ := 0 else PInt64(Dst.PTR)^ := 0;
      CLR_D_F32: if Idx > 0 then PFlt32(PByte(Dst.PTR) + PR^)^ := 0 else PFlt32(Dst.PTR)^ := 0;
      CLR_D_F64: if Idx > 0 then PFlt64(PByte(Dst.PTR) + PR^)^ := 0 else PFlt64(Dst.PTR)^ := 0;
      //==================================================================================================
      // CMP
      //==================================================================================================
      CMP_I32_C: SetCondition(Dst.I32 - Int32(PR^), Flags);
      CMP_L_C32: SetCondition(PInt32(SP + PR^)^ - Int32(PNUInt(PByte(PR) + PTR_SIZE)^), Flags);
      CMP_I32: SetCondition(Dst.I32 - Src1.I32, Flags);
      CMP_L_I32: SetCondition(PInt32(SP + PR^)^ - PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^, Flags);
      CMP_I64: SetCondition(Dst.I64 - Src1.I64, Flags);
      CMP_F64: SetCondition(Dst.F64 - Src1.F64, Flags);
      CMP_ASTR: SetCondition(_VM_STRA_COMPARE(Dst, Src1), Flags);
      CMP_USTR: SetCondition(_VM_STRU_COMPARE(Dst, Src1), Flags);
      CMP_VAR: SetCondition(TVMVariant.Compare(Dst.PVrnt^, Src1.PVrnt^), Flags);
      CMP_TEST32: SetCondition(Src1.I32 and Src2.I32, Flags);
      CMP_TEST64: SetCondition(Src1.U64 and Src2.U64, Flags);
      CMP_MEM_VS_REG: _VM_MEM_COMPARE(Dst.PTR, Src1, PR^, Flags);
      CMP_MEM: _VM_MEM_COMPARE(Dst.PTR, Src1.PTR, PR^, Flags);
      //==================================================================================================
      // ADD
      //==================================================================================================
      INC_L_I32: Inc(PInt32(SP + PR^)^);
      ADD_I32_C: Dst.I32 := Src1.I32 + Int32(PR^);
      ADD_L_I32_C: Dst.I32 := PInt32(SP + PR^)^ + PInt32(PByte(PR) + PTR_SIZE)^;
      ADD_NUINT_C: Dst.NUInt := Src1.NUInt + PR^;
      ADD_U32: Dst.U32 := Src1.U32 + Src2.U32;
      ADD_U64: Dst.U64 := Src1.U64 + Src2.U64;
      ADD_I32: Dst.I32 := Src1.I32 + Src2.I32;
      ADD_L_I32: Dst.I32 := PInt32(SP + PR^)^ + PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^;
      ADD_I64: Dst.I64 := Src1.I64 + Src2.I64;
      ADD_F64: Dst.F64 := Src1.F64 + Src2.F64;
      ADD_F64_CI32: Dst.F64 := Src1.F64 + Int32(PR^);
      ADD_ASTR: _VM_STRA_CONCAT(Dst, Src1, Src2);
      ADD_USTR: _VM_STRU_CONCAT(Dst, Src1, Src2);
      //==================================================================================================
      // SUB
      //==================================================================================================
      SUB_I32_C: Dst.I32 := Src1.I32 - Int32(PR^);
      SUB_F64_CI32: Dst.F64 := Src1.F64 - Int32(PR^);
      SUB_I32: Dst.I32 := Src1.I32 - Src2.I32;
      SUB_I64: Dst.I64 := Src1.I64 - Src2.I64;
      SUB_F64: Dst.F64 := Src1.F64 - Src2.F64;
      //==================================================================================================
      // MUL
      //==================================================================================================
      MUL_C32: Dst.I32 := Src1.I32 * NativeInt(PR^);
      MUL_I32: Dst.I32 := Src1.I32 * Src2.I32;
      MUL_I64: Dst.I64 := Src1.I64 * Src2.I64;
      MUL_F64: Dst.F64 := Src1.F64 * Src2.F64;
      FMA_U32: Dst.U32 := Src1.U32 + Src2.U32*PR^;
      FMA_U64: Dst.U64 := Src1.U64 + Src2.U64*PR^;
      //==================================================================================================
      // IDIV
      //==================================================================================================
      IDIV_U32_C: Dst.U32 := Src1.U32 div PR^;
      IDIV_I32: Dst.I32 := Src1.I32 div Src2.I32;
      IDIV_I64: Dst.I64 := Src1.I64 div Src2.I64;
      //==================================================================================================
      // IMOD
      //==================================================================================================
      IMOD_U32_C: Dst.U32 := Src1.U32 mod PR^;
      IMOD_I32: Dst.I32 := Src1.I32 mod Src2.I32;
      IMOD_I64: Dst.I64 := Src1.I64 mod Src2.I64;
      //==================================================================================================
      // DIV
      //==================================================================================================
      DIV_I32: Dst.F64 := Src1.I32 / Src2.I32;
      DIV_I64: Dst.F64 := Src1.I64 / Src2.I64;
      DIV_F64: Dst.F64 := Src1.F64 / Src2.F64;
      //==================================================================================================
      // NEG
      //==================================================================================================
      NEG_I32: Dst.I32 := -Src1.I32;
      NEG_I64: Dst.I64 := -Src1.I64;
      NEG_F64: Dst.F64 := -Src1.F64;
      //==================================================================================================
      // SHL
      //==================================================================================================
      BIN_SHL32: Dst.U32 := Src1.U32 shl Src2.U32;
      BIN_SHL64: Dst.U64 := Src1.U64 shl Src2.U64;
      //==================================================================================================
      // SHR
      //==================================================================================================
      BIN_SHR32: Dst.U32 := Src1.U32 shr Src2.U32;
      BIN_SHR64: Dst.U64 := Src1.U64 shr Src2.U64;
      //==================================================================================================
      // AND
      //==================================================================================================
      BIN_AND32_C: Dst.U32 := Src1.U32 and PUInt32(PR)^;
      BIN_AND64_C: Dst.U64 := Src1.U64 and PUInt64(PR)^;
      BIN_AND32: Dst.U32 := Src1.U32 and Src2.U32;
      BIN_AND64: Dst.U64 := Src1.U64 and Src2.U64;
      //==================================================================================================
      // OR
      //==================================================================================================
      BIN_OR32_C: Dst.U32 := Src1.U32 or PUInt32(PR)^;
      BIN_OR64_C: Dst.U64 := Src1.U64 or PUInt64(PR)^;
      BIN_OR32: Dst.U32 := Src1.U32 or Src2.U32;
      BIN_OR64: Dst.U64 := Src1.U64 or Src2.U64;
      //==================================================================================================
      // XOR
      //==================================================================================================
      BIN_XOR32: Dst.I32 := Src1.I32 xor Src2.I32;
      BIN_XOR64: Dst.I64 := Src1.I64 xor Src2.I64;
      //==================================================================================================
      // NOT
      //==================================================================================================
      BIN_NOT32: Dst.U32 := not Src1.U32;
      BIN_NOT64: Dst.U64 := not Src1.U64;
      //==================================================================================================
      // UNIQUE
      //==================================================================================================
      UNIQUE_USTR: UniqueString(string(Dst.PTR));
      UNIQUE_ASTR: {$IFNDEF NEXTGEN}UniqueString(AnsiString(Dst.PTR)){$ENDIF};
      //==================================================================================================
      // ETHROW
      //==================================================================================================
      ETHROW: _VM_ETHROW(FMem, Dst.PTR);
      SCHECKB: _VM_SCHECKB(Dst^, PR);
      DCHECKB: _VM_DCHECKB(Dst, Src1);
      ACHECKB: _VM_ACHECKB(Dst, Src1);
      //==================================================================================================
      // ARRAY_LENGTH
      //==================================================================================================
      ARRAY_LENGTH: if Assigned(Src1.PTR) then Dst.U64 := PNUInt(PByte(Src1.PTR) - PTR_SIZE)^ else Dst.U64 := 0;
      ARRAY_INCREF: _VM_ARRAY_INCREF(Dst.PTR);
      ARRAY_DECREF: if Idx = 0 then _VM_ARRAY_DECREF(Dst) else _VM_ARRAY_DECREF_FINAL(Dst, StackSize, GP, SP, PR);
      STR_DECREF: _VM_STR_DECREF(Dst.PTR);
      {$IFDEF CPUX64}STR_INCREF: _VM_STR_INCREF(Dst.PTR);{$ENDIF}
      {$IFDEF CPUX64}STR_LENGTH: if Assigned(Src1.PTR) then Dst.U64 := PUInt32(PByte(Src1.PTR) - 4)^ else Dst.U64 := 0;{$ENDIF}
      {$IFDEF CPUX64}STR_MOVE: if Assigned(Src1.PTR) then Move(Src1.PTR^, Dst.PTR^, PUInt32(PByte(Src1.PTR) - 4)^ * PR^);{$ENDIF}
      //==================================================================================================
      DINF_INCREF: if Dst.PTR <> nil then IInterface(Dst.PTR)._AddRef;
      DINF_DECREF: if Dst.PTR <> nil then IInterface(Dst.PTR)._Release;
      //==================================================================================================
      OBJ_CREATE: Dst.PTR := _VM_OBJ_CREATE(PR);
      OBJ_INCREF: _VM_OBJ_INCREF(Dst.PTR);
      OBJ_DECREF: _VM_OBJ_DECREF(Dst.PTR, StackSize, GP, SP, PR);
      OBJ_WEAKREF: begin _VM_OBJ_WEAKREF(Dst.PTR); continue; end;
      OBJ_STRONGREF: begin _VM_OBJ_STRONGREF(Dst.PTR); continue; end;
      //==================================================================================================
      WEAK_INCREF: _VM_WEAK_INCREF(Dst.PTR);
      WEAK_DECREF: _VM_WEAK_DECREF(Dst.PTR);
      VIRT_METHOD_PTR: _VM_GET_METHOD_PTR(Dst, Src1, PR, GP);
      VM_SET_METHOD_PTR: begin
        PILMethod(Dst.PTR).Proc := Src2.PTR;
        PILMethod(Dst.PTR).Self := Src1.PTR;
      end;
      //==================================================================================================
      // RAW MEMORY ALLOCATE/FREE/SET
      //==================================================================================================
      MEM_ALLOC: begin
        Dst.PTR := GetMemory(PR^);
        FillChar(Dst.PTR^, PR^, #0);
      end;
      MEM_FREE: FreeMemory(Dst.PTR);
      MEM_SET: FillChar(Dst.PTR^, Src1.U32, PR^);
      ARRAY_MEM_ALLOC: _VM_DYNARRAY_CREATE(Dst, Src1, PR^);
      STRU_CREATE: Dst.PTR := _VM_STRU_CREATE(Src1.U32);
      STRA_CREATE: Dst.PTR := _VM_STRA_CREATE(Src1.U32);
      ARRAY_INIT: begin
        PInt32(PByte(Dst.PTR) + STR_REC_PADDING)^ := 1;                // refcount = 1
        PNUInt(PByte(Dst.PTR) + STR_REC_PADDING + 4)^ := UInt32(PR^);  // length
        Dst.PTR := PByte(Dst.PTR) + 4 + PTR_SIZE + STR_REC_PADDING;    // adjust the pointer
      end;
      //==================================================================================================
      // MOVE_MEM
      //==================================================================================================
      MOVE_MEM_C: Move(Src1.PTR^, Dst.PTR^, PR^);
      MOVE_MEM: Move(Src1.PTR^, Dst.PTR^, Src2.U32);
      MOVE_ARRAY: if Assigned(Src1.PTR) then Move(Src1.PTR^, Dst.PTR^, PUInt32(PByte(Src1.PTR) - PTR_SIZE)^ * PR^);
      //==================================================================================================
      // CONVERT
      //==================================================================================================
      CNV_F64_S32: Dst.F64 := Dst.I32;
      CNV_F64_U32: Dst.F64 := Dst.U32;
      CNV_F64_S64: Dst.F64 := Dst.I64;
      CNV_F64_U64: Dst.F64 := Dst.U64;
      CNV_ACHR_TO_ASTR: begin
        PR := _VM_STRA_CREATE(1);
        PAnsiChar(PR)^ := AnsiChar(Src1.U8);
        Dst.PTR := PR;
      end;
      CNV_UCHR_TO_USTR: begin
        PR := _VM_STRU_CREATE(1);
        PUChar(PR)^ := Char(Src1.U16);
        Dst.PTR := PR;
      end;
      CNV_ASTR_TO_USTR: Dst.PTR := _STRU_FROM_STRA(Src1);
      CNV_USTR_TO_ASTR: Dst.PTR := _STRA_FROM_STRU(Src1);
      // Variants
      CNV_VAR_TO_VALUE: _CNV_FROM_VARIANT(Dst, Src1, TDataTypeID(PR^));
      CNV_VALUE_TO_VAR: _CNV_TO_VARIANT(Dst, Src1, TDataTypeID(PR^));
      VAR_RELEASE: _VAR_RELEASE(Dst);
      //==================================================================================================
      // CALL обычный вызов процедуры
      //==================================================================================================
      CALL_PROC, CALL_NEAR: begin
        // вычитываем из инструкции вызова размер стека вызывающей процедуры
        // Instruction используется как StackSize
        Instruction := PNInt(PByte(PR) + PTR_SIZE)^;
        // по смещению StackSize сохраняем указатель на свой стек
        PPtr(SP + Instruction)^ := SP;
        // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
        PPtr(SP + Instruction + PTR_SIZE)^ := PByte(PR) + PTR_SIZE*2;
        // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
        SP := SP + Instruction + PTR_SIZE*2;
        // загружаем указатель на код вызываемой процедуры
        MP := PNativeUInt(GP + PR^);
      end;
      //==================================================================================================
      // CALL вызов виртуального метода
      //==================================================================================================
      CALL_VIRT: MP := _VM_PROC_CALL_VIRTUAL(Dst.PTR, SP, PR, GP);
      CALL_INTF: MP := _VM_PROC_CALL_INTERFACE(Dst.PTR, SP, PR, GP);
      //==================================================================================================
      // CALL косвенный вызов процедуры
      //==================================================================================================
      CALL_INDIRECT: begin
        // вычитываем из инструкции вызова размер стека вызывающей процедуры
        // Instruction используется как StackSize
        Instruction := PR^;
        // по смещению StackSize сохраняем указатель на свой стек
        PPtr(SP + Instruction)^ := SP;
        // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
        PPtr(SP + Instruction + PTR_SIZE)^ := PByte(PR) + PTR_SIZE;
        // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
        SP := SP + Instruction + PTR_SIZE*2;
        // загружаем указатель на код вызываемой процедуры
        MP := Dst.PTR;
      end;
      //==================================================================================================
      CALL_EXT_FAST: InvokeExternalStatic(FHeader, PR, SP);
      CALL_EXT_FASTV: InvokeExternalVirtual(FHeader, PR, SP);
      CALL_EXT_FAST_INTF_PROC: InvokeExternalInterfaceMethod(FHeader, PR, SP);
      CALL_EXT_FAST_INTF_FUNC: InvokeExternalInterfaceFunc(FHeader, PR, SP);
      CALL_EXT_COMMON_PROC: InvokeExternalCommonProc(FHeader, PR, SP);
      CALL_EXT_COMMON_FUNC: InvokeExternalCommonFunc(FHeader, PR, SP);
      //==================================================================================================
      VM_SYSMACRO: _VM_SYSMACRO(TVMMacroID(PR^), Dst, Src1, Src2, SP);
      //==================================================================================================
      // JUMP
      //==================================================================================================
      VM_JMP: MP := PNativeUInt(GP + PR^);
      //==================================================================================================
      // RET
      //==================================================================================================
      PROC_RET: begin
        MP := PNativeUInt(PNativeUInt(SP - PTR_SIZE)^);   // адрес вызывающей инструкции
        if MP <> nil then begin
          NewSP := PByte(PNativeUInt(SP - PTR_SIZE*2)^);  // сохраненный размер стека вызывающей процедуры
          StackSize := SP - NewSP - PTR_SIZE*2;
          SP := NewSP;
          {$IFDEF DEBUG}
          if SP = nil then
            raise Exception.Create('Corrupt stack detected');
          {$ENDIF}
          Continue;
        end;
        Exit; // если указатель на память равен nil значит это был корневой вызов, выходим
      end;
      //==================================================================================================
    else
      Instruction := Word(Instruction);
      raise Exception.CreateFmt('Unknown VM code: %s (%d)', [TEnum<TVMCode>.Name(TVMCode(Instruction)), Instruction]);
    end;
    end;
    {$OVERFLOWCHECKS ON}
  end;
  {$IFDEF FPC} writeln('end'); {$ENDIF}
end;

procedure TILMachine.DebugCallILProc(MP: PNativeUInt; SP, GP: PByte);
type TInstruction = array [0..3] of Byte;
     PInstruction = ^TInstruction;
var
  PR: PNativeUInt;         // прамерты инструкции
  Instruction: NativeUInt; // инструкция (4 байта)
  Cond: TILCondition;      // условие исполнени инструкции
  Dst, Src1, Src2: PVMReg;    // регистры инструкции
  Registers: TVMRegisters;
  StackSize: Integer;
  Idx: Integer;
  Flags: TVMFlags;           // флаги процесоров
  DebugContext: TVMDebugContext;
  NewSP: PByte;
begin
  Flags := [];
  StackSize := 0;
  FillChar(Registers, SIZEOF(Registers), #0);
  FOnVMCall(SP, NativeUInt(MP) - NativeUInt(FMem.Memory));
  while True do
  begin
    Instruction := MP^;

    Idx := (Instruction shr 20) and 15;
    Dst := @Registers[Idx];

    Idx := (Instruction shr 16) and 15;
    Src1 := @Registers[Idx];

    Idx := (Instruction shr 12) and 15;
    Src2 := @Registers[Idx];

    Idx := PInstruction(MP)[3];

    // зависаем на ожидании от пользователя
    if Assigned(FOnBreak) then
    begin
      DebugContext.Stack := SP;
      DebugContext.IMGOffset := NativeUInt(MP) - NativeUInt(FMem.Memory);
      DebugContext.Registers := @Registers;
      FOnBreak(DebugContext);
      if DebugContext.Terminate then
      begin
        Exit;
      end;
    end;

    Inc(MP);
    PR := MP;

    Inc(MP, Idx);

    // во втором байте инструкции содержится условие
    Cond := TILCondition(Byte(Instruction shr 8) and 15);
    if (Cond = cNone) or (Cond in Flags) then
    case TVMCode(Instruction) of
      VM_NOPE: continue;
      VM_STACK: StackSize := PR^;
      //==================================================================================================
      // LOAD
      //==================================================================================================
      LD_C_I32: Dst.I64 := Int32(PR^);
      LD_C_U32: Dst.U64 := UInt32(PR^);
      LD_C_I64: Dst.I64 := PInt64(PR)^;
      LD_C_F32: Dst.F64 := PFlt32(PR)^;
      LD_C_F64: Dst.F64 := PFlt64(PR)^;

      LD_L_I8 : Dst.I64 := PInt8(SP + PR^)^;
      LD_L_U8 : Dst.U64 := PUInt8(SP + PR^)^;
      LD_L_I16: Dst.I64 := PInt16(SP + PR^)^;
      LD_L_U16: Dst.U64 := PUInt16(SP + PR^)^;
      LD_L_I32: Dst.I64 := PInt32(SP + PR^)^;
      LD_L_I64: Dst.I64 := PInt64(SP + PR^)^;
      LD_L_F32: Dst.F64 := PFlt32(SP + PR^)^;
      LD_L_PTR: Dst.PTR := SP + PR^;

      LD_R_I8 : Dst.I64 := PInt8(PPtr(SP + PR^)^)^;
      LD_R_U8 : Dst.U64 := PUInt8(PPtr(SP + PR^)^)^;
      LD_R_I16: Dst.I64 := PInt16(PPtr(SP + PR^)^)^;
      LD_R_U16: Dst.U64 := PUInt16(PPtr(SP + PR^)^)^;
      LD_R_I32: Dst.I64 := PInt32(PPtr(SP + PR^)^)^;
      LD_R_I64: Dst.I64 := PInt64(PPtr(SP + PR^)^)^;
      LD_R_F32: Dst.F64 := PFlt32(PPtr(SP + PR^)^)^;

      LD_G_I8 : Dst.I64 := PInt8(GP + PR^)^;
      LD_G_U8 : Dst.U64 := PUInt8(GP + PR^)^;
      LD_G_I16: Dst.I64 := PInt16(GP + PR^)^;
      LD_G_U16: Dst.U64 := PUInt16(GP + PR^)^;
      LD_G_I32: Dst.I64 := PInt32(GP + PR^)^;
      LD_G_I64: Dst.I64 := PInt64(GP + PR^)^;
      LD_G_F32: Dst.F64 := PFlt32(GP + PR^)^;
      LD_G_PTR,
      LD_G_PROC : Dst.PTR := Pointer(GP + PR^);

      // загрузка по адресу (разименоваене (D)ereference) хранящимуся в Src1 + [смещение]
      LD_D_I8 : if Idx > 0 then Dst.I32 := PInt8(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt8(Src1.PTR)^;
      LD_D_U8 : if Idx > 0 then Dst.U32 := PUInt8(PByte(Src1.PTR) + PR^)^ else Dst.U32 := PUInt8(Src1.PTR)^;
      LD_D_I16: if Idx > 0 then Dst.I32 := PInt16(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt16(Src1.PTR)^;
      LD_D_U16: if Idx > 0 then Dst.U32 := PUInt16(PByte(Src1.PTR) + PR^)^ else Dst.U32 := PUInt16(Src1.PTR)^;
      LD_D_I32: if Idx > 0 then Dst.I32 := PInt32(PByte(Src1.PTR) + PR^)^ else Dst.I32 := PInt32(Src1.PTR)^;
      LD_D_I64: if Idx > 0 then Dst.I64 := PInt64(PByte(Src1.PTR) + PR^)^ else Dst.I64 := PInt64(Src1.PTR)^;
      LD_D_F32: if Idx > 0 then Dst.F64 := PFlt32(PByte(Src1.PTR) + PR^)^ else Dst.F64 := PFlt32(Src1.PTR)^;
      //==================================================================================================
      // SET
      //==================================================================================================
      LD_C_ZERO: Dst.I32 := 0;
      LD_C_ONE: Dst.I32 := 1;
      MEM_SET: FillChar(Dst.PTR^, Src1.U32, PR^);
      //==================================================================================================
      // STORE
      //==================================================================================================
      MOVE_L_I32: PInt32(SP + PR^)^ := PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^;
      ST_C_I32: PInt32(SP + PR^)^ := PInt32(PByte(PR) + PTR_SIZE)^;
      ST_L_I8 : PInt8(SP + PR^)^ := Src1.I8;
      ST_L_I16: PInt16(SP + PR^)^ := Src1.I16;
      ST_L_I32: PInt32(SP + PR^)^ := Src1.I32;
      ST_L_I64: PInt64(SP + PR^)^ := Src1.I64;
      ST_L_F32: PFlt32(SP + PR^)^ := Src1.F64;
      ST_L_VAR: PVMVariant(SP + PR^).Assign(Src1.PVrnt^);

      ST_R_I8 : PInt8(PPtr(SP + PR^)^)^ := Src1.I8;
      ST_R_I16: PInt16(PPtr(SP + PR^)^)^ := Src1.I16;
      ST_R_I32: PInt32(PPtr(SP + PR^)^)^ := Src1.I32;
      ST_R_I64: PInt64(PPtr(SP + PR^)^)^ := Src1.I64;
      ST_R_F32: PFlt32(PPtr(SP + PR^)^)^ := Src1.F64;

      ST_G_I8 : PInt8(GP + PR^)^ := Src1.I8;
      ST_G_I16: PInt16(GP + PR^)^ := Src1.I16;
      ST_G_I32: PInt32(GP + PR^)^ := Src1.I32;
      ST_G_I64: PInt64(GP + PR^)^ := Src1.I64;
      ST_G_F32: PFlt32(GP + PR^)^ := Src1.F64;
      ST_G_VAR: PVMVariant(GP + PR^).Assign(Src1.PVrnt^);

      ST_D_I8:  if Idx > 0 then PInt8(PByte(Dst.PTR) + PR^)^ := Src1.I8 else PInt8(Dst.PTR)^ := Src1.I8;
      ST_D_I16: if Idx > 0 then PInt16(PByte(Dst.PTR) + PR^)^ := Src1.I16 else PInt16(Dst.PTR)^ := Src1.I16;
      ST_D_I32: if Idx > 0 then PInt32(PByte(Dst.PTR) + PR^)^ := Src1.I32 else PInt32(Dst.PTR)^ := Src1.I32;
      ST_D_I64: if Idx > 0 then PInt64(PByte(Dst.PTR) + PR^)^ := Src1.I64 else PInt64(Dst.PTR)^ := Src1.I64;
      ST_D_F32: if Idx > 0 then PFlt32(PByte(Dst.PTR) + PR^)^ := Src1.F64 else PFlt32(Dst.PTR)^ := Src1.F64;

      MOVE_REG: Dst.U64 := Src1.U64;
      MOVE_REG_TO_MEM: Move(Src1.U64, Dst.PTR^, PR^);
      //==================================================================================================
      // CLEAR
      //==================================================================================================
      CLR_L_I8:  PInt8(SP + PR^)^  := 0;
      CLR_L_I16: PInt16(SP + PR^)^ := 0;
      CLR_L_I32: PInt32(SP + PR^)^ := 0;
      CLR_L_I64: PInt64(SP + PR^)^ := 0;
      CLR_L_F32: PFlt32(SP + PR^)^ := 0;
      CLR_L_F64: PFlt64(SP + PR^)^ := 0;
      //==================================================================================================
      CLR_R_I8 : PInt8(PPtr(SP + PR^)^)^  := 0;
      CLR_R_I16: PInt16(PPtr(SP + PR^)^)^ := 0;
      CLR_R_I32: PInt32(PPtr(SP + PR^)^)^ := 0;
      CLR_R_I64: PInt64(PPtr(SP + PR^)^)^ := 0;
      CLR_R_F32: PFlt32(PPtr(SP + PR^)^)^ := 0;
      CLR_R_F64: PFlt64(PPtr(SP + PR^)^)^ := 0;
      //==================================================================================================
      CLR_G_I8 : PInt8(GP + PR^)^  := 0;
      CLR_G_I16: PInt16(GP + PR^)^ := 0;
      CLR_G_I32: PInt32(GP + PR^)^ := 0;
      CLR_G_I64: PInt64(GP + PR^)^ := 0;
      CLR_G_F32: PFlt32(GP + PR^)^ := 0;
      CLR_G_F64: PFlt64(GP + PR^)^ := 0;
      //==================================================================================================
      CLR_D_I8 : if Idx > 0 then PInt8(PByte(Dst.PTR) + PR^)^ := 0 else PInt8(Dst.PTR)^ := 0;
      CLR_D_I16: if Idx > 0 then PInt16(PByte(Dst.PTR) + PR^)^ := 0 else PInt16(Dst.PTR)^ := 0;
      CLR_D_I32: if Idx > 0 then PInt32(PByte(Dst.PTR) + PR^)^ := 0 else PInt32(Dst.PTR)^ := 0;
      CLR_D_I64: if Idx > 0 then PInt64(PByte(Dst.PTR) + PR^)^ := 0 else PInt64(Dst.PTR)^ := 0;
      CLR_D_F32: if Idx > 0 then PFlt32(PByte(Dst.PTR) + PR^)^ := 0 else PFlt32(Dst.PTR)^ := 0;
      CLR_D_F64: if Idx > 0 then PFlt64(PByte(Dst.PTR) + PR^)^ := 0 else PFlt64(Dst.PTR)^ := 0;
      //==================================================================================================
      // CMP
      //==================================================================================================
      CMP_I32_C: SetCondition(Dst.I32 - Int32(PR^), Flags);
      CMP_L_C32: SetCondition(PInt32(SP + PR^)^ - Int32(PNUInt(PByte(PR) + PTR_SIZE)^), Flags);
      CMP_I32: SetCondition(Dst.I32 - Src1.I32, Flags);
      CMP_L_I32: SetCondition(PInt32(SP + PR^)^ - PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^, Flags);
      CMP_I64: SetCondition(Dst.I64 - Src1.I64, Flags);
      CMP_F64: SetCondition(Dst.F64 - Src1.F64, Flags);
      CMP_ASTR: SetCondition(_VM_STRA_COMPARE(Dst, Src1), Flags);
      CMP_USTR: SetCondition(_VM_STRU_COMPARE(Dst, Src1), Flags);
      CMP_VAR: SetCondition(TVMVariant.Compare(Dst.PVrnt^, Src1.PVrnt^), Flags);
      CMP_TEST32: SetCondition(Dst.I32 and Src1.I32, Flags);
      CMP_TEST64: SetCondition(Dst.U64 and Src1.U64, Flags);
      CMP_MEM_VS_REG: _VM_MEM_COMPARE(Dst.PTR, Src1, PR^, Flags);
      CMP_MEM: _VM_MEM_COMPARE(Dst.PTR, Src1.PTR, PR^, Flags);
      //==================================================================================================
      // ADD
      //==================================================================================================
      INC_L_I32: Inc(PInt32(SP + PR^)^);
      ADD_I32_C: Dst.I32 := Src1.I32 + Int32(PR^);
      ADD_L_I32_C: Dst.I32 := PInt32(SP + PR^)^ + PInt32(PByte(PR) + PTR_SIZE)^;
      ADD_NUINT_C: Dst.NUInt := Src1.NUInt + PR^;
      ADD_U32: Dst.U32 := Src1.U32 + Src2.U32;
      ADD_U64: Dst.U64 := Src1.U64 + Src2.U64;
      ADD_I32: Dst.I32 := Src1.I32 + Src2.I32;
      ADD_L_I32: Dst.I32 := PInt32(SP + PR^)^ + PInt32(SP + PNUInt(PByte(PR) + PTR_SIZE)^)^;
      ADD_I64: Dst.I64 := Src1.I64 + Src2.I64;
      ADD_F64: Dst.F64 := Src1.F64 + Src2.F64;
      ADD_F64_CI32: Dst.F64 := Src1.F64 + Int32(PR^);
      ADD_ASTR: _VM_STRA_CONCAT(Dst, Src1, Src2);
      ADD_USTR: _VM_STRU_CONCAT(Dst, Src1, Src2);
      //==================================================================================================
      // SUB
      //==================================================================================================
      SUB_I32_C: Dst.I32 := Src1.I32 - Int32(PR^);
      SUB_F64_CI32: Dst.F64 := Src1.F64 - Int32(PR^);
      SUB_I32: Dst.I32 := Src1.I32 - Src2.I32;
      SUB_I64: Dst.I64 := Src1.I64 - Src2.I64;
      SUB_F64: Dst.F64 := Src1.F64 - Src2.F64;
      //==================================================================================================
      // MUL
      //==================================================================================================
      MUL_C32: Dst.I32 := Src1.I32 * NativeInt(PR^);
      MUL_I32: Dst.I32 := Src1.I32 * Src2.I32;
      MUL_I64: Dst.I64 := Src1.I64 * Src2.I64;
      MUL_F64: Dst.F64 := Src1.F64 * Src2.F64;
      FMA_U32: Dst.U32 := Src1.U32 + Src2.U32*PR^;
      FMA_U64: Dst.U64 := Src1.U64 + Src2.U64*PR^;
      //==================================================================================================
      // IDIV
      //==================================================================================================
      IDIV_U32_C: Dst.U32 := Src1.U32 div PR^;
      IDIV_I32: Dst.I32 := Src1.I32 div Src2.I32;
      IDIV_I64: Dst.I64 := Src1.I64 div Src2.I64;
      //==================================================================================================
      // IMOD
      //==================================================================================================
      IMOD_U32_C: Dst.U32 := Src1.U32 mod PR^;
      IMOD_I32: Dst.I32 := Src1.I32 mod Src2.I32;
      IMOD_I64: Dst.I64 := Src1.I64 mod Src2.I64;
      //==================================================================================================
      // DIV
      //==================================================================================================
      DIV_I32: Dst.F64 := Src1.I32 / Src2.I32;
      DIV_I64: Dst.F64 := Src1.I64 / Src2.I64;
      DIV_F64: Dst.F64 := Src1.F64 / Src2.F64;
      //==================================================================================================
      // NEG
      //==================================================================================================
      NEG_I32: Dst.I32 := -Src1.I32;
      NEG_I64: Dst.I64 := -Src1.I64;
      NEG_F64: Dst.F64 := -Src1.F64;
      //==================================================================================================
      // SHL
      //==================================================================================================
      BIN_SHL32: Dst.U32 := Src1.U32 shl Src2.U32;
      BIN_SHL64: Dst.U64 := Src1.U64 shl Src2.U64;
      //==================================================================================================
      // SHR
      //==================================================================================================
      BIN_SHR32: Dst.U32 := Src1.U32 shr Src2.U32;
      BIN_SHR64: Dst.U64 := Src1.U64 shr Src2.U64;
      //==================================================================================================
      // AND
      //==================================================================================================
      BIN_AND32_C: Dst.U32 := Src1.U32 and PUInt32(PR)^;
      BIN_AND64_C: Dst.U64 := Src1.U64 and PUInt64(PR)^;
      BIN_AND32: Dst.U32 := Src1.U32 and Src2.U32;
      BIN_AND64: Dst.U64 := Src1.U64 and Src2.U64;
      //==================================================================================================
      // OR
      //==================================================================================================
      BIN_OR32_C: Dst.U32 := Src1.U32 or PUInt32(PR)^;
      BIN_OR64_C: Dst.U64 := Src1.U64 or PUInt64(PR)^;
      BIN_OR32: Dst.U32 := Src1.U32 or Src2.U32;
      BIN_OR64: Dst.U64 := Src1.U64 or Src2.U64;
      //==================================================================================================
      // XOR
      //==================================================================================================
      BIN_XOR32: Dst.I32 := Src1.I32 xor Src2.I32;
      BIN_XOR64: Dst.I64 := Src1.I64 xor Src2.I64;
      //==================================================================================================
      // NOT
      //==================================================================================================
      BIN_NOT32: Dst.U32 := not Src1.U32;
      BIN_NOT64: Dst.U64 := not Src1.U64;
      //==================================================================================================
      // UNIQUE
      //==================================================================================================
      UNIQUE_USTR: UniqueString(string(Dst.PTR));
      UNIQUE_ASTR: {$IFNDEF NEXTGEN}UniqueString(AnsiString(Dst.PTR)){$ENDIF};
      //==================================================================================================
      // ETHROW
      //==================================================================================================
      ETHROW: _VM_ETHROW(FMem, Dst.PTR);
      SCHECKB: _VM_SCHECKB(Dst^, PR);
      DCHECKB: _VM_DCHECKB(Dst, Src1);
      ACHECKB: _VM_ACHECKB(Dst, Src1);
      //==================================================================================================
      // ARRAY_LENGTH
      //==================================================================================================
      ARRAY_LENGTH: if Assigned(Src1.PTR) then Dst.U64 := PNUInt(PByte(Src1.PTR) - PTR_SIZE)^ else Dst.U64 := 0;
      {$IFDEF CPUX64}STR_LENGTH: if Assigned(Src1.PTR) then Dst.U64 := PUInt32(PByte(Src1.PTR) - 4)^ else Dst.U64 := 0;{$ENDIF}
      //==================================================================================================
      ARRAY_INCREF: _VM_ARRAY_INCREF(Dst.PTR);
      ARRAY_DECREF: if Idx = 0 then _VM_ARRAY_DECREF(Dst) else _VM_ARRAY_DECREF_FINAL(Dst, StackSize, GP, SP, PR);
      {$IFDEF CPUX64}STR_INCREF: _VM_STR_INCREF(Dst.PTR);{$ENDIF}
      STR_DECREF: _VM_STR_DECREF(Dst.PTR);
      //==================================================================================================
      DINF_INCREF: if Dst.PTR <> nil then IInterface(Dst.PTR)._AddRef;
      DINF_DECREF: if Dst.PTR <> nil then IInterface(Dst.PTR)._Release;
      //==================================================================================================
      WEAK_INCREF: _VM_WEAK_INCREF(Dst.PTR);
      WEAK_DECREF: _VM_WEAK_DECREF(Dst.PTR);
      //==================================================================================================
      OBJ_CREATE: Dst.PTR := _VM_OBJ_CREATE(PR);
      //==================================================================================================
      OBJ_INCREF: _VM_OBJ_INCREF(Dst.PTR);
      OBJ_DECREF: _VM_OBJ_DECREF(Dst.PTR, StackSize, GP, SP, PR);
      //==================================================================================================
      OBJ_WEAKREF: begin _VM_OBJ_WEAKREF(Dst.PTR); continue; end;
      OBJ_STRONGREF: begin _VM_OBJ_STRONGREF(Dst.PTR); continue; end;
      //==================================================================================================
      // ALLOC
      //==================================================================================================
      VIRT_METHOD_PTR: _VM_GET_METHOD_PTR(Dst, Src1, PR, GP);
      VM_SET_METHOD_PTR: begin
        PILMethod(Dst.PTR).Proc := Src2.PTR;
        PILMethod(Dst.PTR).Self := Src1.PTR;
      end;
      MEM_ALLOC: begin
        Dst.PTR := GetMemory(PR^);
        FillChar(Dst.PTR^, PR^, #0);
      end;
      MEM_FREE: FreeMemory(Dst.PTR);
      ARRAY_MEM_ALLOC: _VM_DYNARRAY_CREATE(Dst, Src1, PR^);
      STRU_CREATE: Dst.PTR := _VM_STRU_CREATE(Src1.U32);
      STRA_CREATE: Dst.PTR := _VM_STRA_CREATE(Src1.U32);
      ARRAY_INIT: begin
        PInt32(PByte(Dst.PTR) + STR_REC_PADDING)^ := 1;                // refcount = 1
        PNUInt(PByte(Dst.PTR) + STR_REC_PADDING + 4)^ := UInt32(PR^);  // length
        Dst.PTR := PByte(Dst.PTR) + 4 + PTR_SIZE + STR_REC_PADDING;    // adjust the pointer
      end;
      //==================================================================================================
      // MOVE_MEM
      //==================================================================================================
      MOVE_MEM_C: Move(Src1.PTR^, Dst.PTR^, PR^);
      MOVE_MEM: Move(Src1.PTR^, Dst.PTR^, Src2.U32);
      MOVE_ARRAY: if Assigned(Src1.PTR) then Move(Src1.PTR^, Dst.PTR^, PUInt32(PByte(Src1.PTR) - PTR_SIZE)^ * PR^);
      {$IFDEF CPUX64}STR_MOVE: if Assigned(Src1.PTR) then Move(Src1.PTR^, Dst.PTR^, PUInt32(PByte(Src1.PTR) - 4)^ * PR^);{$ENDIF}
      //==================================================================================================
      // CONVERT
      //==================================================================================================
      CNV_F64_S32: Dst.F64 := Dst.I32;
      CNV_F64_U32: Dst.F64 := Dst.U32;
      CNV_F64_S64: Dst.F64 := Dst.I64;
      CNV_F64_U64: Dst.F64 := Dst.U64;
      CNV_ACHR_TO_ASTR: begin
        PR := _VM_STRA_CREATE(1);
        PAnsiChar(PR)^ := AnsiChar(Src1.U8);
        Dst.PTR := PR;
      end;
      CNV_UCHR_TO_USTR: begin
        PR := _VM_STRU_CREATE(1);
        PUChar(PR)^ := Char(Src1.U16);
        Dst.PTR := PR;
      end;
      CNV_ASTR_TO_USTR: Dst.PTR := _STRU_FROM_STRA(Src1);
      CNV_USTR_TO_ASTR: Dst.PTR := _STRA_FROM_STRU(Src1);
      // Variants
      VAR_RELEASE: _VAR_RELEASE(Dst);
      CNV_VAR_TO_VALUE: _CNV_FROM_VARIANT(Dst, Src1, TDataTypeID(PR^));
      CNV_VALUE_TO_VAR: _CNV_TO_VARIANT(Dst, Src1, TDataTypeID(PR^));
      //==================================================================================================
      // CALL обычный вызов процедуры
      //==================================================================================================
      CALL_PROC, CALL_NEAR: begin
        // вычитываем из инструкции вызова размер стека вызывающей процедуры
        // Instruction используется как StackSize
        Instruction := PNInt(PByte(PR) + PTR_SIZE)^;
        // по смещению StackSize сохраняем указатель на свой стек
        PPtr(SP + Instruction)^ := SP;
        // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
        PPtr(SP + Instruction + PTR_SIZE)^ := PByte(PR) + PTR_SIZE*2;
        // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
        SP := SP + Instruction + PTR_SIZE*2;
        // загружаем указатель на код вызываемой процедуры
        MP := PNativeUInt(GP + PR^);
        if Assigned(FOnVMCall) then
          FOnVMCall(SP, PR^);
      end;
      //==================================================================================================
      // CALL вызов виртуального метода
      //==================================================================================================
      CALL_VIRT: begin
        MP := _VM_PROC_CALL_VIRTUAL(Dst.PTR, SP, PR, GP);
        if Assigned(FOnVMCall) then
          FOnVMCall(SP, NativeUInt(MP) - NativeUInt(FMem.Memory));
      end;
      CALL_INTF: begin
        MP := _VM_PROC_CALL_INTERFACE(Dst.PTR, SP, PR, GP);
        if Assigned(FOnVMCall) then
          FOnVMCall(SP, NativeUInt(MP) - NativeUInt(FMem.Memory));
      end;
      //==================================================================================================
      // CALL косвенный вызов процедуры
      //==================================================================================================
      CALL_INDIRECT: begin
        // вычитываем из инструкции вызова размер стека вызывающей процедуры
        // Instruction используется как StackSize
        Instruction := PR^;
        // по смещению StackSize сохраняем указатель на свой стек
        PPtr(SP + Instruction)^ := SP;
        // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
        PPtr(SP + Instruction + PTR_SIZE)^ := PByte(PR) + PTR_SIZE;
        // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
        SP := SP + Instruction + PTR_SIZE*2;
        // загружаем указатель на код вызываемой процедуры
        MP := Dst.PTR;
        if Assigned(FOnVMCall) then
          FOnVMCall(SP, NativeUInt(MP) - NativeUInt(FMem.Memory));
      end;
      //==================================================================================================
      CALL_EXT_FAST: InvokeExternalStatic(FHeader, PR, SP);
      CALL_EXT_FASTV: InvokeExternalVirtual(FHeader, PR, SP);
      CALL_EXT_FAST_INTF_PROC: InvokeExternalInterfaceMethod(FHeader, PR, SP);
      CALL_EXT_FAST_INTF_FUNC: InvokeExternalInterfaceFunc(FHeader, PR, SP);
      CALL_EXT_COMMON_PROC: InvokeExternalCommonProc(FHeader, PR, SP);
      CALL_EXT_COMMON_FUNC: InvokeExternalCommonFunc(FHeader, PR, SP);
      //==================================================================================================
      VM_SYSMACRO: _VM_SYSMACRO(TVMMacroID(PR^), Dst, Src1, Src2, SP);
      //==================================================================================================
      // JUMP
      //==================================================================================================
      VM_JMP: MP := PNativeUInt(GP + PR^);
      //==================================================================================================
      // RET
      //==================================================================================================
      PROC_RET: begin
        if Assigned(FOnVMRet) then
          FOnVMRet(SP);
        MP := PNativeUInt(PNativeUInt(SP - PTR_SIZE)^);   // адрес вызывающей инструкции
        if MP <> nil then begin
          NewSP := PByte(PNativeUInt(SP - PTR_SIZE*2)^);  // сохраненный размер стека вызывающей процедуры
          StackSize := SP - NewSP - PTR_SIZE*2;
          SP := NewSP;
          {$IFDEF DEBUG}
          if SP = nil then
            raise Exception.Create('Corrupt stack detected');
          {$ENDIF}
          Continue;
        end;
        Exit; // если указатель на память равен nil значит это был корневой вызов, выходим
      end;
      //==================================================================================================
    else
      Instruction := Word(Instruction);
      raise Exception.CreateFmt('Unknown VM code: %s (%d)', [TEnum<TVMCode>.Name(TVMCode(Instruction)), Instruction]);
    end;
  end;
end;

constructor TILMachine.Create(Stack: PByte; StackSize: UInt32; FreeStackWhenDestroy: Boolean);
begin
  FMem := TILMemoryStream.Create;
  FStack := Stack;
  FStackSize := StackSize;
  FFreeStackWhenDestroy := FreeStackWhenDestroy;
end;

constructor TILMachine.Create(StackSize: UInt32);
var
  Stack: PByte;
begin
  Stack := GetMemory(StackSize);
  {$IFDEF DEBUG}
  FillChar(Stack^, StackSize, #01);
  {$ENDIF}
  Create(Stack, StackSize, True);
end;

destructor TILMachine.Destroy;
begin
  FMem.Free;
  if FFreeStackWhenDestroy then
    FreeMemory(FStack);
  inherited;
end;

function TILMachine.FindTypeProc(Struct: PRTTIStruct; const MethodName: string): PVMExportProc;
var
  i: Integer;
  M: PVMExportProc;
  Name: string;
  Methods: PVMExportProcs;
begin
  Methods := GetRTTIPtr(Struct.Methods);
  for i := 0 to Struct.MethodsCount - 1 do
  begin
    M := @(Methods[i]);
    Name := GetString(GetIMGPtr(M.Name));
    if CompareText(Name, MethodName) = 0 then
      Exit(M);
  end;
  if Struct.Ancestor <> 0 then
    Result := FindTypeProc(PRTTIStruct(Struct.Ancestor), MethodName)
  else
    Result := nil;
end;

function TILMachine.FindMethod(const TypeName, MethodName, UnitName: string): PVMExportProc;
var
  T: PRTTIType;
  AncTypeName: string;
begin
  T := FindType(TypeName, UnitName);
  if not Assigned(T) then
    Exit(nil);

  if not (T.DataTypeID in [dtRecord, dtClass]) then
    Exit(nil);

  Result := FindProc(TypeName + '.' + MethodName, UnitName);
  while (not Assigned(Result) and (PRTTIStruct(T).Ancestor <> 0)) do
  begin
    T := PRTTIType(PRTTIStruct(T).Ancestor);
    AncTypeName := GetTypeName(T);
    Result := FindProc(AncTypeName + '.' + MethodName, UnitName);
  end;
end;

function TILMachine.FindProc(const ProcName: string; const UnitName: string = ''): PVMExportProc;
var
  i, j: Integer;
  UN: PVMUnit;
  Proc: PVMExportProc;
  Name: string;
begin
  for i := 0 to FUnitsCount - 1 do
  begin
    UN := @(FUnits[i]);
    Name := GetUnitName(UN);
    if (UnitName <> '') and (Name <> UnitName) then
      continue;
    for j := 0 to UN.ExportProcsCount - 1 do begin
      Proc := @(UN.ExportProcs[j]);
      Name := GetProcName(Proc);
      if CompareText(Name, ProcName) = 0 then
        Exit(Proc);
    end;
  end;
  Result := nil;
end;

function TILMachine.FindType(const TypeName, UnitName: string): PRTTIType;
var
  ui, ti: Integer;
  UN: PVMUnit;
  Item: PRTTIType;
  Name: string;
begin
  for ui := 0 to FUnitsCount - 1 do
  begin
    UN := @(FUnits[ui]);
    if (UnitName <> '') and (GetUnitName(UN) <> UnitName) then
      continue;

    for ti := 0 to UN.TypesCount - 1 do begin
      Item := PRTTIType(PByte(PPtrSArray(UN.FTypes)[ti]) - Sizeof(TVMObjHeader));
      Name := GetTypeName(Item);
      if CompareText(Name, TypeName) = 0 then
        Exit(Item);
    end;
  end;
  Result := nil;
end;

function TILMachine.FindVar(const VarName: string; const UnitName: string = ''): PVMVariable;
var
  i, j: Integer;
  UN: PVMUnit;
  VarInfo: PVMVariable;
  Name: string;
begin
  for i := 0 to FUnitsCount - 1 do
  begin
    UN := @(FUnits[i]);
    if (UnitName <> '') and (GetUnitName(UN) <> UnitName) then
      continue;
    for j := 0 to UN.FVarsCount - 1 do begin
      VarInfo := @(UN.FVars[j]);
      Name := GetVarName(VarInfo);
      if CompareText(Name, VarName) = 0 then
        Exit(VarInfo);
    end;
  end;
  Result := nil;
end;

procedure TILMachine.LoadVMImage(Stream: TStream);
const
  SUPPORTED_IMG_FORMAT: TIMGFormat = {$IF PTR_SIZE = 4} IMG_32Bit {$ELSE} IMG_64Bit {$ENDIF};
var
  ui, ti, pi: Integer;
  UN: PVMUnit;
  pType: PRTTIType;
  pVar: PVMVariable;
  M: PByte;
  IMGSign: UInt32;
  IMGFlags: UInt32;
  IMGFormat: TIMGFormat;
  IMGSize: UInt32;
  RTTISize: Integer;
  ProcEntry: ^TVMIMGProcFrame;
  Procs: PVMIMGProcFrames;
  TypesRTTIArray: PPtrSArray;
begin
  FMem.Size := 0;
  IMGSign := Stream.ReadUInt32;
  if IMGSign <> VM_IMG_SIGNATURE_V1 then
    raise Exception.Create('Image signature is invalid');

  IMGFlags := Stream.ReadUInt32;
  IMGFormat := TIMGFormat(IMGFlags and 3);
  if IMGFormat <> SUPPORTED_IMG_FORMAT then
    raise Exception.CreateFmt('Image version %s is not supported', [ifthen(IMGFormat = IMG_32Bit, 'X86', 'X86-64')]);

  IMGSize := Stream.ReadUInt32;
  if IMGSize <> 0 then; // hint remove
  FMem.Size := Stream.Size;
  M := FMem.Memory;
  Stream.Position := 0;
  FMem.CopyFrom(Stream, Stream.Size);
  FMem.Position := 0;
  FHeader := PIMGHeader(M);
  if FHeader.ImportTableCount > 0 then
  begin
    FHeader.ImportTable := NativeUInt(M + FHeader.ImportTable);
    MapImportProcedures;
  end;

  {коррекция списка всех модулей образа}
  FRttiUnits := GetIMGPtr(FHeader.UnitsOffset);
  FUnitsCount := PNUInt(GetIMGPtr(FHeader.UnitsOffset - PTR_SIZE))^;
  for ui := 0 to FUnitsCount - 1 do
    FRttiUnits[ui] := GetIMGPtr(FRttiUnits[ui]);

  FMem.Position := FHeader.UnitsOffset + PTR_SIZE*FUnitsCount;
  FUnits := FMem.MemoryPosition;

  if FUnitsCount = 0 then
    raise Exception.Create('Invalid package (Units count = 0)');

  {коррекция списка всех процедур образа}
  if FHeader.ProcFramesCount > 0 then
  begin
    FHeader.ProcFramesSection := NativeUInt(M + FHeader.ProcFramesSection);
    Procs := PVMIMGProcFrames(FHeader.ProcFramesSection);
    for pi := 0 to FHeader.ProcFramesCount - 1 do
    begin
      ProcEntry := @(Procs[pi]);
      ProcEntry.ProcAddr := GetIMGPtr(ProcEntry.ProcAddr);
      if Assigned(ProcEntry.ProcInfo) then
        ProcEntry.ProcInfo := M + FHeader.RTTIOffset + NativeUInt(ProcEntry.ProcInfo) + sizeof(TVMObject);
    end;
  end;

  // коррекция RTTI системных типов
  if Check(IMGFlags, IMG_HAS_RTTI) then
  begin
    pType := GetRTTIPtr(4);
    for ti := 0 to TSystemTypes.Count - 1 do
    begin
      pType.TypeInfo := GetRTTIPtr(pType.TypeInfo);
      pType.Name := NativeUInt(cDataTypeNames[pType.DataTypeID]);
      RTTISize := GetRTTIStructSize(pType.DataTypeID);
      pType := PRTTIType(PByte(pType) + RTTISize);
    end;
  end; {}

  // коррекция модулей
  for ui := 0 to FUnitsCount - 1 do
  begin
    UN := @(FUnits[ui]);

    if Assigned(UN.TypeInfo) then
      UN.TypeInfo := PRTTIType(GetRTTIPtr(UN.TypeInfo));

    if UN.Name > 0 then
      UN.Name := NativeUInt(GetIMGPtr(UN.Name));

    if Assigned(UN.FVars) then
      UN.FVars := PVMVariables(GetIMGPtr(UN.FVars));

    if Assigned(UN.FExportProcs) then
      UN.FExportProcs := PVMExportProcs(GetRTTIPtr(UN.FExportProcs));

    // коррекция RTTI типов
    if UN.FTypes > 0 then
    begin
      UN.FTypes := NativeUInt(GetRTTIPtr(UN.FTypes));

      TypesRTTIArray := PPtrSArray(UN.FTypes);
      for ti := 0 to UN.TypesCount - 1 do
      begin
        pType := GetRTTIPtr(NativeUInt(TypesRTTIArray[ti]) - Sizeof(TVMObjHeader));
        TypesRTTIArray[ti] := GetRTTIPtr(TypesRTTIArray[ti]);

        if Check(IMGFlags, IMG_HAS_RTTI) then
          pType.TypeInfo := GetRTTIPtr(pType.TypeInfo);

        if pType.Name > 0 then
          pType.Name := NativeUInt(M + pType.Name);

        if pType.DataTypeID in [dtRecord, dtClass, dtInterface] then
        begin
          if PRTTIStruct(pType).Ancestor > 0 then
            PRTTIStruct(pType).Ancestor := NativeUInt(GetRTTIPtr(PRTTIStruct(pType).Ancestor));

          if PRTTIStruct(pType).Methods > 0 then
          begin
            PRTTIStruct(pType).Methods := NativeUInt(GetRTTIPtr(PRTTIStruct(pType).Methods));
            for pi := 0 to PRTTIStruct(pType).MethodsCount - 1 do
            begin
              // todo
              // Method := addr(PRTTIProcedures(PRTTIStruct(pType).Methods)[pi]);
              // Method.Name := NativeUInt(M + Method.Name); не работает!
            end;
          end;
        end;
      end;
    end;

    // коррекция глобальных переменных
    for ti := 0 to UN.VarsCount - 1 do
    begin
      pVar := @(UN.FVars[ti]);
      if pVar.Name > 0 then
        pVar.Name := NativeUInt(GetIMGPtr(pVar.Name));

      pVar.Addr := GetIMGPtr(pVar.Addr);
      pVar.DataType := GetRTTIPtr(pVar.DataType);
      // инициализация значений по умолчанию дин. массивов и строк
      if (PNativeUInt(pVar.Addr)^ > 0) and
         (pVar.DataType.DataTypeID in [dtAnsiString, dtString]) then
        PNativeUInt(pVar.Addr)^ := NativeUInt(M + PNativeUInt(pVar.Addr)^);
    end;
    // подготавливаем процедуры
    PrepareProcs(UN);

    // подготавливаем экспортные процедуры
    PrepareExportProcs(UN);
  end;

  if FHeader.FixTable > 0 then
  begin
    FHeader.FixTable := NativeUInt(M + FHeader.FixTable);
    PrepareFixTable(M);
  end;
end;

procedure SetArrRefCnt(Arr: Pointer; RefCnt: Integer);
var
  PRefCnt: PInteger;
begin
  PRefCnt := PInteger(PByte(Arr) - PTR_SIZE - Sizeof(Int32));
  PRefCnt^ := RefCnt;
end;

procedure TILMachine.PrepareExportProcs(UN: PVMUnit);
var
  M: PByte;
  pi, vi: Integer;
  EProc: PVMExportProc;
  Param: PVMParam;
  EProcs: TVMExportProcsArray;
  EProcParams: TVMParamsArray;
begin
  if UN.FExportProcs = nil then
    Exit;
  M := FMem.Memory;
  // коррекция экспортных процедур
  SetArrRefCnt(UN.FExportProcs, 1); // убираем копирование дин. массива
  EProcs := TVMExportProcsArray(UN.FExportProcs);
  for pi := 0 to Length(EProcs) - 1 do
  begin
    EProc := @(EProcs[pi]);
    if EProc.Name > 0 then
      EProc.Name := NativeUInt(M + EProc.Name);
    EProc.Offset := NativeUInt(M + EProc.Offset);

    if EProc.Struct > 0 then
      EProc.Struct := NativeUInt(M + FHeader.RTTIOffset + EProc.Struct);

    if EProc.Params <> nil then
    begin
      EProc.Params := PVMParams(M + FHeader.RTTIOffset + NativeUInt(EProc.Params));
      SetArrRefCnt(EProc.Params, 1); // убираем копирование дин. массива
      EProcParams := TVMParamsArray(EProc.Params);
      for vi := 0 to Length(EProcParams) - 1 do
      begin
        Param := @(EProc.Params[vi]);
        if Param.Name > 0 then
          Param.Name := NativeUInt(M + Param.Name);
        if Assigned(Param.DataType) then
          Param.DataType := PRTTIType(M + FHeader.RTTIOffset + NativeUInt(Param.DataType));
      end;
      SetArrRefCnt(EProc.Params, -1);
    end;
  end;
  SetArrRefCnt(UN.FExportProcs, -1);
end;

procedure TILMachine.PrepareFixTable(BasePtr: PByte);
var
  cnt: Integer;
  Ptr: PInt32;
  FixedPtr: PNativeUInt;
begin
  // get the count of the FIX table
  Ptr := PInt32(FHeader.FixTable);
  cnt := Ptr^;
  Inc(Ptr);
  while Cnt > 0 do begin
    // Get the next fixed memory pointer
    FixedPtr := PNativeUInt(BasePtr + Ptr^);
    // Write the correct value to it
    FixedPtr^ := NativeUInt(BasePtr + FixedPtr^);
    Dec(Cnt);
    Inc(Ptr);
  end;
end;

procedure TILMachine.PrepareProcLocalVars(Proc: PRTTIProcedure);
var
  i: Integer;
  LVars: TRTTILocalVars;
  LVar: PRTTILocalVar;
begin
  if Proc.LocalVars = 0 then
    Exit;
  Proc.LocalVars := TOffset(GetRTTIPtr(Proc.LocalVars));

  SetArrRefCnt(Pointer(Proc.LocalVars), 1);
  LVars := TRTTILocalVars(Proc.LocalVars);
  for i := 0 to Length(LVars) - 1 do
  begin
    LVar := @(LVars[i]);
    LVar.Name := TOffset(GetIMGPtr(LVar.Name));
    LVar.DataType := TOffset(GetRTTIPtr(LVar.DataType));
  end;
  SetArrRefCnt(Pointer(Proc.LocalVars), -1);
end;

procedure TILMachine.PrepareProcs(UN: PVMUnit);
var
  pi: Integer;
  Proc: PRTTIProcedure;
  Str: string;
begin
  // коррекция секции initialization
  if UN.InitProc > 0 then
    UN.FInitProc := TOffset(GetIMGPtr(UN.InitProc));

  // коррекция секции finalization
  if UN.FinalProc > 0 then
    UN.FFinalProc := TOffset(GetIMGPtr(UN.FinalProc));

  if not Assigned(UN.FProcs) then
    Exit;

  UN.FProcs := PRTTIProcedures(GetRTTIPtr(UN.FProcs));
  for pi := 0 to UN.ProcsCount - 1 do
  begin
    Proc := GetRTTIPtr(PPtrSArray(UN.FProcs)[pi]);
    PPtrSArray(UN.FProcs)[pi] := PByte(Proc) + SizeOf(TVMObjHeader);

    if Assigned(Proc.TypeInfo) then
      Proc.TypeInfo := GetRTTIPtr(Proc.TypeInfo);

    Proc.ADDR := NativeUInt(GetIMGPtr(Proc.ADDR));
    if Proc.Name > 0 then
      Proc.Name := NativeUInt(GetIMGPtr(Proc.Name))
    else begin
      if Proc.ADDR = UN.InitProc then
      begin
        Str := '$initialization';
        Proc.Name := NativeUint(Str);
      end else
      if Proc.ADDR = UN.FinalProc then
      begin
        Str := '$finalization';
        Proc.Name := NativeUint(Str);
      end;
    end;
    // коррекция локальных переменных
    PrepareProcLocalVars(Proc);
  end;
end;

procedure TILMachine.MapImportProcedures;
var
  i: Integer;
  ImportTable: PImportTable;
  ImportProc: PImportEntry;
  Ptr: Pointer;
  LibName, ProcName, StructName: string;
  RegType: TTypeRegInfo;
begin
  ImportTable := PImportTable(FHeader.ImportTable);
  for i := 0 to FHeader.ImportTableCount - 1 do
  begin
    ImportProc := @ImportTable[i];
    LibName := GetImportProcLib(ImportProc);
    ProcName := GetImportProcName(ImportProc);

    if ImportProc.Struct = 0 then
    begin
      // если это глобальная процедура, ищем в списке глобальных процедур
      Ptr := VM.Invoke.FindProc(LibName, ProcName);
      if not Assigned(Ptr) then
        raise Exception.CreateFmt('Import proc "%s" is not found in "%s" library', [ProcName, LibName]);
    end else begin
      // если это метод структуры/класса, ищем в списке типов
      if not ImportProc.IsIntfMethod then
      begin
        StructName := GetString(GetIMGPtr(ImportProc.Struct));
        RegType := VM.Invoke.FindType(LibName, StructName);
        if not Assigned(RegType) then
          raise Exception.CreateFmt('Import type "%s" is not found in "%s" library', [StructName, LibName]);
        Ptr := RegType.FindMethod(ProcName);
        if not Assigned(Ptr) then
          raise Exception.CreateFmt('Import method "%s" is not found in "%s" library', [ProcName, LibName]);
      end else
        Ptr := Pointer(ImportProc.ADDR);
    end;
    ImportProc.ADDR := TOffset(Ptr);

    Ptr := VM.Invoke.GetInvokeAdapterPtr(ImportProc.Adapter);
    {$IFDEF USE_ONLY_FAST_INVOKE}
    if not Assigned(Ptr) then
      raise Exception.CreateFmt('Unknown invoke adapter: %d', [ImportProc.Adapter]);
    {$ENDIF}
    ImportProc.Adapter := TOffset(Ptr);
  end;
end;

procedure TILMachine.Run(StackSize: Integer);
var
  Stack: PByte;
begin
  Stack := GetMemory(StackSize);
  try
    {$IFDEF DEBUG} FillChar(Stack^, SizeOf(StackSize), #0);{$ENDIF}
    RunInitSections(Stack);
    RunFinalSections(Stack);
  finally
    FreeMemory(Stack);
  end;
end;

procedure TILMachine.Run;
begin
  RunInitSections(FStack);
  RunFinalSections(FStack);
end;

procedure TILMachine.DebugRun;
begin
  DebugRunInitSections(FStack);
  DebugRunFinalSections(FStack);
end;

procedure TILMachine.RunProc(Proc: PVMExportProc);
begin
  if not Assigned(Proc) then
    Exit;
  CallVMProc(PNativeUInt(Proc.Offset), FStack, FMem.Memory);
end;

procedure TILMachine.RunProc(Proc: PVMExportProc; Param0: Int32);
var
  Stack: PByte;
begin
  if not Assigned(Proc) then
    Exit;

  Stack := FStack;
  PInt32(Stack)^ := Param0;

  CallVMProc(PNativeUInt(Proc.Offset), Stack, FMem.Memory);
end;

procedure CheckVarParam(const Variable: PVMVariable); inline;
begin
  if not Assigned(Variable) then
    raise Exception.Create('Parameter must be assigned');
end;

procedure TILMachine.SetVarAsBool(const Variable: PVMVariable; const Value: boolean);
begin
  CheckVarParam(Variable);
  PBoolean(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsFloat32(const Variable: PVMVariable; const Value: Float32);
begin
  CheckVarParam(Variable);
  PFlt32(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsFloat64(const Variable: PVMVariable; const Value: Float64);
begin
  CheckVarParam(Variable);
  PFlt64(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsInt16(const Variable: PVMVariable; const Value: Int16);
begin
  CheckVarParam(Variable);
  PInt16(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsInt32(const Variable: PVMVariable; const Value: Int32);
begin
  CheckVarParam(Variable);
  PInt32(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsInt64(const Variable: PVMVariable; const Value: Int64);
begin
  CheckVarParam(Variable);
  PInt64(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsInt8(const Variable: PVMVariable; const Value: Int8);
begin
  CheckVarParam(Variable);
  PInt8(Variable.Addr)^ := Value;
end;

procedure TILMachine.SetVarAsPointer(const Variable: PVMVariable; const Value: Pointer);
begin
  CheckVarParam(Variable);
  PPointer(Variable.Addr)^ := Value;
end;

procedure TILMachine.RunFunc(Proc: PVMExportProc; Param0: NativeUInt; ResultDataType: TDataTypeID; Result: Pointer);
var
  Stack: PByte;
begin
  if not Assigned(Proc) then
    Exit;

  Stack := FStack;

  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);

  // местро в стеке под результат
  PPtr(Stack)^ := nil;
  // первый параметр
  PNativeUInt(Stack + PTR_SIZE)^ := Param0;

  CallVMProc(PNativeUInt(Proc.Offset), Stack, FMem.Memory);

  ReadResultFromStack(Stack, ResultDataType, Result);
end;

procedure TILMachine.RunFunc(Proc: PVMExportProc; Param0, Param1: NativeUInt; ResultDataType: TDataTypeID; Result: pointer);
var
  Stack: PByte;
begin
  if not Assigned(Proc) then
    Exit;

  Stack := FStack;

  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);

  // местро в стеке под результат
  PPtr(Stack)^ := nil;

  // первый параметр
  PNativeUInt(Stack + PTR_SIZE)^ := Param0;
  // второй параметр
  PNativeUInt(Stack + PTR_SIZE*2)^ := Param1;

  CallVMProc(PNativeUInt(Proc.Offset), Stack, FMem.Memory);

  ReadResultFromStack(Stack, ResultDataType, Result);
end;

procedure TILMachine.RunFunc(const ProcName: string; const Params: array of Variant; out Result: Variant; StackSize: Integer);
var
  Proc: PVMExportProc;
  Stack, CAddr: PByte;
begin
  Proc := FindProc(ProcName);
  if not Assigned(Proc) then
    raise Exception.CreateFmt('Func name %s not found', [ProcName]);

  Stack := GetMemory(StackSize);
  try
    {$IFDEF DEBUG} FillChar(Stack^, SizeOf(StackSize), #0);{$ENDIF}
    Stack := FMem.MemoryPosition;
    PPtr(Stack)^ := nil; // адрес возврата
    Inc(Stack, PTR_SIZE);

    // местро в стеке под результат
    PPtr(Stack)^ := nil;
    WriteParamsToStack(Stack + PTR_SIZE, Params);

    CAddr := PByte(Proc.Offset);
    CallVMProc(PNativeUInt(CAddr), Stack, FMem.Memory);
  finally
    FreeMemory(Stack);
  end;
end;

procedure TILMachine.RunInitSections(Stack: PByte);
var
  i: Integer;
  P: TOffset;
begin
  if not Assigned(Stack) then
    Stack := FStack;
  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);

  for i := 0 to FUnitsCount - 1 do
  begin
    P := FUnits[i].InitProc;
    if P = 0 then
      continue;
    CallVMProc(PNativeUInt(P), Stack, FMem.Memory);
  end;
end;

procedure TILMachine.RunFinalSections(Stack: PByte);
var
  i: Integer;
  P: TOffset;
begin
  if not Assigned(Stack) then
    Stack := FStack;
  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);

  for i := FUnitsCount - 1 downto 0 do
  begin
    P := FUnits[i].FinalProc;
    if P = 0 then
      continue;
    CallVMProc(PNativeUInt(P), Stack, FMem.Memory);
  end;
end;

procedure TILMachine.DebugRunFinalSections(Stack: PByte);
var
  i: Integer;
  P: TOffset;
begin
  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);
  // местро в стеке под результат
  PPtr(Stack)^ := nil;

  for i := 0 to FUnitsCount - 1 do
  begin
    P := FUnits[i].FinalProc;
    if P = 0 then
      continue;
    DebugCallILProc(PNativeUInt(P), Stack, FMem.Memory);
  end;
end;

procedure TILMachine.DebugRunInitSections(Stack: PByte);
var
  i: Integer;
  P: TOffset;
begin
  PPtr(Stack)^ := nil; // адрес возврата
  Inc(Stack, PTR_SIZE);
  // местро в стеке под результат
  PPtr(Stack)^ := nil;

  for i := FUnitsCount - 1 downto 0 do
  begin
    P := FUnits[i].InitProc;
    if P = 0 then
      continue;
    DebugCallILProc(PNativeUInt(P), Stack, FMem.Memory);
  end;
end;

procedure TILMachine.RunProc(const ProcName: string; const Params: array of Variant; StackSize: Integer);
var
  Proc: PVMExportProc;
  Stack, P, CAddr: PByte;
begin
  Proc := FindProc(ProcName);
  if not Assigned(Proc) then
    raise Exception.CreateFmt('Proc name %s not found', [ProcName]);

  P := GetMemory(StackSize);
  Stack := P;
  try
    {$IFDEF DEBUG} FillChar(Stack^, SizeOf(StackSize), #0);{$ENDIF}
    PPtr(Stack)^ := nil; // адрес возврата
    Inc(Stack, SizeOf(Pointer));

    WriteParamsToStack(Stack, Params);

    CAddr := PByte(Proc.Offset);
    CallVMProc(PNativeUInt(CAddr), Stack, FMem.Memory);
  finally
    FreeMemory(P);
  end;
end;

procedure TILMachine.WriteParamsToStack(Stack: PByte; const Params: array of Variant);
var
  i: Integer;
  Param: PVarData;
begin
  for i := 0 to Length(Params) - 1 do
  begin
    Param := PVarData(@Params[i]);
    case Param.VType of
      varInteger, varBoolean, varByte, varLongWord, varShortInt, varWord:
      begin
        PInt32(Stack)^ := Param.VInteger;
        Inc(Stack, SizeOf(Integer));
      end;
      varDouble, varDate: begin
        PFlt64(Stack)^ := Param.VDouble;
        Inc(Stack, SizeOf(Float64));
      end;
      varInt64, varUInt64: begin
        PInt64(Stack)^ := Param.VInt64;
        Inc(Stack, SizeOf(Int64));
      end;
      varByRef, {$IFNDEF FPC}varObject, {$ENDIF} varUnknown: begin
        PPtr(Stack)^ := Param.VPointer;
        Inc(Stack, SizeOf(Pointer));
      end;
    else
      raise Exception.Create('Parameter data type is not supported');
      {  vtString        = 4;
          vtClass         = 8;
          vtWideChar      = 9;
          vtPWideChar     = 10;
          vtAnsiString    = 11;
          vtCurrency      = 12;
          vtVariant       = 13;
          vtWideString    = 15;
          vtUnicodeString = 17;}
    end;
  end;
end;

function TILMachine.GetRTTICharset: TRTTICharset;
begin
  Result := TRTTICharset((FHeader.IMGFlags shr 2) and 3);
end;

function TILMachine.GetString(const Ptr: Pointer): string;
begin
  if Ptr = nil then
    Exit('');
  case GetRTTICharset of
    RTTICharsetASCII: Result := string(AnsiString(Ptr));
    RTTICharsetUTF16: Result := string(Ptr);
  else
    Result := '';
  end;
end;

function TILMachine.GetTypeName(const pType: PRTTIType): string;
begin
  try
    if not Assigned(pType) then
      Exit('');
    if pType.DataTypeID <= dtVariant then
      Result := GetDataTypeName(pType.DataTypeID)
    else begin
      if (pType.DataTypeID = dtPointer) and (PRTTIPointer(pType).RefTypeInfoOffset = 0) then
        Result := 'Pointer'
      else
        Result := GetString(PByte(pType.Name));
    end;
  except
    Result := '<error>';
  end;
end;

function TILMachine.GetRTTIPtr(const Offset: TOffset): Pointer;
begin
  Result := PByte(FMem.Memory) + FHeader.RTTIOffset + Offset;
end;

function TILMachine.GetRTTIPtr(const Offset: Pointer): Pointer;
begin
  Result := PByte(FMem.Memory) + FHeader.RTTIOffset + NativeUInt(Offset);
end;

function TILMachine.GetIMGPtr(const Offset: Integer): Pointer;
begin
  Result := PByte(FMem.Memory) + Offset;
end;

function TILMachine.GetIMGPtr(const Offset: Pointer): Pointer;
begin
  Result := PByte(FMem.Memory) + NativeUInt(Offset);
end;

function TILMachine.GetImportProcLib(ImportProc: PImportEntry): string;
begin
  Result := GetString(GetIMGPtr(ImportProc.LibName));
end;

function TILMachine.GetImportProcName(ImportProc: PImportEntry): string;
begin
  Result := GetString(GetIMGPtr(ImportProc.Name));
end;

function TILMachine.GetInitProcOffset(pUnit: PVMUnit): NativeUInt;
begin
  if pUnit.InitProc > 0 then
    Result := pUnit.InitProc - NativeUInt(FMem.Memory)
  else
    Result := 0;
end;

function TILMachine.GetLocalVarName(const LVar: PRTTILocalVar): string;
begin
  Result := GetString(Pointer(LVar.Name));
end;

function SearchProc(const Values: PVMIMGProcFrames; const Ptr: Pointer; out FoundIndex: Integer; Count: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
  LastState: (ssNone, sLess, sGreater);
begin
  Result := False;
  L := 0;
  H := Count - 1;
  LastState := ssNone;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := NativeInt(Values[mid].ProcAddr) - NativeInt(Ptr);
    if cmp < 0 then
    begin
      if L = H then
      begin
        FoundIndex := L;
        Exit;
      end;
      L := mid + 1;
      LastState := sGreater;
    end else
    if cmp = 0 then
    begin
      FoundIndex := mid;
      Result := True;
      Exit;
    end else
    begin
      if (L = H) and (LastState = sGreater) then
      begin
        FoundIndex := L - 1;
        Exit;
      end;
      H := mid - 1;
      LastState := sLess;
    end;
  end;
  FoundIndex := H;
end;

function TILMachine._GetCallStack(CurProc: Pointer; SP: PByte): TCallStack;
var
  Idx: Integer;
  StackSize: Integer;
  PAddr: Pointer;
  Procs: PVMIMGProcFrames;
  CSFrame: TCallStackFrame;
  pRefCnt: PInt32;
begin
  Procs := PVMIMGProcFrames(FHeader.ProcFramesSection);
  PAddr := PByte(CurProc);
  while True do begin
    // ищем адрес процедуры в которой находится текущий указатель
    SearchProc(Procs, PAddr, Idx, FHeader.ProcFramesCount);
    if Idx = -1 then
      Break;

    CSFrame.CallAddr := PAddr;             // адрес вызова call
    CSFrame.ProcInfo := Procs[Idx].ProcInfo;   // RTTI процедуры
    Result := Result + [CSFrame];

    PAddr := PPointer(SP - 4)^; // адрес возврата
    if PAddr = nil then         // если адрес возврата = nil - мы раскрутили цикл полностью
      Break;

    StackSize := NativeInt(SP) - NativeInt(PPointer(SP - 8)^);  // сохраненный стек
    SP := SP - StackSize;
  end;
  // удержывает счетчик ссылок (освобождаться будет уже в VM)
  pRefCnt := PInt32(PByte(Result) - ARR_REFCNT_OFFSET);
  Inc(pRefCnt^);
end;

function TILMachine.GetFinalProcOffset(pUnit: PVMUnit): NativeUInt;
begin
  if pUnit.FinalProc > 0 then
    Result := pUnit.FinalProc - NativeUInt(FMem.Memory)
  else
    Result := 0;
end;

function TILMachine.GetParamName(const Param: PVMParam): string;
begin
  try
    Result := GetString(PByte(Param.Name))
  except
    Result := '<ERROR>';
  end;
end;

function TILMachine.GetProcName(const pProc: PVMExportProc): string;
var
  StructName: string;
begin
  Result := GetString(PByte(pProc.Name));
  if pProc.Struct > 0 then
  begin
    StructName := GetString(PByte(PRTTIType(pProc.Struct).Name));
    Result := StructName + '.' + Result;
  end;
end;

function TILMachine.GetProcOffset(const Proc: PRTTIProcedure): NativeUInt;
begin
  Result := Proc.ADDR - NativeUInt(FMem.Memory);
end;

function TILMachine.GetProcOffset(const pProc: PVMExportProc): NativeUInt;
begin
  Result := pProc.Offset - NativeUInt(FMem.Memory);
end;

function TILMachine.GetUnitName(const pUnit: PVMUnit): string;
begin
  Result := GetString(PByte(pUnit.Name));
end;

function TILMachine.GetVarName(const pVar: PVMVariable): string;
begin
  try
    Result := GetString(PByte(pVar.Name))
  except
    Result := '<ERROR>';
  end;
end;

function IncMemAligned(P: PByte; DataSize: Integer; Align: Integer): Pointer;
begin
  if (DataSize mod Align) = 0 then
    Result := P + DataSize
  else
    if Align > DataSize then
      Result := P + Align
    else
      Result := P + DataSize + (Align - (DataSize mod Align));
end;

function TILMachine.ReadSimple(var P: Pointer; TypeInfo: PRTTIType; Align: Integer = 1): string;
begin
  case TypeInfo.DataTypeID of
    dtInt8: Result := IntToStr(PInt8(P)^);
    dtInt16: Result := IntToStr(PInt16(P)^);
    dtInt32: Result := IntToStr(PInt32(P)^);
    dtUInt8: Result := IntToStr(PUInt8(P)^);
    dtUInt16: Result := IntToStr(PUInt16(P)^);
    dtUInt32: Result := IntToStr(Int64(PUInt32(P)^));
    dtInt64: Result := IntToStr(PInt64(P)^);
    dtUInt64: Result := IntToStr(PUInt64(P)^);
    dtNativeInt: Result := IntToStr(PNativeInt(P)^);
    dtNativeUInt: Result := IntToStr(PNativeUInt(P)^);
    dtString: Result := '''' + PUStr(P)^ + '''';
    dtAnsiString: Result := '''' + string(PAStr(P)^) + '''';
    dtChar: begin
      if PUChar(P)^ = #0 then
        Result := '#0'
      else
        Result := '''' + PUChar(P)^ + '''';
    end;
    dtAnsiChar: begin
      if PAChar(P)^ = #0 then
        Result := '#0'
      else
        Result := '''' + string(PAChar(P)^) + '''';
    end;
    dtFloat32: Result := FloatToStr(PFlt32(P)^);
    dtFloat64: Result := FloatToStr(PFlt64(P)^);
    dtBoolean: Result := BoolToStr(PBoolean(P)^, True);
    dtRange: begin
      Result := IntToStr(PNativeUInt(P)^);
    end;
    dtGuid: Result := GUIDToString(PGUID(P)^);
  else
    raise Exception.CreateFmt('Read value as string: type "%s" is not supported', [GetEnumName(System.TypeInfo(TDataTypeID), Ord(TypeInfo.DataTypeID))]);
  end;
  P := IncMemAligned(P, TypeInfo.DataSize, Align);
end;

function TILMachine.ReadValue(var P: Pointer; TypeInfo: PRTTIType; Align: Integer = 1): string;
begin
  if not Assigned(TypeInfo) then
    Exit('');
  case TypeInfo.DataTypeID of
    dtPointer, dtWeakRef: Result := ReadPointer(P, PRTTIPointer(TypeInfo));
    dtStaticArray: Result := ReadArray(P, 0, PRTTIArray(TypeInfo));
    dtDynArray: Result := ReadDynArray(P, 0, PRTTIArray(TypeInfo));
    dtRecord: Result := ReadRecord(P, PRTTIRecord(TypeInfo));
    dtClass: Result := ReadClass(P, PRTTIClass(TypeInfo));
    dtClassOf: Result := ReadPointer(P, PRTTIPointer(TypeInfo));
    dtInterface: Result := ReadIntf(P, PRTTIInterface(TypeInfo));
    dtProcType: Result := ReadProcType(P, PRTTIProcType(TypeInfo));
    dtEnum: Result := ReadEnum(P, PRTTIOrdinal(TypeInfo));
    dtSet: Result := ReadSet(P, PRTTISet(TypeInfo));
    dtVariant: Result := ReadVarinat(P, PRTTIVariant(TypeInfo));
  else
    Result := ReadSimple(P, TypeInfo, Align);
  end;
end;

function TILMachine.ReadArray(var P: Pointer; DimIndex: UInt32; TypeInfo: PRTTIArray): string;
var
  ElTypeInfo: PRTTIType;
  DimInfo: PRTTIOrdinal;
  i: Integer;
  ElData: string;
  Dimensions: PRTTIDimensions;
begin
  Result := '';
  {$R-}
  Dimensions := GetRTTIPtr(TypeInfo.Dimensions);
  DimInfo := PRTTIOrdinal(GetRTTIPtr(Dimensions[DimIndex]));
  {$R+}
  for i := DimInfo.LoBound to DimInfo.HiBound do
  begin
    if DimIndex < TypeInfo.DimensionsCount - 1 then
      ElData := ReadArray(P, DimIndex + 1, TypeInfo)
    else begin
      ElTypeInfo := GetRTTIPtr(TypeInfo.ElementTypeInfoOffset);
      ElData := ReadValue(P, ElTypeInfo);
    end;
    Result := AddStringSegment(Result, ElData);
  end;
  Result :=  'array(' + Result + ')';
end;

function TILMachine.ReadClass(var P: Pointer; TypeInfo: PRTTIClass): string;
var
  PtrValue: NativeUInt;
  CName: string;
  CType: PRTTIType;
begin
  PtrValue := PNativeUInt(P)^;
  if (PtrValue <> 0) and Check(FHeader.IMGFlags, IMG_HAS_RTTI) then
  begin
    CType := PVMObject(PByte(PtrValue) - Sizeof(TVMObject)).TypeInfo;
    if CType.ImportLib = 0 then
      CName := ' as ' + GetTypeName(CType);
  end;
  Result := Format('class($%x%s)', [PtrValue, CName]);
  Inc(PNativeInt(P));
end;

function TILMachine.ReadIntf(var P: Pointer; TypeInfo: PRTTIInterface): string;
begin
  Result := Format('interface($%x)', [PNativeUInt(P)^]);
  Inc(PNativeInt(P));
end;

function TILMachine.ReadDynArray(var P: Pointer; DimIndex: UInt32; TypeInfo: PRTTIArray): string;
var
  ElTypeInfo: PRTTIType;
  i: Integer;
  ElData: string;
  ArrayPtr: Pointer;
begin
  Result := '';
  ElTypeInfo := GetRTTIPtr(TypeInfo.ElementTypeInfoOffset);
  ArrayPtr := PPtr(P)^;
  if ArrayPtr <> nil then
  for i := 0 to PInt32(PByte(ArrayPtr) - 4)^ - 1 do
  begin
    ElData := ReadValue(ArrayPtr, ElTypeInfo);
    Result := AddStringSegment(Result, ElData);
  end;
  Result :=  'dynarray[' + Result + ']';
end;

function TILMachine.ReadEnum(var P: Pointer; TypeInfo: PRTTIOrdinal): string;
var
  S1, S2, SM: Integer;
  Buf: Int64;
begin
  S1 := GetValueByteSize(TypeInfo.LoBound);
  S2 := GetValueByteSize(TypeInfo.HiBound);
  SM := Max(S1, S2);
  Buf := 0;
  Move(P^, Buf, SM);
  Result := IntToStr(Buf);
end;

function TILMachine.ReadPointer(var P: Pointer; TypeInfo: PRTTIPointer): string;
var
  RefPtr: Pointer;
  Value: string;
begin
  RefPtr := PPointer(P)^;
  if Assigned(RefPtr) then
    Value := Format('$%x', [NativeUInt(RefPtr)])
  else
    Value := 'nil';
  Result := Format('pointer(%s)', [Value]);
  Inc(PNativeInt(P));
end;

function TILMachine.ReadProcType(var P: Pointer; TypeInfo: PRTTIProcType): string;
begin
  if TypeInfo.ProcStatic then
  begin
    if Pointer(P^) <> nil then
      Result := format('TProc($%x)', [NativeUInt(Pointer(P^))])
    else
      Result := 'TProc(nil)';
  end else begin
    if Pointer(P^) <> nil then
      Result := format('TMethod($%x, $%x)', [NativeUInt(PILMethod(P).Proc), NativeUInt(PILMethod(P).Self)])
    else
      Result := 'TMethod(nil)';
  end;
end;

function TILMachine.ReadRecord(var P: Pointer; TypeInfo: PRTTIRecord): string;
  function DoReadRecord(var P: Pointer; TypeInfo: PRTTIRecord): string;
  var
    i: Integer;
    AncestorTypeInfo, MemberInfo: PRTTIType;
    Fields: TRTTIFields;
    Fld: PRTTIField;
    Value: string;
  begin
    Result := '';
    if TypeInfo.Ancestor <> 0 then
    begin
      AncestorTypeInfo := PRTTIType(TypeInfo.Ancestor);
      Result := DoReadRecord(P, PRTTIRecord(AncestorTypeInfo));
    end;
    Fields := TRTTIFields(GetRTTIPtr(TypeInfo.Fields));
    for i := 0 to TypeInfo.FieldsCount - 1 do begin
      Fld := @(Fields[i]);
      MemberInfo := GetRTTIPtr(Fld.DataType);
      Value := ReadValue(P, MemberInfo, 4);
      Result := AddStringSegment(Result, Value, ', ');
    end;
  end;
begin
  Result := DoReadRecord(P, TypeInfo);
  Result := 'record(' + Result + ')';
end;

procedure TILMachine.ReadResultFromStack(Stack: PByte; ResultDataType: TDataTypeID; Result: Pointer);
begin
  case ResultDataType of
    dtInt8, dtUInt8, dtBoolean, dtAnsiChar: PInt8(Result)^ := PInt8(Stack)^;
    dtInt16, dtUInt16, dtChar: PInt16(Result)^ := PInt16(Stack)^;
    dtInt32, dtUInt32, dtFloat32: PInt32(Result)^ := PInt32(Stack)^;
    dtInt64, dtUInt64, dtFloat64: PInt64(Result)^ := PInt64(Stack)^;
    dtPointer, dtClass, dtString, dtInterface, dtNativeInt,
    dtNativeUInt: PNativeInt(Result)^ := PNativeInt(Stack)^;
  else
    raise exception.CreateFmt('Data type "%s" is not supported', [GetDataTypeName(ResultDataType)]);
  end;
end;

function TILMachine.ReadSet(var P: Pointer; TypeInfo: PRTTISet): string;
var
  i, c: Integer;
  DimInfo: PRTTIOrdinal;
  S, Bit: UInt64;
begin
  Result := '';
  DimInfo := PRTTIOrdinal(GetRTTIPtr(TypeInfo.Dimensions));
  c := Integer(DimInfo.HiBound) - Integer(DimInfo.LoBound);
  if c > 256 then
    c := 256; // ограничение вывода
  S := PUint64(P)^;
  Bit := 1;
  c := c + 3;
  SetLength(Result, c);
  Result[1] := '[';
  Result[c] := ']';
  for i := 2 to c - 1 do begin
    if (S and Bit) <> 0 then
      Result[i] := '1'
    else
      Result[i] := '0';
    Bit := Bit shl 1;
  end;
end;

function TILMachine.ReadVarinat(var P: Pointer; TypeInfo: PRTTIVariant): string;
begin
  Result := PVMVariant(P).AsString();
  Inc(PByte(P), SizeOf(Variant));
end;

function TILMachine.ReadVarValue(const Variable: PVMVariable): string;
var
  Addr: Pointer;
begin
  if Check(FHeader.IMGFlags, IMG_HAS_RTTI) then
  begin
    Addr := Variable.Addr;
    Result := ReadValue(Addr, Variable.DataType);
  end;
end;

{ TVMUnit }

function TVMUnit.GetExportProcsCount: Int32;
begin
  if Assigned(FExportProcs) then
    Result := PNativeUInt(PByte(FExportProcs) - PTR_SIZE)^
  else
    Result := 0;
end;

function TVMUnit.GetProcsCount: Int32;
begin
  Result := PNativeUInt(PByte(FProcs) - PTR_SIZE)^;
end;

function TVMUnit.GetTypesCount: Integer;
begin
  Result := PNativeUInt(PByte(FTypes) - PTR_SIZE)^;
end;

initialization
  Assert(Integer(High(TVMCode)) < High(Byte));

end.
