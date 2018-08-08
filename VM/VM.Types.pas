unit VM.Types;

interface

uses SysUtils, IL.TypeInfo;

{$I compilers.inc}

const
  C1G = 1024*1024*1024;

{$IF Defined(CPUX86) or Defined(CPU86) or Defined(CPUARM)}
  {$DEFINE PTR_SIZE4}
  const PTR_SIZE = 4;
{$IFEND}

{$IF Defined(CPUX64) or Defined(CPUX86_64)}
  {$DEFINE PTR_SIZE8}
  const PTR_SIZE = 8;
{$IFEND}

  VM_IMG_SIGNATURE_V1: UInt32 = $ABCDEF01;
  VM_IMG_RTTI_SIGNATURE: UInt32 = $FFFFFFFF;

type

  {$IFDEF NEXTGEN}
  AnsiString = UTF8String;
  AnsiChar = System.UTF8Char;
  PAnsiChar = ^AnsiChar;
  PAnsiString = ^AnsiString;
  {$ENDIF}

  PInt8 = PShortInt;
  PUInt8 = PByte;
  PInt16 = PSmallInt;
  PUInt16 = PWORD;
  PInt32 = PInteger;
  PUInt32 = PCardinal;

  PUStr = PUnicodeString;
  PUChar = PChar;
  PAChar = PAnsiChar;
  PAStr = PAnsiString;
  PFlt32 = PSingle;
  PFlt64 = PDouble;
  PPtr = PPointer;

  {$IFDEF FPC}
  PUInt64 = ^UInt64;
  PBoolean = ^Boolean;
  {$ENDIF}

  PNInt = PNativeInt;
  PNUInt = PNativeUInt;

  Float32 = Single;
  Float64 = Double;

  EVMException = class(Exception)

  end;

  TIMGImportEntry = packed record
    Struct: TOffset;
    Name: TOffset;
    LibName: TOffset;
    ADDR: NativeUInt;
    Adapter: NativeInt;
    IsIntfMethod: Boolean;
  end;
  PImportEntry = ^TIMGImportEntry;
  TImportTable = array [0..65535] of TIMGImportEntry;
  PImportTable = ^TImportTable;

  TIMGFormat = (
    IMG_32Bit,     // 32-разрядный образ
    IMG_64Bit      // 64-разрядный образ
  );

  // заголовок
  TIMGHeader = packed record
    Signature: UInt32;           // стартовая cигнатура
    IMGFlags: UInt32;            // флаги образа
    IMGSize: UInt32;             // размер образа
    RTTIOffset: TOffset;         // смщение секции RTTI
    ConstsOffset: TOffset;       // смщение секции констант
    ProcFramesCount: UInt32;     // кол-во всего процедур в образе
    ProcFramesSection: TOffset;  // смещение секции списка (адрес, rtti) процедур
    ImportTableCount: UInt32;    // кол-во элементов таблицы импорта
    ImportTable: TOffset;        // смещение таблицы импорта
    UnitsOffset: TOffset;        // смещение секции модулей
    FixTable: TOffset;           // смещение фикс-таблицы
    HeaderCRC32: UInt32;         // CRC32 заголовка
  end;
  PIMGHeader = ^TIMGHeader;

  TVMMacroID = (
    vmsmCheckClass,   // проверяет является ли инстанс заданным классом (и не нулом)
    vmsmQueryClass,   // приводит класс к заданному (или nullptr)
    vmsmCheckIntf,    // проверяет реализует ли инстанс заданный интерфейс (и не нулом)
    vmsmQueryIntf,    // приводит класс к заданному интерфейсу (или nullptr)
    vmsmStrRefCount,  // вычисляет refcount строки
    vmsmArrRefCount,  // вычисляет refcount дин. массива
    vmsmObjRefCount,  // вычисляет refcount обьекта/интерфейса
    vmsmNow,          // получает локальную дату/время
    vmsmQTypeInfo,
    vmsmGetCallStack,
    vmsmGetCurUnit,
    vmsmGetUnitsList
  );

const
  IMG_HAS_RTTI = 16;
  IMG_HAS_DEBUG_INFO = 32;

implementation

end.
