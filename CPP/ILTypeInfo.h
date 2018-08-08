//---------------------------------------------------------------------------

#ifndef ILTypeInfoH
#define ILTypeInfoH

#include <stdint.h>
#include "CRUtils.h"

namespace VM {
  using namespace CRBase;

  typedef NativeUInt TOffset;

  enum TProcType: char {ptProcedure, ptFunction};
  enum TCallConvention: char {ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall};

  #pragma pack (push, 1)
  struct TObjectHeader{
  private:
	Pointer FTypeInfo;
	NativeInt FRefCount;
	Pointer FWeekInfo;
	Pointer FSyncInfo;
  };
  #pragma pack (pop)


  enum TDataTypeID: char
  {
	dtInt8,
	dtInt16,
	dtInt32,
	dtInt64,
	dtUInt8,
	dtUInt16,
	dtUInt32,
	dtUInt64,
	dtNativeInt,
	dtNativeUInt,
	dtFloat32,
	dtFloat64,
	dtBoolean,      // булевый тип
	dtAnsiChar,     // ansi-символ
	dtChar,         // utf16-символ
	dtAnsiString,   // ansi-строка
	dtString,       // utf16-строка
	dtVariant,      // тип вариант
	dtGuid,         // тип GUID
	dtPointer,      // указатель (любой)
	dtWeakRef,      // слабая ссылка
	dtGeneric,      // обобщенный тип
	dtRange,        // диаппазоный тип (тип с ограниченным диаппазоном)
	dtEnum,         // перечисление
	dtSet,
	dtStaticArray,  // статический массив
	dtDynArray,     // динамический массив
	dtOpenArray,
	dtProcType,     // процедурная ссылка
	dtRecord,       // структура
	dtClass,        // класс
	dtClassOf,      // метакласс
	dtInterface     // интерфейс
  };

  #pragma pack (push, 1)
  struct TILTypeInfo: TObjectHeader{
  public:
	TDataTypeID DataTypeID;  // константа элементарного типа
	Boolean TypePacked;
	UInt32 DataSize;         // размер в байтах элемента данного типа
	TOffset TypeName;        // имя типа
	Int32 UnitID;            // ID модуля
	Int32 Index;             // индекс
	TOffset ImportLib;       // название библиотеки импорта
	TOffset ImportName;      // название типа в библиотке импорта
	Pointer ImportADDR;      // указатель в памяти
  };
  #pragma pack (pop)

  static_assert(sizeof(TILTypeInfo) == 46, "sizeof(TILTypeInfo) not correct");

  typedef TILTypeInfo* PILTypeInfo;

  //=====================================================
  // POINTER TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIPointer: TILTypeInfo{
	TOffset RefTypeInfoOffset;
  };
  #pragma pack (pop)
  typedef TRTTIPointer* PRTTIPointer;

  //=====================================================
  // ORDINAL TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIOrdinal: TILTypeInfo{
	Int64 LoBound;  // нижняя граница ordinal типа
	Int64 HiBound;  // верхняя граница ordinal типа
	bool Signed;    // знаковый ли тип
  };
  #pragma pack (pop)

  static_assert(sizeof(TRTTIOrdinal) == 63, "sizeof(TRTTIOrdinal) not correct");

  typedef TRTTIOrdinal* PRTTIOrdinal;

  //=====================================================
  // ARRAY TYPE
  //=====================================================

  typedef TOffset TRTTIDimensions[65536];
  typedef TRTTIDimensions* PRTTIDimensions;

  #pragma pack (push, 1)
  struct TRTTIArray: TILTypeInfo{
	TOffset ElementTypeInfoOffset; // ссылка на тип элемента массива
	UInt32 DimensionsCount;        // кол-во измерений массива
	TOffset Dimensions;            // информация по измерениям (ссылки на TypeInfo)
  };
  #pragma pack (pop)

  static_assert(sizeof(TRTTIArray) == 58, "sizeof(TRTTIArray) not correct");

  typedef TRTTIArray* PRTTIArray;

  #pragma pack (push, 1)
  struct TRTTIDynArray: TILTypeInfo{
	int Flags;
	TOffset InitProc;
	TOffset FinalProc;
  };
  #pragma pack (pop)
  typedef TRTTIDynArray* PRTTIDynArray;

  typedef TRTTIArray TRTTISet;
  typedef TRTTISet* PRTTISet;

  //=====================================================
  // STRUCT FIELD INFO
  //=====================================================

  enum TCopyOnWriteState: char {cowNotNeeded, cowNeeded, cowDone};

  #pragma pack (push, 1)
  struct TRTTIField
  {
	TOffset Name;       // название поля
	Int32 Index;        // индекс поля в структуре
	Int32 Offset;       // смещение в стековом фрейме/области глобальных переменных/структуре
	Boolean Reference;  // это ссылочный параметр
	Boolean IsConstant; // это константа
	TOffset DataType;   // тип поля
	TCopyOnWriteState CopyOnWrite;
  };
  #pragma pack (pop)
  typedef TRTTIField* PRTTIField;

  typedef TRTTIField TRTTIFields[65536];
  typedef TRTTIFields* PRTTIFields;

  //=====================================================
  // PROCEDURE PARAMETER INFO
  //=====================================================

  typedef TRTTIField TRTTIParameter;
  typedef PRTTIField* PRTTIParameter;

  typedef TRTTIParameter TRTTIParams[];
  typedef TRTTIParams* PRTTIParams;

  //=====================================================
  // PROCEDURAL TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIProcType: TILTypeInfo
  {
	Boolean ProcStatic; // является ли обявление типа указателем на статическую процедуру или на метод
	TOffset ResultType;
	TCallConvention CallConv;
	Int32 ParamsCount;
	TOffset Params;
  };
  #pragma pack (pop)
  typedef TRTTIProcType* PRTTIProcType;


  #pragma pack (push, 1)
  struct TILMethod{
	Pointer Proc;
	Pointer Self;
  };
  #pragma pack (pop)
  typedef TILMethod* PILMethod;

  //=====================================================
  // PROCEDURE INFO
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIProcedure
  {
	TOffset Name;
	TOffset ADDR;
	TCallConvention CallConv;
	TOffset StructInfo;          // self
	Pointer ExternalMethod;
	TOffset ResultType;
	Int32 ParamsCount;
	TOffset Params;
	Int32 StackSize;
	Int32 Flags;
	TOffset ImportLib;           // библиотека импорта (0 если нет)
	TOffset ImportName;          // имя в библиотеке импорта (0 если нет)
	Int32 VirtualIndex;
  };
  #pragma pack (pop)
  typedef TRTTIProcedure* PRTTIProcedure;

  typedef TRTTIProcedure TRTTIProcedures[65536];
  typedef TRTTIProcedures* PRTTIProcedures;

  //=====================================================
  // COMMON STRUCT TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIStruct: TILTypeInfo {
	TOffset Ancestor;                  // предок
	Int32 FieldsCount;                 // кол-во полей
	TOffset Fields;                    // поля
	Int32 MethodsCount;                // кол-во методов
	TOffset Methods;                   // методы
	Int32 TotalMethodsCount;           // кол-во методов включая всех предков
	Int32 TotalFieldsCount;            // кол-во полей включая всех предков
  };
  #pragma pack (pop)
  typedef TRTTIStruct* PRTTIStruct;

  //=====================================================
  // RECORD TYPE
  //=====================================================

  enum TRecordFlag: char {RecordHasManaged, RecordPacked};
  typedef UInt8 TRecordFlags;

  #pragma pack (push, 1)
  struct TRTTIRecord: TRTTIStruct
  {
	TRecordFlags Flags;
  };
  #pragma pack (pop)
  typedef TRTTIRecord* PRTTIRecord;

  //=====================================================
  // INTERFACE TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIInterface: TRTTIStruct
  {
	Int32 InterfaceID;  // ID интерфейса
	char GUID[16];      // GUID интерфейса
  };
  #pragma pack (pop)
  typedef TRTTIInterface* PRTTIInterface;

  typedef TOffset TIMTS[65536];
  typedef TIMTS* PIMTS;

  typedef TOffset TIMT[65536];
  typedef TIMT* PIMT;

  //=====================================================
  // CLASS TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIClass: TRTTIStruct {
	Int32 VMTCount; // кол-во строк в таблице VMT
	TOffset VMT;    // указатель на таблицу VMT, таблица содержит все вируальные методы начиная с TObject
	TOffset IMTS;   // ссылка на таблицу таблиц IMT
  };
  #pragma pack (pop)
  typedef TRTTIClass* PRTTIClass;

  typedef int TVMT[65535];
  typedef TVMT* PVMT;

  //=====================================================
  // VARIANT TYPE
  //=====================================================

  #pragma pack (push, 1)
  struct TRTTIVariant: TILTypeInfo{
  };
  typedef TRTTIVariant* PRTTIVariant;
  #pragma pack (pop)

  //=====================================================
  // utils functions
  //=====================================================

  int GetRTTIStructSize(const TDataTypeID DataTypeID);
  bool ClassInheritsFrom(const PRTTIClass ChildClass, const PRTTIClass AncestorClass);

//---------------------------------------------------------------------------
}//VM
#endif

