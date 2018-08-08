//---------------------------------------------------------------------------

#ifndef ILMachineTypesH
#define ILMachineTypesH

#include <stdint.h>
#include "CRStreams.h"
#include "ILTypeInfo.h"

namespace VM {

  using namespace CRBase;

  typedef bool* PBoolean;
  typedef Float32* PFlt32;
  typedef Float64* PFlt64;

  typedef Int8* PInt8;
  typedef Int16* PInt16;
  typedef Int32* PInt32;
  typedef Int64* PInt64;

  typedef UInt8* PUInt8;
  typedef UInt16* PUInt16;
  typedef UInt32* PUInt32;
  typedef UInt64* PUInt64;
  typedef CRBase::String* PUStr;
  typedef CRBase::AnsiString* PAStr;
  typedef Pointer* PPtr;
  typedef PNativeInt PNInt;

#ifdef __BORLANDC__
  typedef WideChar* PUChar;
  typedef AnsiChar* PAChar;
#else
  typedef wchar_t*  PUChar;
  typedef char*     PAChar;
#endif


  const unsigned int PTR_SIZE = sizeof(Pointer);

  class TILMemoryStream: public TMemoryStream {
	public:
	  TILMemoryStream() : TMemoryStream()
	  { }
  };

  class EVMException: public Exception {
  };

  #pragma pack (push, 1)
  struct TImportEntry
  {
    TOffset Struct;
	TOffset Name;
	TOffset LibName;
	NativeInt ADDR;
	NativeInt Adapter;
    Boolean IsIntfMethod;
  };
  #pragma pack (pop)
  typedef TImportEntry* PImportEntry;
  typedef TImportEntry TImportTable[65536];
  typedef TImportTable* PImportTable;

  #pragma pack (push, 1)
  struct TIMGHeader {
	UInt32 IMGFlags;
	UInt32 IMGSize;
	TOffset RTTIOffset;
	TOffset ConstsOffset;
	UInt32 ImportTableCount;
	TOffset ImportTable;
	UInt32 UnitsCount;
	TOffset UnitsOffset;
  };
  #pragma pack (pop)
  typedef TIMGHeader* PIMGHeader;

  #pragma pack (push, 1)
  struct TVMVariable {
	PILTypeInfo TypeInfo;
    Boolean Reference;
	TOffset Name;		//FIXME: int type for name
	Pointer Addr;
  };
  #pragma pack (pop)
  typedef TVMVariable* PVMVariable;

  typedef TVMVariable TVMVariables[65536];
  typedef TVMVariables* PVMVariables;

  // процедуры
  #pragma pack (push, 1)
  struct TVMProc {
	TOffset Name;
	TOffset Offset;
	TOffset Struct;
	UInt32 StackSize;
	TProcType ProcType;
	int VarsCount;
	PVMVariables Vars;
  };
  #pragma pack (pop)
  typedef TVMProc* PVMProc;

  typedef TVMProc TVMProcedures[65536];
  typedef TVMProcedures* PVMProcedures;

  // типы
  typedef TILTypeInfo TVMTypes[65536];
  typedef TVMTypes* PVMTypes;

  #pragma pack (push, 1)
  struct TVMUnit {
	TOffset Name;            //FIXME: Why is type of Int?
	Int32 TypesCount;
	PVMTypes Types;
	Int32 VarsCount;
	PVMVariables Vars;
	Int32 ProcsCount;
	PVMProcedures Procs;
	TOffset InitProc;   // секция инициализации
	TOffset FinalProc;  // секция финализации
  };
  #pragma pack (pop)
  typedef TVMUnit* PVMUnit;

  typedef TVMUnit TVMUnits[65536];
  typedef TVMUnits* PVMUnits;

  enum TILCondition {
	cNone,
	cEqual,
	cNotEqual,
	cGreater,
	cGreaterOrEqual,
	cLess,
	cLeassOrEqual,
	cZero,
	cNonZero
  };

  enum TVMMacroID {
    vmsmCheckClass,   // проверяет является ли инстанс заданным классом (и не нулом)
	vmsmQueryClass,	  // приводит класс к заданному (или nullptr)
	vmsmCheckIntf,    // проверяет реализует ли инстанс заданный интерфейс (и не нулом)
	vmsmQueryIntf,    // приводит класс к заданному интерфейсу (или nullptr)
	vmsmStrRefCount,  // вычисляет refcount строки
    vmsmArrRefCount,  // вычисляет refcount дин. массива
	vmsmObjRefCount   // вычисляет refcount обьекта/интерфейса
  };

  enum TIMGFormat {
	IMG_32Bit,     // 32-разрядный образ
	IMG_64Bit      // 66-разрядный образ
  };

  enum TRTTICharset {
	RTTICharsetASCII,
	RTTICharsetUTF8,
	RTTICharsetUTF16,
	RTTICharsetUTF32
  };

}//VM
//---------------------------------------------------------------------------
#endif
