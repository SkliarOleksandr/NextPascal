//---------------------------------------------------------------------------
#include <stdint.h>
#include <cstring>
#include <string>
//---------------------------------------------------------------------------
#ifdef __BORLANDC__
  #include <vcl.h>
  #include <SysUtils.hpp>
  #define USE_EXCEPTIONS
  using namespace System;
#endif

#include "ILMachine.h"
#include "ILMachineInvoke.h"
#include "ILTypeInfo.h"

#ifdef __GNUG__
  #ifdef _CHIBIOS_RT_
    #include "ch.hpp"
  #elif defined _FREERTOS_
	#include "FreeRTOS.h"
  #endif
#endif

namespace VM{

using namespace CRBase;

//if GCC used, these defines are neccessary for std::to_string: "_GLIBCXX_USE_C99", "_GLIBCXX_HAVE_BROKEN_VSWPRINTF"
using std::to_string;

#ifdef __GNUG__
size_t strnlen_s(const char* str, size_t len)
{
	size_t res = 0;
	if(str) res = strnlen(str, len);
	return res;
}
#endif

template<typename T>
static CRBase::UnicodeString to_wstring(T Value)
{
  #ifndef __GNUG__
	return std::to_wstring(Value);
  #else
   return CRBase::UnicodeString();  //FIXME: Dummy op
  #endif
}

CRBase::String IntToHex(Int32 Value, size_t)
{
  return std::to_string(Value);
}

CRBase::String str_to_wstring(const std::string& in, std::locale loc)
{
	CRBase::String out(in.length(), 0);
	auto i = in.begin(), ie = in.end();
	auto j = out.begin();
	for(; i!=ie; ++i, ++j)
		*j = std::use_facet<std::ctype<wchar_t>>(loc).widen(*i);
	return out;
}

CRBase::String str_to_wstring(const char* str)
{
	size_t len = strnlen_s(str, 4096);
	if(len >= 4095)
		return CRBase::String(L_"Length of string exceed 4k chars");
	return str_to_wstring(std::string(str));
}

Pointer VMAllocMem(size_t Size)
{
  return calloc(Size, 1);
}

void VMFreeMem(Pointer P)
{
  free(P);
}


#ifndef __BORLANDC__
static void InterlockedIncrement(long int*)
{

}

static void InterlockedDecrement(long int*)
{

}
#endif

static inline void _VM_MOVE(void* dest, void* src, size_t num)
{
	std::memcpy(dest, src, num);
}

static inline Pointer MemoryPosition(TMemoryStream* Stream)
{
  #ifndef __BORLANDC__
	return PByte(Stream->GetMemory()) + Stream->GetPosition();
  #else
	return PByte(Stream->Memory) + Stream->Position;
  #endif
}

static inline void StreamPosition(TStream* Stream, int Position)
{
#ifndef __BORLANDC__
	Stream->SetPosition(Position);
  #else
	Stream->Position = Position;
  #endif
}

static inline int StreamSize(TStream* Stream)
{
#ifndef __BORLANDC__
  return Stream->GetSize();
  #else
	return Stream->Size;
  #endif
}

inline PByte TILMachine::GetIMGPtr(TOffset Offset) const
{
  return IMGPtr() + Offset;
}

inline PILTypeInfo TILMachine::GetTypeInfo(TOffset TypeInfoOffset) const
{
  return PILTypeInfo(IMGPtr() + TypeInfoOffset);
}

inline PILTypeInfo TILMachine::GetRTTIPtr(TOffset TypeInfoOffset) const
{
  return PILTypeInfo(IMGPtr() + FHeader->RTTIOffset + TypeInfoOffset);
}

PByte IncMemAligned(PByte P, Int32 DataSize, Int32 Align)
{
  PByte Result;
  if ((DataSize % Align) == 0)
	Result = P + DataSize;
  else
	if (Align > DataSize)
	  Result = P + Align;
	else
	  Result = P + DataSize + (Align - (DataSize % Align));
  return Result;
}

PByte TILMachine::IMGPtr() const
{
  #ifndef __BORLANDC__
    return PByte(FMem->GetMemory());
  #else
	return PByte(FMem->Memory);
  #endif
}

TVMError TILMachine::RunInitSections(PByte Stack)
{
  TOffset P;

  if (!Stack) Stack = FStack;

  *PPtr(Stack) = NULL; // адрес возврата
  Stack = Stack + PTR_SIZE;
  // место в стеке под результат
  *PPtr(Stack) = NULL;

  TVMError Result = VM_OK;
  for (int i = 0; i < FUnitsCount; i++)
  {
    P = (*FUnits)[i].InitProc;
    if (P == 0)
      continue;
	Result = CallILProc(PNativeUInt(P), Stack, IMGPtr());
	if (Result != VM_OK) break;
  };
  return Result;
}

TVMError TILMachine::RunFinalSections(PByte Stack)
{
  TOffset P;

  if (!Stack) Stack = FStack;

  *PPtr(Stack) = NULL; // адрес возврата
  Stack = Stack + PTR_SIZE;
  // местро в стеке под результат
  *PPtr(Stack) = NULL;

  TVMError Result = VM_OK;
  for (int i = FUnitsCount - 1; i > 0; i--)
  {
	P = (*FUnits)[i].FinalProc;
	if (P == 0)
	  continue;
	Result = CallILProc(PNativeUInt(P), Stack, IMGPtr());
	if (Result != VM_OK) break;
  };
  return Result;
}

TVMError TILMachine::Run()
{
  TVMError Result = RunInitSections(FStack);
  if (Result == VM_OK)
    Result = RunFinalSections(FStack);
  return Result;
}

TVMError TILMachine::Run(PByte Stack, UInt32 StackSize)
{
  FStackSize = StackSize;
  TVMError Result = RunInitSections(Stack);
  if (Result == VM_OK)
	Result = RunFinalSections(Stack);
  return Result;
}

void WriteInt32ToStack(PByte &Stack, Int32 Value)
{
  *PInt32(Stack) = Value;
  Stack = Stack + sizeof(Int32);
}

TVMError TILMachine::RunProc(PVMProc Proc)
{
  if (!Proc)
	return VM_INVALID_PARAM;

  return CallILProc(PNativeUInt(Proc->Offset), FStack, IMGPtr());
}

TVMError TILMachine::RunProc(PVMProc Proc, Pointer Param0)
{
  if (!Proc)
	return VM_INVALID_PARAM;

  PByte Stack = FStack;

  // место под корневой адрес возврата (возврат из VM)
  *PPtr(Stack) = nullptr;
  Stack = Stack + PTR_SIZE;
  // первый параметр
  *PPtr(Stack) = Param0;

  return CallILProc(PNativeUInt(Proc->Offset), Stack, IMGPtr());
}

TVMError TILMachine::RunFunc(PVMProc Proc, Pointer Param0, NativeUInt &Result)
{
  if (!Proc)
	return VM_INVALID_PARAM;

  PByte Stack = FStack;

  // место под корневой адрес возврата (возврат из VM)
  *PPtr(Stack) = nullptr;
  Stack = Stack + PTR_SIZE;
  // местро в стеке под результат
  *PNativeUInt(Stack) = 0;
  // первый параметр
  *PPtr(Stack + PTR_SIZE) = Param0;

  TVMError Res = CallILProc(PNativeUInt(Proc->Offset), Stack, IMGPtr());

  if (Res != VM_OK)
    return Res;

  // копируем результат
  Result = *PNativeUInt(Stack);

  return VM_OK;
}


CRBase::String TILMachine::ReadPointer(PByte& P, PRTTIPointer TypeInfo) const
{
  (void)TypeInfo;
  CRBase::String Result = Concat(L_"pointer($", VM::IntToHex(Int64(*PNativeUInt(P)), PTR_SIZE*2), L_")");
  P = P + PTR_SIZE;
  return Result;
}

CRBase::String TILMachine::ReadClass(PByte& P, PRTTIClass TypeInfo) const
{
  (void)TypeInfo;
  CRBase::String Result = Concat(L_"class($", VM::IntToHex(Int64(*PNativeUInt(P)), PTR_SIZE*2), L_")");
  P = P + PTR_SIZE;
  return Result;
}

CRBase::String TILMachine::ReadIntf(PByte& P, PRTTIInterface TypeInfo) const
{
  (void)TypeInfo;
  CRBase::String Result = Concat(L_"interface($", VM::IntToHex(Int64(*PNativeUInt(P)), PTR_SIZE*2), L_")");
  P = P + PTR_SIZE;
  return Result;
}

CRBase::String TILMachine::ReadProcType(PByte& P, PRTTIProcType TypeInfo) const
{
  (void)TypeInfo;
  CRBase::String Result;
  if (TypeInfo->ProcStatic)
  {
	if (P != NULL)
	  Result = Concat(L_"TProc($", VM::IntToHex(Int64(*PNativeUInt(P)), PTR_SIZE*2), L_")");
	else
	  Result = L_"TProc(nil)";
  } else {
	if (P != NULL)
	  Result = Concat(L_"TMethod($", VM::IntToHex(Int64(PILMethod(P)->Proc), PTR_SIZE*2), L_", $", VM::IntToHex(Int64(PILMethod(P)->Self), PTR_SIZE*2), L_")");
	else
	  Result = L_"TMethod(nil)";
  }
  return Result;
}

CRBase::String TILMachine::ReadArray(PByte& P, UInt32 DimIndex, PRTTIArray TypeInfo) const
{
  PILTypeInfo ElTypeInfo;
  CRBase::String ElData;
  CRBase::String Result;

  PRTTIDimensions Dimensions = PRTTIDimensions(GetRTTIPtr(TypeInfo->Dimensions));

  PRTTIOrdinal DimTypeInfo = PRTTIOrdinal(GetRTTIPtr((*Dimensions)[DimIndex]));

  for (auto i = DimTypeInfo->LoBound; i <= DimTypeInfo->HiBound; i++)
  {
	if (DimIndex < TypeInfo->DimensionsCount - 1)
	  ElData = ReadArray(P, DimIndex + 1, TypeInfo);
	else {
	  ElTypeInfo = GetRTTIPtr(TypeInfo->ElementTypeInfoOffset);
	  ElData = ReadValue(P, ElTypeInfo);
	};
	Result = Result + ElData + L_", ";
  };
  Result = Concat(L_"array(", Result, L_")");
  return Result;
}

CRBase::String TILMachine::ReadDynArray(PByte& P, UInt32 DimIndex, PRTTIArray TypeInfo) const
{
  (void)P; (void)DimIndex; (void)TypeInfo;
  return EMPTY_STR;
}

CRBase::String TILMachine::ReadSimple(PByte& P, PILTypeInfo TypeInfo, int Align) const
{
  CRBase::String Result;
  switch (TypeInfo->DataTypeID)
  {
	case dtInt8: Result = to_string(*PInt8(P)); break;
	case dtInt16: Result = to_string(*PInt16(P)); break;
	case dtInt32: Result = to_string(*PInt32(P)); break;
	case dtUInt8: Result = to_string(*PUInt8(P)); break;
	case dtUInt16: Result = to_string(*PUInt16(P)); break;
	case dtUInt32: Result = to_string(Int64(*PUInt32(P))); break;
	case dtInt64: Result = to_string(*PInt64(P)); break;
	case dtUInt64: Result = to_string(Int64(*PUInt64(P))); break;
	case dtNativeInt: Result = to_string(*PNativeInt(P)); break;
	case dtNativeUInt: Result = to_string(Int64(*PNativeUInt(P))); break;
	case dtString: Result = L_"""" + CRBase::String(PChar(*PPointer(P))) + L_""""; break;
	case dtAnsiString: Result = L_"""" + CRBase::String(PChar(*PPointer(P))) + L_""""; break;
	case dtChar:
	  if (*PUChar(P) == 0)
		Result = L_"#0";
	  else
		Result = *PUChar(P);
	  break;
	case dtAnsiChar:
	  if (*PAChar(P) == 0)
		Result = L_"#0";
	  else
		Result = str_to_wstring(PAChar(P));
	  break;
	case dtFloat32: Result = to_string(*PFlt32(P)); break;
	case dtFloat64: Result = to_string(*PFlt64(P)); break;
	case dtBoolean: Result = to_string(*PBoolean(P)); break;
	case dtEnum: Result = to_string(Int64(*PNativeUInt(P))); break;
	case dtRange: Result = to_string(Int64(*PNativeUInt(P))); break;
	case dtGuid: Result = L_"GUID is unsupported type"; break;
	default:
	  #ifdef USE_EXCEPTIONS
	  throw Exception(L_"Read value as CRBase::String: type not supported %d", TypeInfo->DataTypeID);
	  #endif
	  ;
  };
  P = IncMemAligned(P, TypeInfo->DataSize, Align);
  return Result;
}


CRBase::String TILMachine::DoReadRecord(PByte& P, PRTTIRecord TypeInfo) const
{
  PILTypeInfo AncestorTypeInfo, MemberInfo;
  PRTTIFields Fields;
  CRBase::String Value;
  CRBase::String Result;

  if (TypeInfo->Ancestor != 0)
  {
	AncestorTypeInfo = GetRTTIPtr(TypeInfo->Ancestor);
	Result = DoReadRecord(P, PRTTIRecord(AncestorTypeInfo));
  };
  Fields = PRTTIFields(GetRTTIPtr(TypeInfo->Fields));
  for (int i = 0; i < TypeInfo->FieldsCount; i++)
  {
	MemberInfo = GetRTTIPtr((*Fields)[i].DataType);
	Value = ReadValue(P, MemberInfo, PTR_SIZE);
	Result = Concat(Result, Value, L_"; ");// AddStringSegment(Result, Value, ", ");
  };
  return Result;
}

CRBase::String TILMachine::ReadRecord(PByte& P, PRTTIRecord TypeInfo) const
{
  CRBase::String Result = DoReadRecord(P, TypeInfo);
  Result = Concat(L_"record(", Result, L_")");
  return Result;
}

CRBase::String TILMachine::ReadSet(PByte& P, PRTTISet TypeInfo) const
{
  UInt64 S, Bit;

  CRBase::String Result;

  PRTTIDimensions Dimensions = PRTTIDimensions(GetRTTIPtr(TypeInfo->Dimensions));
  PRTTIOrdinal DimInfo = PRTTIOrdinal(GetRTTIPtr((*Dimensions)[0]));

  auto c = NativeInt(DimInfo->HiBound - DimInfo->LoBound + 3);
  if (c > 259) c = 259;
  S = *PUInt64(P);
  Bit = 1;
  Result.resize((size_t)c);
  Result[0] = '[';
  Result[c-1] = ']';
  for (int i = 1; i < (c - 1); i++)
  {
	if ((S & Bit) != 0)
	  Result[i] = '1';
	else
	  Result[i] = '0';
	Bit = Bit << 1;
  };
  return Result;
}

CRBase::String TILMachine::ReadVarinat(PByte& P, PRTTIVariant TypeInfo) const
{
  (void)TypeInfo; (void)P;
  //Result := PVariant(P)^;
  //Inc(PByte(P), SizeOf(Variant));
  return EMPTY_STR;
}

CRBase::String TILMachine::ReadValue(PByte& P, PILTypeInfo TypeInfo, int Align) const
{
  CRBase::String Result;
  switch (TypeInfo->DataTypeID)
  {
	case dtPointer:
	case dtWeakRef: Result = ReadPointer(P, PRTTIPointer(TypeInfo)); break;
	case dtStaticArray: Result = ReadArray(P, 0, PRTTIArray(TypeInfo)); break;
	case dtDynArray: Result = ReadDynArray(P, 0, PRTTIArray(TypeInfo)); break;
	case dtRecord: Result = ReadRecord(P, PRTTIRecord(TypeInfo)); break;
	case dtClass: Result = ReadClass(P, PRTTIClass(TypeInfo)); break;
	case dtClassOf: Result = ReadPointer(P, PRTTIPointer(TypeInfo)); break;
	case dtInterface: Result = ReadIntf(P, PRTTIInterface(TypeInfo)); break;
	case dtProcType: Result = ReadProcType(P, PRTTIProcType(TypeInfo)); break;
	case dtSet: Result = ReadSet(P, PRTTISet(TypeInfo)); break;
	case dtVariant: Result = ReadVarinat(P, PRTTIVariant(TypeInfo)); break;
  default:
	Result = ReadSimple(P, TypeInfo, Align);
  };
  return Result;
}

CRBase::String TILMachine::ReadVarValue(const PVMVariable Variable) const
{
  PByte VarAddr = PByte(Variable->Addr);
  return ReadValue(VarAddr, Variable->TypeInfo);
}

PVMProc TILMachine::FindProc(const CRBase::String& ProcName, const CRBase::String& UnitName) const
{
  CRBase::String ProcNameLC = LowerCase(ProcName);
  for (int i = 0; i< FUnitsCount; i++)
  {
	PVMUnit UN = &(*FUnits)[i];

	if ((UnitName != EMPTY_STR) && (GetUnitName(UN) != UnitName))
	  continue;

	for (int j = 0; j < UN->ProcsCount; j++)
	{
	  PVMProc ProcInfo = &(*UN->Procs)[j];
	  CRBase::String Name = LowerCase(GetProcName(ProcInfo));

	  if (Name == ProcNameLC)
		return ProcInfo;
	};
  };
  return NULL;
}

PILTypeInfo TILMachine::FindType(const CRBase::String& TypeName, const CRBase::String& UnitName) const
{
  CRBase::String TypeNameLC = LowerCase(TypeName);
  CRBase::String UnitNameLC = LowerCase(UnitName);
  CRBase::String Name;
  int RTTISize;
  for (int i = 0; i < FUnitsCount; i++)
  {
	PVMUnit UN = &(*FUnits)[i];

	CRBase::String CUnitName = LowerCase(GetUnitName(UN));
	if ((UnitName != EMPTY_STR) && (CUnitName != UnitName))
	  continue;

	PILTypeInfo Item = &(*(UN->Types)[0]);
	for (int j = 0; j < UN->TypesCount; j++)
	{
	  Name = LowerCase(GetTypeName(Item));
	  if (Name == TypeNameLC)
		return Item;

	  RTTISize = GetRTTIStructSize(Item->DataTypeID);
	  Item = PILTypeInfo(PByte(Item) + RTTISize);
	};
  };
  return NULL;
}

PVMProc TILMachine::FindMethod(const CRBase::String& TypeName, const CRBase::String& ProcName, const CRBase::String& UnitName) const
{
  PILTypeInfo T = FindType(TypeName, UnitName);
  if (!T)
	return NULL;

  if ((T->DataTypeID != dtRecord) && (T->DataTypeID != dtClass))
	return NULL;

  PVMProc Result = FindProc(TypeName + L_"." + ProcName, UnitName);
  while (!Result && PRTTIStruct(T)->Ancestor)
  {
	T = PILTypeInfo(PRTTIStruct(T)->Ancestor);
	CRBase::String AncTypeName = GetTypeName(PRTTIStruct(T));
	Result = FindProc(AncTypeName + L_"." + ProcName, UnitName);
  };
  return Result;
}

PVMVariable TILMachine::FindVar(const CRBase::String& VarName, const CRBase::String& UnitName) const
{
  CRBase::String VarNameLC = LowerCase(VarName);
  for (int i = 0; i < FUnitsCount; i++)
  {
	PVMUnit UN = &(*FUnits)[i];
	CRBase::String CUnitName = LowerCase(GetUnitName(UN));
	if ((UnitName != EMPTY_STR) && (CUnitName != UnitName))
	  continue;

	for (int j = 0; j < UN->VarsCount; j++)
	{
	  PVMVariable Item = &(*UN->Vars)[j];
	  CRBase::String Name = LowerCase(GetVarName(Item));
      if (Name == VarNameLC)
		return Item;
	};
  };
  return NULL;
}

TRTTICharset TILMachine::GetRTTICharset() const
{
  return TRTTICharset((FHeader->IMGFlags >> 2) & 3);
}

CRBase::String TILMachine::GetString(Pointer Ptr) const
{
  TRTTICharset CharSet = GetRTTICharset();
  switch (CharSet)
  {
	case RTTICharsetASCII: return AnsiToUnicode(CRBase::AnsiString(CRBase::PAnsiChar(Ptr)));
	case RTTICharsetUTF16: return CRBase::String(CRBase::PChar(Ptr));
	default: return EMPTY_STR;
  }
}

CRBase::String TILMachine::GetUnitName(PVMUnit pUnit) const
{
  return GetString(PByte(pUnit->Name));
}

CRBase::String TILMachine::GetProcName(PVMProc pProc) const
{
  CRBase::String Result = GetString(PByte(pProc->Name));
  if (pProc->Struct > 0)
  {
	CRBase::String StructName = GetString(PByte(PILTypeInfo(pProc->Struct)->TypeName));
	Result = Concat(StructName, L_".", Result);
  };
  return Result;
}

CRBase::String TILMachine::GetVarName(PVMVariable pVar) const
{
  return GetString(PByte(pVar->Name));
}

CRBase::String TILMachine::GetTypeName(PILTypeInfo pType) const
{
  return GetString(PByte(pType->TypeName));
}

void TILMachine::MapImportProcedures()
{
  Pointer Ptr;
  CRBase::String LibName, StructName, ProcName;
  PImportTable ImportTable = PImportTable(FHeader->ImportTable);
  for (int i = 0; i < (Int32)FHeader->ImportTableCount; i++)
  {
	PImportEntry ImportProc = &(*ImportTable)[i];
	LibName = GetString(IMGPtr() + ImportProc->LibName);
	ProcName = GetString(IMGPtr() + ImportProc->Name);

	if (ImportProc->Struct == 0)
	{
      // если это глобальная процедура, ищем в списке глобальных процедур
	  Ptr = FindImportProc(LibName, ProcName);
	  if (!Ptr)
		throw Exception(Concat(L_"Import proc '", ProcName, L_"' is not found in '", LibName, L_"' library").c_str());

	  ImportProc->ADDR = TOffset(Ptr);
	} else {
	  // если это метод структуры/класса, ищем в списке типов
      StructName = GetString(IMGPtr() + ImportProc->Struct);
	  if (!ImportProc->IsIntfMethod)
	  {
		PTypeRegInfo RegType = FindImportType(LibName, StructName);
		if (!RegType)
		  throw Exception(("Import type " + UnicodeToAnsi(StructName) + " is not found in library " + UnicodeToAnsi(LibName)).data());
		Ptr = RegType->FindMethod(ProcName);
		if (!Ptr)
		  throw Exception(("Import method " + UnicodeToAnsi(ProcName) + " is not found in library " + UnicodeToAnsi(LibName)).data());
      } else
		Ptr = Pointer(ImportProc->ADDR);
    }

	Ptr = GetInvokeAdapter(ImportProc->Adapter);
	if (!Ptr)
	  throw Exception(UnicodeToAnsi(Concat(L_"Proc ", StructName, L_".", ProcName, L_": Unknown invoke adapter '", std::to_string(ImportProc->Adapter))).data());

	ImportProc->Adapter = TOffset(Ptr);
  };
}

TVMError TILMachine::LoadVMImage(TStream* Stream)
{
  const TIMGFormat SUPPORTED_IMG_FORMAT = IMG_32Bit;

  UInt32 IMGFlags = ReadUInt32(Stream);
  TIMGFormat IMGFormat = TIMGFormat(IMGFlags & 3);

  if (IMGFormat != SUPPORTED_IMG_FORMAT)
	#ifdef USE_EXCEPTIONS
	throw Exception(L_"Image version is not supported");
	#else
	return VM_PREPARE_ERROR;
	#endif

  Int32 IMGBinSize = ReadInt32(Stream);
  (void)IMGBinSize;
  Int32 Size = StreamSize(Stream);
  FMem->SetSize(Size);
  PByte M = IMGPtr();

  StreamPosition(Stream, 0);
  FMem->CopyFrom(Stream, StreamSize(Stream));
  StreamPosition(FMem, 0);

  FHeader = PIMGHeader(M);

  if (FHeader->ImportTableCount > 0)
  {
	FHeader->ImportTable = NativeUInt(M + FHeader->ImportTable);
	MapImportProcedures();
  };

  FUnitsCount = FHeader->UnitsCount;
  StreamPosition(FMem, FHeader->UnitsOffset);
  FUnits = PVMUnits(MemoryPosition(FMem));
  #ifdef USE_EXCEPTIONS
  if (FUnitsCount == 0)
	throw Exception(L_"Invalid package");
  #endif

  for (int i = 0; i < FUnitsCount; i++)
  {
	PVMUnit UN = &(*FUnits)[i];

	UN->Name = NativeUInt(M + UN->Name);

	if (UN->Types !=0)
	  UN->Types = PVMTypes(M + NativeUInt(FHeader->RTTIOffset) + NativeUInt(UN->Types));

	if (UN->Vars !=0)
	  UN->Vars = PVMVariables(M + NativeUInt(UN->Vars));

	if (UN->Procs !=0)
	  UN->Procs = PVMProcedures(M + NativeUInt(FHeader->RTTIOffset) + NativeUInt(UN->Procs));

	// коррекция типов
	if (UN->TypesCount > 0)
	{
	  PILTypeInfo pType = &(*UN->Types)[0];
	  for (int j = 0; j < UN->TypesCount; j++)
	  {
		if (pType->TypeName > 0)
		  pType->TypeName = NativeUInt(M + pType->TypeName);

		TDataTypeID DTID = pType->DataTypeID;
		if ((DTID == dtRecord) || (DTID == dtClass) || (DTID == dtInterface))
		{
		  PRTTIStruct Struct = PRTTIStruct(pType);

		  if (Struct->Ancestor > 0)
		    Struct->Ancestor = NativeUInt(M + NativeUInt(FHeader->RTTIOffset) + Struct->Ancestor);

		  if (Struct->Methods > 0)
		  {
			Struct->Methods = NativeUInt(M + FHeader->RTTIOffset + Struct->Methods);
			for (int k = 0; k < Struct->MethodsCount; k++)
			{
			  PRTTIProcedure Method = &((*PRTTIProcedures(Struct->Methods))[k]);
			  Method->Name = NativeUInt(M + Method->Name);
			};
		  };
		};
		int RTTISize = GetRTTIStructSize(DTID);
		pType = PILTypeInfo(PByte(pType) + RTTISize);
	  };
	};

	// коррекция глобальных переменных
	for (int j = 0; j < UN->VarsCount; j++)
	{
	  PVMVariable pVar = &(*UN->Vars)[j];
	  if (pVar->Name > 0)
		pVar->Name = NativeUInt(M + pVar->Name);

	  pVar->Addr = Pointer(M + NativeUInt(pVar->Addr));
	  pVar->TypeInfo = PILTypeInfo(M + NativeUInt(FHeader->RTTIOffset) + NativeUInt(pVar->TypeInfo));
	  // инициализация значений по умолчанию дин. массивов и строк
	  if (((*PNativeUInt(pVar->Addr)) > 0) &&
		  ((pVar->TypeInfo->DataTypeID == dtAnsiString) || (pVar->TypeInfo->DataTypeID == dtString)))
		*PNativeUInt(pVar->Addr) = NativeUInt(M + *PNativeUInt(pVar->Addr));
	}

	// коррекция секции initialization
	if (UN->InitProc > 0)
	  UN->InitProc = NativeUInt(M + UN->InitProc);

	// коррекция секции finalization
	if (UN->FinalProc > 0)
	  UN->FinalProc = NativeUInt(M + UN->FinalProc);

	// коррекция процедур
	for (int j = 0; j < UN->ProcsCount; j++)
	{
	  PVMProc pProc = &(*UN->Procs)[j];
	  if (pProc->Name > 0)
		pProc->Name = NativeUInt(M + pProc->Name);

	  pProc->Offset = NativeUInt(M + pProc->Offset);

	  if (pProc->Struct > 0)
	    pProc->Struct = NativeUInt(M + FHeader->RTTIOffset + pProc->Struct);
	};
  };
  StreamPosition(FMem, FHeader->IMGSize);
  return VM_OK;
}

template<typename T>
inline void SetCondition(T Result, int& F)
{
if (Result > 0)
	F = (1 << cGreater)|(1 << cGreaterOrEqual)|(1 << cNotEqual)|(1 << cNonZero);
  else if (Result < 0)
	F = (1 << cLess)|(1 << cLeassOrEqual)|(1 << cNotEqual)|(1 << cNonZero);
	else
		F = (1 << cEqual)|(1U << cGreaterOrEqual)|(1 << cLeassOrEqual)|(1 << cZero);
}

static void _VM_PROC_CALL_INDIRECT(Pointer PTR, PByte& SP, PNativeUInt& M)
{
  // вычитываем из инструкции вызова размер стека вызывающей процедуры
  int StackSize = *M;
  // по смещению StackSize сохраняем указатель на свой стек
  *(PPtr)(SP + StackSize) = SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  *(PPtr)(SP + StackSize + PTR_SIZE) = (PByte)M + PTR_SIZE;
  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
  SP = SP + StackSize + PTR_SIZE*2;
  // загружаем указатель на код вызываемой процедуры
  M = (PNativeUInt)PTR;
}

static void _VM_PROC_CALL(PByte& SP, PNativeUInt& M, PByte PS)
{
  // вычитываем из инструкции вызова размер стека вызывающей процедуры
  int StackSize = *(PNInt)(PByte(M) + PTR_SIZE);
  // по смещению StackSize сохраняем указатель на свой стек
  *(PPtr)(SP + StackSize) = SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  *(PPtr)(SP + StackSize + PTR_SIZE) = (PByte)M + PTR_SIZE*2;
  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
  SP = SP + StackSize + PTR_SIZE*2;
  // загружаем указатель на код вызываемой процедуры
  M = (PNativeUInt)(PS + *M);
}


  struct TVMWeakRef {
	Pointer Reference;    // указатель на обьект
	NativeInt RefCount;   // счетчик ссылок
  };
  typedef TVMWeakRef* PVMWeakRef;

  struct TVMObject {
	PRTTIClass TypeInfo;  // указатель на RTTI класса
	NativeInt RefCount;   // счетчик сильных ссылок
	Pointer WeakInfo;     // указатель на мягкую ссылку
	Pointer SyncInfo;     // указатель блок синхронизации
  };
  typedef TVMObject* PVMObject;


#ifdef CPUX64
const int STR_REC_PADDING = 4;
#else
const int STR_REC_PADDING = 0;
#endif
const int STR_REC_SIZE = STR_REC_PADDING + 4 + 4 + 4;
const int STR_LEN_OFFSET = 4;
const int STR_REFCNT_OFFSET = 8;
const int ARR_REFCNT_OFFSET = 4 + PTR_SIZE;

static PNativeUInt _VM_PROC_CALL_VIRTUAL(Pointer PTR, PByte& SP, PNativeUInt& PR, PByte GP)
{
  // вычитываем из инструкции вызова размер своего стека
  int StackSize = *(PNInt)((PByte)PR + PTR_SIZE);
  // по смещению StackSize сохраняем указатель на свой стек
  *(PPtr)(SP + StackSize) = SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  const Int32 CMD_DATA_CNT = 2;
  *(PPtr)(SP + StackSize + PTR_SIZE) = (PByte)PR + PTR_SIZE*CMD_DATA_CNT;
  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
  SP = SP + StackSize + PTR_SIZE*2;
  PTR = PByte(PTR) - sizeof(TObjectHeader); // смещение на TypeInfo
  // загружаем указатель на код вызываемой процедуры
  PVMT VMT = PVMT(GP + PVMObject(PTR)->TypeInfo->VMT);
  return PNativeUInt(GP + (*VMT)[*PR]);
}

static PNativeUInt _VM_PROC_CALL_INTERFACE(Pointer Self, PByte& SP, PNativeUInt& PR, PByte GP)
{
  // вычитываем из инструкции вызова размер своего стека
  int StackSize = *(PNInt)((PByte)PR + PTR_SIZE*2);
  // по смещению StackSize сохраняем указатель на свой стек
  *(PPtr)(SP + StackSize) = SP;
  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
  const Int32 CMD_DATA_CNT = 3;
  *(PPtr)(SP + StackSize + PTR_SIZE) = (PByte)PR + PTR_SIZE*CMD_DATA_CNT;
  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
  SP = SP + StackSize + PTR_SIZE*2;
  // смещение на TypeInfo
  Self = PByte(Self) - sizeof(TObjectHeader);
  PIMTS IMTS = PIMTS(GP + PVMObject(Self)->TypeInfo->IMTS); // получаем спикос таблиц IMT
  Int32 InterfaceID = *PNInt(PR);
  PIMT IMT = PIMT(GP + (*IMTS)[InterfaceID]);               // получаем IMT для данного интерфейса
  Int32 MethodID = *PNInt(PByte(PR) + PTR_SIZE);
  return PNativeUInt(GP + (*IMT)[MethodID]);
}


static void _VM_ARRAY_INCREF(Pointer PTR)
{
  PInt32 pRefCnt = PInt32(NativeUInt(PTR) - ARR_REFCNT_OFFSET);
  if (*pRefCnt > -1)
	InterlockedIncrement((long*)pRefCnt);
}

static void _VM_OBJ_INCREF(Pointer PTR)
{
  if (PTR) {
	PVMObject OBJ = PVMObject(PByte(PTR) - sizeof(TObjectHeader));
	if (OBJ->RefCount > 0)
	  InterlockedIncrement((long*)&(OBJ->RefCount));
  }
}

static void _VM_ARRAY_DECREF(PVMReg Dst)
{
  Pointer PTR = Dst->PTR;
  if (PTR){
	PInt32 pRefCnt = PInt32(PByte(PTR) - ARR_REFCNT_OFFSET);
	if (*pRefCnt > -1) {
	  InterlockedDecrement((long*)pRefCnt);
	  if (*pRefCnt == 0)
        VMFreeMem(PByte(PTR) - ARR_REFCNT_OFFSET);
	};
  };
}

void TILMachine::_VM_ARRAY_DECREF_FINAL(PVMReg Dst, int StackSize, PByte GP, PByte SP, PNativeUInt PR)
{
  Pointer PTR = Dst->PTR;
  if (PTR){
	PInt32 pRefCnt = PInt32(PByte(PTR) - ARR_REFCNT_OFFSET);
	if (*pRefCnt > -1) {
	  InterlockedDecrement((long*)pRefCnt);
	  if (*pRefCnt > 0)
		return;

      // по смещению StackSize сохраняем указатель на свой стек
	  *PPtr(SP + StackSize) = SP;
	  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
	  *PPtr(SP + StackSize + PTR_SIZE) = NULL; // возврат будет в паскаль код!!! иначе PByte(PR) + PTR_SIZE;
	  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
	  PByte NewSP = SP + StackSize + PTR_SIZE*2;
	  *PPtr(NewSP) = PTR;                            // передаем единственные параметр - массив
      CallILProc(PNativeUInt(GP + *PR), NewSP, GP);   // вызываем финализатор
      VMFreeMem(PByte(PTR) - ARR_REFCNT_OFFSET);
	};
  };
}

static void _VM_STR_DECREF(Pointer PTR)
{
  if (PTR){
	PInt32 pRefCnt = PInt32(PByte(PTR) - STR_REFCNT_OFFSET);
	if (*pRefCnt > -1) {
	  InterlockedDecrement((long*)pRefCnt);
	  if (*pRefCnt == 0)
        VMFreeMem(PByte(PTR) - STR_REC_SIZE);
	};
  };
}

static void _VM_WEAK_INCREF(Pointer PTR)
{
  if (PTR) {
	PNativeInt pRefCnt = &(PVMWeakRef(PTR)->RefCount);
	if (*pRefCnt > -1)
	  InterlockedIncrement((long*)pRefCnt);
  };
}

static void _VM_WEAK_DECREF(Pointer PTR)
{
  if (PTR) {
	PNativeInt pRefCnt = &(PVMWeakRef(PTR)->RefCount);
	if (*pRefCnt > -1) {
	  InterlockedDecrement((long*)pRefCnt);
	  if (*pRefCnt == 0) {
		// если обьект еще сущетвует, удаляем у него ссылку на weak
		if (PVMWeakRef(PTR)->Reference)
		  PVMObject(PByte(PVMWeakRef(PTR)->Reference) - sizeof(TObjectHeader))->WeakInfo = NULL;
        VMFreeMem(PTR);
	  };
	};
  };
}

static int _VM_STRU_COMPARE(const PVMReg SrcL, const PVMReg SrcR)
{
  #ifdef USE_UNICODE
  if (SrcL->PTR == SrcR->PTR) return 0;

  // проверка на NULL
  int LLen, RLen;
  if (SrcL->PTR != nullptr) LLen = *PInt32(PByte(SrcL->PTR) - 4); else LLen = 0;
  if (SrcR->PTR != nullptr) RLen = *PInt32(PByte(SrcR->PTR) - 4); else RLen = 0;
  if (SrcL->PTR == nullptr) {if (RLen > 0) return -1; else return 0;};
  if (SrcR->PTR == nullptr) {if (LLen > 0) return +1; else return 0;};

  return wcscmp(PUChar(SrcL->PTR), PUChar(SrcR->PTR));  // todo: нужно сравнение с учетом кейса и локали
  #else
  return 0;
  #endif
}

static int _VM_STRA_COMPARE(const PVMReg SrcL, const PVMReg SrcR)
{
  if (SrcL->PTR == SrcR->PTR) return 0;

  // проверка на NULL
  int LLen, RLen;
  if (SrcL->PTR != nullptr) LLen = *PInt32(PByte(SrcL->PTR) - 4); else LLen = 0;
  if (SrcR->PTR != nullptr) RLen = *PInt32(PByte(SrcR->PTR) - 4); else RLen = 0;
  if (SrcL->PTR == nullptr) {if (RLen > 0) return -1; else return 0;};
  if (SrcR->PTR == nullptr) {if (LLen > 0) return +1; else return 0;};

  return strcmp(PAChar(SrcL->PTR), PAChar(SrcR->PTR));  // todo: нужно сравнение с учетом кейса и локали
}

static int _VM_MEM_COMPARE(Pointer Left, Pointer Right, UInt32 Size)
{
  return memcmp(Left, Right, Size);
}


static Pointer _VM_STRU_CREATE(int Length)
{
  int Size = STR_REC_SIZE + Length*2 + 2;
  PByte Ptr = PByte(VMAllocMem(Size));
  *PInt16(Ptr + STR_REC_PADDING + 0) = 1200;        // code page (utf-16)
  *PInt16(Ptr + STR_REC_PADDING + 2) = 2;           // char size
  *PInt32(Ptr + STR_REC_PADDING + 4) = 1;           // refcount = 1
  *PInt32(Ptr + STR_REC_PADDING + 8) = Length;      // length
  *PInt16(Ptr + Size - 2) = 0;    					// null-term
  return Ptr + STR_REC_SIZE;
}

static Pointer _VM_STRA_CREATE(int Length)
{
  int Size = STR_REC_SIZE + Length + 1;
  PByte Ptr = PByte(VMAllocMem(Size));
  *PInt16(Ptr + STR_REC_PADDING + 0) = 1251;        // code page (ansi)
  *PInt16(Ptr + STR_REC_PADDING + 2) = 1;           // char size
  *PInt32(Ptr + STR_REC_PADDING + 4) = 1;           // refcount = 1
  *PInt32(Ptr + STR_REC_PADDING + 8) = Length;      // length
  *PInt8(Ptr + Size - 1) = 0;     					// null-term
  return Ptr + STR_REC_SIZE;
}

static Pointer _STRU_FROM_STRA(const PVMReg Src)
{

  Pointer SrcPtr = Src->PTR;
  if (!SrcPtr)
	return nullptr;
#ifdef USE_UNICODE
  typedef TBytes* PBytes;
  TBytes Bytes = TEncoding::Convert(TEncoding::ANSI, TEncoding::Unicode, *PBytes(Src->PTR));
  int Len = *PInt32(PByte(SrcPtr) - STR_LEN_OFFSET);
  Pointer Result = _VM_STRU_CREATE(Len);
  _VM_MOVE(Result, &Bytes[0], Len*sizeof(UnicodeChar));
  return Result;
#else
  return nullptr;
#endif
}

static Pointer _STRA_FROM_STRU(const PVMReg Src)
{
  Pointer SrcPtr = Src->PTR;
  if (!SrcPtr)
	return nullptr;
#ifdef USE_UNICODE
  int Len = *PInt32(PByte(SrcPtr) - STR_LEN_OFFSET);
  Pointer Result = _VM_STRA_CREATE(Len);

  CRBase::UnicodeString* UStr = (CRBase::UnicodeString*)(SrcPtr);
  CRBase::AnsiString AStr = CRBase::UnicodeToAnsi(*UStr);
  _VM_MOVE(Result, (void*)AStr.data(), Len*sizeof(AnsiChar));
  return Result;
#else
  return nullptr;
#endif
}

static void _VM_STRU_CONCATE(const PVMReg Dst, const PVMReg SrcL, const PVMReg SrcR)
{
  int L1 = (SrcL->PTR ? (*PInt32(PByte(SrcL->PTR) - 4)) : 0);
  int L2 = (SrcR->PTR ? (*PInt32(PByte(SrcR->PTR) - 4)) : 0);
  int Size = STR_REC_SIZE + L1*2 + L2*2 + 2;

  PByte Ptr = PByte(VMAllocMem(Size));
  *PInt16(Ptr + STR_REC_PADDING + 0) = 1200;        // code page (utf-16)
  *PInt16(Ptr + STR_REC_PADDING + 2) = 2;           // char size
  *PInt32(Ptr + STR_REC_PADDING + 4) = 1;           // refcount = 1
  *PInt32(Ptr + STR_REC_PADDING + 8) = L1 + L2;     // length
  *PInt16(Ptr + Size - 2) = 0;    					// null-term

  Ptr = Ptr + STR_REC_SIZE;
  memcpy(Ptr, SrcL->PTR, L1*2);
  memcpy(Ptr + L1*2, SrcR->PTR, L2*2);
  Dst->PTR = Ptr;
}

static void _VM_STRA_CONCATE(const PVMReg Dst, const PVMReg SrcL, const PVMReg SrcR)
{
  int L1 = (SrcL->PTR ? (*PInt32(PByte(SrcL->PTR) - 4)) : 0);
  int L2 = (SrcR->PTR ? (*PInt32(PByte(SrcR->PTR) - 4)) : 0);
  int Size = STR_REC_SIZE + L1 + L2 + 1;

  PByte Ptr = PByte(VMAllocMem(Size));
  *PInt16(Ptr + STR_REC_PADDING + 0) = 1251;        // code page (utf-16)
  *PInt16(Ptr + STR_REC_PADDING + 2) = 1;           // char size
  *PInt32(Ptr + STR_REC_PADDING + 4) = 1;           // refcount = 1
  *PInt32(Ptr + STR_REC_PADDING + 8) = L1 + L2;     // length
  *PInt8(Ptr + Size - 1) = 0;     					// null-term

  Ptr = Ptr + STR_REC_SIZE;
  memcpy(Ptr, SrcL->PTR, L1);
  memcpy(Ptr + L1, SrcR->PTR, L2);
  Dst->PTR = Ptr;
}

void _VM_OBJ_WEAKREF(Pointer& PTR)
{
  if (PTR) {
	PVMObject Obj = PVMObject(PByte(PTR) - sizeof(TObjectHeader));
	Pointer WeakPtr = Obj->WeakInfo;
	// если у обьекта RefCount = 0 то слабую ссылку на него уже не получить.
	if (!WeakPtr && (Obj->RefCount > 0))
	{
      WeakPtr = VMAllocMem(sizeof(TVMWeakRef));
	  PVMWeakRef(WeakPtr)->RefCount = 1;
	  PVMWeakRef(WeakPtr)->Reference = PTR;
	  Obj->WeakInfo = WeakPtr;
	};
	PTR = WeakPtr;
  };
}

static void _VM_OBJ_STRONGREF(Pointer& PTR)
{
  if (PTR) {
	Pointer Ref = PVMWeakRef(PTR)->Reference;
	if (Ref) {
	  // если слабая ссылка - валидный обьект, то икриментируем счетчик и возвращаем
	  PVMObject Obj = PVMObject(PByte(Ref) - sizeof(TObjectHeader));
	  InterlockedIncrement((long*)&(Obj->RefCount));
	  PTR = Ref;
	  return;
	};
  };
  PTR = NULL;
}

static void _VM_GET_METHOD_PTR(PVMReg Dst, PVMReg Src, PNativeUInt PR, PByte GP)
{
  PVMObject Obj = PVMObject(PByte(Src->PTR) - sizeof(TObjectHeader)); // смещение на TypeInfo
  PVMT VMT = PVMT(GP + Obj->TypeInfo->VMT);
  Dst->PTR = PNativeUInt(GP + (*VMT)[*PR]);
}

const int IL_VMT_FINALIZE = 1;   // индекс финализатора обьекта (код генерируется компилятором)
const int IL_VMT_DESTROY = 0;    // индекс деструктора обьекта (содержимое определяется пользователем)

Pointer TILMachine::_VM_OBJ_CREATE(PNativeUInt M)
{
  PRTTIClass ClassInfo = PRTTIClass(GetRTTIPtr(*M));
  PVMObject Obj = PVMObject(VMAllocMem(ClassInfo->DataSize));
  memset(Obj, 0, ClassInfo->DataSize);
  Obj->TypeInfo = ClassInfo;
  Obj->RefCount = 1;
  return PByte(Obj) + 16;
}

void TILMachine::_VM_OBJ_DECREF(Pointer PTR, int StackSize, PByte GP, PByte SP, PNativeUInt PR)
{
  if (PTR)
  {
	PVMObject OBJ = PVMObject(PByte(PTR) - sizeof(TObjectHeader));
    PInt32 P = (PInt32)&(OBJ->RefCount);
	if (*P >= 1)
	  InterlockedDecrement((long*)P);

	if (*P == 0)
	{
	  // заниливаем ссылку на себя в слабой ссылке (если есть)
	  PVMWeakRef WeakRef = PVMWeakRef(OBJ->WeakInfo);
	  if (WeakRef)
		WeakRef->Reference = NULL;

	  // по смещению StackSize сохраняем указатель на свой стек
	  *PPtr(SP + StackSize) = SP;
	  // по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
	  *PPtr(SP + StackSize + PTR_SIZE) = NULL; // возврат будет в паскаль код, иначе PByte(M) + PTR_SIZE
	  // формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
	  SP = SP + StackSize + PTR_SIZE*2;
	  // получаем указатель на VMT
	  PILTypeInfo TI = PVMObject(OBJ)->TypeInfo;
	  PVMT VMT = PVMT(GP + PRTTIClass(TI)->VMT);
	  *PPtr(SP) = PTR; // передаем self
	  PNativeUInt MethodPtr = PNativeUInt(GP + (*VMT)[IL_VMT_DESTROY]);
      CallILProc(MethodPtr, SP, GP);       // вызываем деструктор
	  if (*PR > 0) {
		MethodPtr = PNativeUInt(GP + *PR);
		CallILProc(MethodPtr, SP, GP);     // вызываем финализатор
	  };
      VMFreeMem(OBJ);          // освобождаем память обьекта
	};
  };
}

static void _CNV_TO_VARIANT(PVMReg Dst, PVMReg Src, TDataTypeID DataTypeID)
{
  void();
/*    case DataTypeID of
	dtInt8: Dst.PVrnt^ := Src.I8;
	dtInt16: Dst.PVrnt^ := Src.I16;
	dtInt32: Dst.PVrnt^ := Src.I32;
	dtInt64: Dst.PVrnt^ := Src.I64;
    dtUInt8: Dst.PVrnt^ := Src.U8;
	dtUInt16: Dst.PVrnt^ := Src.U16;
	dtUInt32: Dst.PVrnt^ := Src.U32;
	dtUInt64: Dst.PVrnt^ := Src.U64;
    dtNativeInt: Dst.PVrnt^ := Src.NInt;
	dtNativeUInt: Dst.PVrnt^ := Src.NUInt;
	dtFloat32: Dst.PVrnt^ := Src.F64;
	dtFloat64: Dst.PVrnt^ := Src.F64;
	dtBoolean: Dst.PVrnt^ := Boolean(Src.I32);
	dtChar: Dst.PVrnt^ := Char(Src.I32);
	dtAnsiChar: Dst.PVrnt^ := AnsiChar(Src.I32);
	dtString: Dst.PVrnt^ := string(Src.PTR);
	dtAnsiString: Dst.PVrnt^ := AnsiString(Src.PTR);
  end; */
}

void _CNV_FROM_VARIANT(PVMReg Dst, PVMReg Src, TDataTypeID DataTypeID)
{
  void();
/*  case DataTypeID of
    dtInt8: Dst.I8 := Src.PVrnt^;
    dtInt16: Dst.I16 := Src.PVrnt^;
    dtInt32: Dst.I32 := Src.PVrnt^;
    dtInt64: Dst.I64 := Src.PVrnt^;
    dtUInt8: Dst.U8 := Src.PVrnt^;
    dtUInt16: Dst.U16 := Src.PVrnt^;
    dtUInt32: Dst.U32 := Src.PVrnt^;
    dtUInt64: Dst.U64 := Src.PVrnt^;
    dtNativeInt: Dst.NInt := Src.PVrnt^;
    dtNativeUInt: Dst.NUInt := Src.PVrnt^;
    dtFloat32: Dst.F64 := Src.PVrnt^;
    dtFloat64: Dst.F64 := Src.PVrnt^;
    dtBoolean: Dst.I32 := Int32(Boolean(Src.PVrnt^));
    dtChar: begin
      s := Src.PVrnt^;
      Dst.I32 := Int32(s[1]);
    end;
    dtAnsiChar: Dst.I32 := Int32(Src.PVrnt^);
    //dtString: Reg.PVrnt^ := Reg.U64;
	//dtAnsiString: Reg.PVrnt^ := Reg.U64;
  end;  */
}

void _VAR_RELEASE(PVMReg Dst)
{
  void();
}

void TILMachine::_VM_SYSMACRO(const TVMMacroID MacroID, PVMReg Dst, PVMReg Src1, PVMReg Src2)
{
  switch (MacroID)
  {
	case vmsmCheckClass: {
	  PRTTIClass DstClass = PRTTIClass(GetRTTIPtr(Src1->NUInt));
	  PRTTIClass SrcClass = PVMObject(PByte(Dst->PTR) - sizeof(TObjectHeader))->TypeInfo;
	  if (ClassInheritsFrom(SrcClass, DstClass))
		Dst->I64 = 1;
	  else
		Dst->I64 = 0;
	  break;
	}
	case vmsmQueryClass: {
	  PRTTIClass DstClass = PRTTIClass(GetRTTIPtr(Src1->NUInt));
	  PRTTIClass SrcClass = PVMObject(PByte(Dst->PTR) - sizeof(TObjectHeader))->TypeInfo;
	  if (!ClassInheritsFrom(SrcClass, DstClass))
		Dst->I64 = 0;
	  break;
	}
	case vmsmCheckIntf: {
	  PRTTIClass SrcClass = PVMObject(PByte(Dst->PTR) - sizeof(TObjectHeader))->TypeInfo;
	  Dst->I64 = ((*PIMTS(GetIMGPtr(SrcClass->IMTS)))[Src1->NUInt] > 0);
	  break;
	}
	case vmsmQueryIntf: {
	  PRTTIClass SrcClass = PVMObject(PByte(Dst->PTR) - sizeof(TObjectHeader))->TypeInfo;
	  PIMTS IMTTable = PIMTS(GetIMGPtr(SrcClass->IMTS));
	  if ((*IMTTable)[Src1->NUInt] == 0)
        Dst->I64 = 0;
	  break;
	}
	case vmsmStrRefCount: Dst->I64 = *PInt32(PByte(Src1->PTR) - STR_REFCNT_OFFSET); break;
	case vmsmArrRefCount: Dst->I64 = *PInt32(PByte(Src1->PTR) - ARR_REFCNT_OFFSET); break;
	case vmsmObjRefCount: Dst->I64 = PVMObject(PByte(Src1->PTR) - sizeof(TObjectHeader))->RefCount; break;
	default: {

	}
  }
}

static void _VM_DYNARRAY_CREATE(const PVMReg Dst, PVMReg Src, NativeUInt ElSize)
{
  Dst->PTR = VMAllocMem(STR_REC_PADDING + 4/*refcnt*/ + PTR_SIZE/*length*/ + Src->U32*ElSize);
  *PUInt32(PByte(Dst->PTR) + STR_REC_PADDING) = 1;                // refcount = 1
  *PNativeUInt(PByte(Dst->PTR) + 4 + STR_REC_PADDING) = Src->U32; // length
  Dst->PTR = PByte(Dst->PTR) + 4 + PTR_SIZE + STR_REC_PADDING;    // adjust the pointer
  memset(Dst->PTR, 0, Src->U32*ElSize);                           // clear memory
}

TVMError TILMachine::CallILProc(PNativeUInt MP /* указатель на код процедры*/,
								PByte SP /* Указатель на стек процедуры */,
								PByte GP /* указатель на память глобальных переменных*/)
{
  const int PTR_SIZE_DIV_4 = 3 - PTR_SIZE / 4;
  (void)PTR_SIZE_DIV_4;
  TVMRegisters Registers;
  #ifdef _DEBUG
  memset(Registers, 0, sizeof(Registers));
  #endif
  int Flags = 0;
  int Idx;
  UInt32 StackSize = 0;
  NativeUInt Instruction;
  TILCondition Cond;
  while (True)
  {
	Instruction = *MP;

	Idx = (Instruction >> 20) & 15;
	PVMReg Dst = &Registers[Idx];

	Idx = (Instruction >> 16) & 15;
	PVMReg Src1 = &Registers[Idx];

	Idx = (Instruction >> 12) & 15;
	PVMReg Src2 = &Registers[Idx];

	Idx = (Instruction >> 24) & 255;

	MP++;
	PNativeUInt PR = MP;
    MP = MP + Idx;

	Cond = TILCondition(UInt8(Instruction >> 8) & 15);
	if ((Cond == cNone) || (((1 << Cond) & Flags) != 0))
	{
	  TILMachineCode Code = TILMachineCode(Instruction & 255);
	  switch (Code){
	  //==================================================================================================
	  // LOAD
	  //==================================================================================================
	  case VM_NOPE: continue;
	  case VM_STACK: StackSize = UInt32(*PR); break;
	  case LD_C_I32: Dst->I64 = Int32(*PR); break;
	  case LD_C_U32: Dst->U64 = UInt32(*PR); break;
	  case LD_C_I64: Dst->I64 = *PInt64(PR); break;
	  case LD_C_F32: Dst->F64 = *PFlt32(PR); break;
	  case LD_C_F64: Dst->F64 = *PFlt64(PR); break;
	  //------------------------------------------------------------------------
	  case LD_L_I8: Dst->I64 = *PInt8(SP + *PR); break;
	  case LD_L_U8: Dst->U64 = *PUInt8(SP + *PR); break;
	  case LD_L_I16: Dst->I64 = *PInt16(SP + *PR); break;
	  case LD_L_U16: Dst->U64 = *PUInt16(SP + *PR); break;
	  case LD_L_I32: Dst->I64 = *PInt32(SP + *PR); break;
	  case LD_L_I64: Dst->I64 = *PInt64(SP + *PR); break;
	  case LD_L_F32: Dst->F64 = *PFlt32(SP + *PR); break;
	  case LD_L_PTR: Dst->PTR = SP + *PR; break;
	  //------------------------------------------------------------------------
	  case LD_R_I8: Dst->I64 = *PInt8(*PPtr(SP + *PR)); break;
	  case LD_R_U8: Dst->U64 = *PUInt8(*PPtr(SP + *PR)); break;
	  case LD_R_I16: Dst->I64 = *PInt16(*PPtr(SP + *PR)); break;
	  case LD_R_U16: Dst->U64 = *PUInt16(*PPtr(SP + *PR)); break;
	  case LD_R_I32: Dst->I64 = *PInt32(*PPtr(SP + *PR)); break;
	  case LD_R_I64: Dst->I64 = *PInt64(*PPtr(SP + *PR)); break;
	  case LD_R_F32: Dst->F64 = *PFlt32(*PPtr(SP + *PR)); break;
	  //------------------------------------------------------------------------
	  case LD_G_I8: Dst->I64 = *PInt8(GP + *PR); break;
	  case LD_G_U8: Dst->U64 = *PUInt8(GP + *PR); break;
	  case LD_G_I16: Dst->I64 = *PInt16(GP + *PR); break;
	  case LD_G_U16: Dst->U64 = *PUInt16(GP + *PR); break;
	  case LD_G_I32: Dst->I64 = *PInt32(GP + *PR); break;
	  case LD_G_I64: Dst->I64 = *PInt64(GP + *PR); break;
	  case LD_G_F32: Dst->F64 = *PFlt32(GP + *PR); break;
	  case LD_G_PTR:
	  case LD_G_PROC: Dst->PTR = Pointer(GP + *PR); break;
	  // загрузка по адресу (разименоваене (D)ereference) хранящимуся в R0
	  case LD_D_I8 : if (Idx > 0) Dst->I32 = *PInt8(PByte(Src1->PTR) + *PR); else Dst->I32 = *PInt8(Src1->PTR); break;
  	  case LD_D_U8 : if (Idx > 0) Dst->U32 = *PUInt8(PByte(Src1->PTR) + *PR); else Dst->U32 = *PUInt8(Src1->PTR); break;
	  case LD_D_I16: if (Idx > 0) Dst->I32 = *PInt16(PByte(Src1->PTR) + *PR); else Dst->I32 = *PInt16(Src1->PTR); break;
	  case LD_D_U16: if (Idx > 0) Dst->U32 = *PUInt16(PByte(Src1->PTR) + *PR); else Dst->U32 = *PUInt16(Src1->PTR); break;
	  case LD_D_I32: if (Idx > 0) Dst->I32 = *PInt32(PByte(Src1->PTR) + *PR); else Dst->I32 = *PInt32(Src1->PTR); break;
	  case LD_D_I64: if (Idx > 0) Dst->I64 = *PInt64(PByte(Src1->PTR) + *PR); else Dst->I64 = *PInt64(Src1->PTR); break;
	  case LD_D_F32: if (Idx > 0) Dst->F64 = *PFlt32(PByte(Src1->PTR) + *PR); else Dst->F64 = *PFlt32(Src1->PTR); break;
	  //==================================================================================================
	  // SET
	  //==================================================================================================
	  case ST_REG_ZERO: Dst->I32 = 0; break;
	  case ST_REG_ONE: Dst->I32 = 1; break;
	  //==================================================================================================
	  // STORE
	  //==================================================================================================
	  case MOVE_L_I32: *PInt32(SP + *PR) = *PInt32(SP + *PNativeUInt(PByte(PR) + PTR_SIZE)); break;
	  case ST_C_I32: *PInt32(SP + *PR) = *PInt32(PByte(PR) + PTR_SIZE); break;
	  case ST_L_I8 : *PInt8(SP + *PR) = Src1->I8; break;
	  case ST_L_I16: *PInt16(SP + *PR) = Src1->I16; break;
	  case ST_L_I32: *PInt32(SP + *PR) = Src1->I32; break;
	  case ST_L_I64: *PInt64(SP + *PR) = Src1->I64; break;
	  case ST_L_F32: *PFlt32(SP + *PR) = static_cast<Float32>(Src1->F64); break;
	  case ST_L_VAR: *PVMVariant(SP + *PR) = *Src1->PVrnt; break;
	  //------------------------------------------------------------------------
	  case ST_R_I8 : *PInt8(*PPtr(SP + *PR)) = Src1->I8; break;
	  case ST_R_I16: *PInt16(*PPtr(SP + *PR)) = Src1->I16; break;
	  case ST_R_I32: *PInt32(*PPtr(SP + *PR)) = Src1->I32; break;
	  case ST_R_I64: *PInt64(*PPtr(SP + *PR)) = Src1->I64; break;
	  case ST_R_F32: *PFlt32(*PPtr(SP + *PR)) = static_cast<Float32>(Src1->F64); break;
	  //------------------------------------------------------------------------
	  case ST_G_I8 : *PInt8(GP + *PR) = Src1->I8; break;
	  case ST_G_I16: *PInt16(GP + *PR) = Src1->I16; break;
	  case ST_G_I32: *PInt32(GP + *PR) = Src1->I32; break;
	  case ST_G_I64: *PInt64(GP + *PR) = Src1->I64; break;
	  case ST_G_F32: *PFlt32(GP + *PR) = static_cast<Float32>(Src1->F64); break;
	  case ST_G_VAR: *PVMVariant(GP + *PR) = *Src1->PVrnt; break;
	  //------------------------------------------------------------------------
	  case ST_D_I8:  if (Idx > 0) *PInt8(PByte(Dst->PTR) + *PR) = Src1->I8;   else *PInt8(Dst->PTR) = Src1->I8; break;
	  case ST_D_I16: if (Idx > 0) *PInt16(PByte(Dst->PTR) + *PR) = Src1->I16; else *PInt16(Dst->PTR) = Src1->I16; break;
	  case ST_D_I32: if (Idx > 0) *PInt32(PByte(Dst->PTR) + *PR) = Src1->I32; else *PInt32(Dst->PTR) = Src1->I32; break;
	  case ST_D_I64: if (Idx > 0) *PInt64(PByte(Dst->PTR) + *PR) = Src1->I64; else *PInt64(Dst->PTR) = Src1->I64; break;
	  case ST_D_F32: if (Idx > 0) *PFlt32(PByte(Dst->PTR) + *PR) = Src1->F64; else *PFlt32(Dst->PTR) = Src1->F64; break;
	  //------------------------------------------------------------------------
	  case MOVE_REG: Dst->U64 = Src1->U64; break;
	  case MOVE_REG_TO_MEM: _VM_MOVE(Dst->PTR, &Src1, *PR); break;
	  //==================================================================================================
	  // CLR
	  //==================================================================================================
	  case CLR_L_I8:  *PInt8(SP + *PR) = 0; break;
	  case CLR_L_I16: *PInt16(SP + *PR) = 0; break;
	  case CLR_L_I32: *PInt32(SP + *PR) = 0; break;
	  case CLR_L_I64: *PInt64(SP + *PR) = 0; break;
	  case CLR_L_F32: *PFlt32(SP + *PR) = 0; break;
	  case CLR_L_F64: *PFlt64(SP + *PR) = 0; break;
	  //------------------------------------------------------------------------
	  case CLR_R_I8 : *PInt8(*PPtr(SP + *PR)) = 0; break;
	  case CLR_R_I16: *PInt16(*PPtr(SP + *PR)) = 0; break;
	  case CLR_R_I32: *PInt32(*PPtr(SP + *PR)) = 0; break;
	  case CLR_R_I64: *PInt64(*PPtr(SP + *PR)) = 0; break;
	  case CLR_R_F32: *PFlt32(*PPtr(SP + *PR)) = 0; break;
	  case CLR_R_F64: *PFlt64(*PPtr(SP + *PR)) = 0; break;
	  //------------------------------------------------------------------------
	  case CLR_G_I8 : *PInt8(GP + *PR) = 0; break;
	  case CLR_G_I16: *PInt16(GP + *PR) = 0; break;
	  case CLR_G_I32: *PInt32(GP + *PR) = 0; break;
	  case CLR_G_I64: *PInt64(GP + *PR) = 0; break;
	  case CLR_G_F32: *PFlt32(GP + *PR) = 0; break;
	  case CLR_G_F64: *PFlt64(GP + *PR) = 0; break;
      //==================================================================================================
	  case CLR_D_I8 : if (Idx > 0) *PInt8(PByte(Dst->PTR) + *PR) = 0; else *PInt8(Dst->PTR) = 0; break;
	  case CLR_D_I16: if (Idx > 0) *PInt16(PByte(Dst->PTR) + *PR) = 0; else *PInt16(Dst->PTR) = 0; break;
	  case CLR_D_I32: if (Idx > 0) *PInt32(PByte(Dst->PTR) + *PR) = 0; else *PInt32(Dst->PTR) = 0; break;
	  case CLR_D_I64: if (Idx > 0) *PInt64(PByte(Dst->PTR) + *PR) = 0; else *PInt64(Dst->PTR) = 0; break;
	  case CLR_D_F32: if (Idx > 0) *PFlt32(PByte(Dst->PTR) + *PR) = 0; else *PFlt32(Dst->PTR) = 0; break;
	  case CLR_D_F64: if (Idx > 0) *PFlt64(PByte(Dst->PTR) + *PR) = 0; else *PFlt64(Dst->PTR) = 0; break;
	  //==================================================================================================
	  // CMP
	  //==================================================================================================
	  case CMP_I32_C: SetCondition(Dst->I32 - Int32(*PR), Flags); break;
	  case CMP_L_C32: SetCondition(*PInt32(SP + *PR) - Int32(*PNativeUInt(PByte(PR) + PTR_SIZE)), Flags); break;
	  case CMP_I32: SetCondition(Dst->I32 - Src1->I32, Flags); break;
	  case CMP_L_I32: SetCondition(*PInt32(SP + *PR) - *PInt32(SP + *PNativeUInt(PByte(PR) + PTR_SIZE)), Flags); break;
	  case CMP_I64: SetCondition(Dst->I64 - Src1->I64, Flags); break;
	  case CMP_F64: SetCondition(Dst->F64 - Src1->F64, Flags); break;
	  case CMP_ASTR: SetCondition(_VM_STRA_COMPARE(Dst, Src1), Flags); break;
	  case CMP_USTR: SetCondition(_VM_STRU_COMPARE(Dst, Src1), Flags); break;
	  //case CMP_VAR: begin SetCondition(Dst->PVrnt^, Src->PVrnt^, Flags); continue; end;
	  case CMP_TEST32: SetCondition(Src1->I32 & Src2->I32, Flags); break;
	  case CMP_TEST64: SetCondition(Src1->I64 & Src2->I64, Flags); break;
	  case CMP_MEM_VS_REG: SetCondition(_VM_MEM_COMPARE(Dst->PTR, Src1, *PR), Flags); break;
	  case CMP_MEM: SetCondition(_VM_MEM_COMPARE(Dst->PTR, Src1->PTR, *PR), Flags); break;
	  //==================================================================================================
	  // ADD
	  //==================================================================================================
	  case INC_L_I32: (*PInt32(SP + *PR))++; break;
	  case ADD_I32_C: Dst->I32 = Src1->I32 + Int32(*PR); break;
	  case ADD_L_I32_C: Dst->I32 = *PInt32(SP + *PR) + *PInt32(PByte(PR) + PTR_SIZE); break;
	  case ADD_NUINT_C: Dst->NUInt = Src1->NUInt + *PR; break;
	  case ADD_U32: Dst->U32 = Src1->U32 + Src2->U32; break;
	  case ADD_U64: Dst->U64 = Src1->U64 + Src2->U64; break;
	  case ADD_I32: Dst->I32 = Src1->I32 + Src2->I32; break;
	  case ADD_L_I32: Dst->I32 = *PInt32(SP + *PR) + *PInt32(SP + *PNativeUInt(PByte(PR) + PTR_SIZE)); break;
	  case ADD_I64: Dst->I64 = Src1->I64 + Src2->I64; break;
	  case ADD_F64: Dst->F64 = Src1->F64 + Src2->F64; break;
	  case ADD_F64_CI32: Dst->F64 = Src1->F64 + Int32(*PR); break;
	  case ADD_ASTR: _VM_STRA_CONCATE(Dst, Src1, Src2); break;
	  case ADD_USTR: _VM_STRU_CONCATE(Dst, Src1, Src2); break;
	  //==================================================================================================
	  // SUB
	  //==================================================================================================
	  case SUB_I32_C: Dst->I32 = Src1->I32 - Int32(*PR); break;
	  case SUB_F64_CI32: Dst->F64 = Src1->F64 - Int32(*PR); break;
	  case SUB_I32: Dst->I32 = Src1->I32 - Src2->I32; break;
	  case SUB_I64: Dst->I64 = Src1->I64 - Src2->I64; break;
	  case SUB_F64: Dst->F64 = Src1->F64 - Src2->F64; break;
	  //==================================================================================================
	  // MUL
	  //==================================================================================================
	  case MUL_C32: Dst->I32 = Src1->I32 * Int32(*PR); break;
	  case MUL_I32: Dst->I32 = Src1->I32 * Src2->I32; break;
	  case MUL_I64: Dst->I64 = Src1->I64 * Src2->I64; break;
	  case MUL_F64: Dst->F64 = Src1->F64 * Src2->F64; break;
	  case FMA_U32: Dst->U32 = Src1->U32 + Src2->U32*(*PR); break;
	  case FMA_U64: Dst->U64 = Src1->U64 + Src2->U64*(*PR); break;
	  //==================================================================================================
	  // IDIV
	  //==================================================================================================
	  case IDIV_U32_C: Dst->U32 = Src1->U32 / (UInt32)*PR; break;
	  case IDIV_I32: Dst->I32 = Src1->I32 / Src2->I32; break;
	  case IDIV_I64: Dst->I64 = Src1->I64 / Src2->I64; break;
	  //==================================================================================================
	  // IMOD
	  //==================================================================================================
	  case IMOD_U32_C: Dst->U32 = Src1->U32 % *PR; break;
	  case IMOD_I32: Dst->I32 = Src1->I32 % Src2->I32; break;
	  case IMOD_I64: Dst->I64 = Src1->I64 % Src2->I64; break;
	  //==================================================================================================
	  // DIV
	  //==================================================================================================
	  case DIV_I32: Dst->F64 = (double)Src1->I32 / Src2->I32; break;
	  case DIV_I64: Dst->F64 = (double)Src1->I64 / Src2->I64; break;
	  case DIV_F64: Dst->F64 = (double)Src1->F64 / Src2->F64; break;
	  //==================================================================================================
	  // NEG
	  //==================================================================================================
	  case NEG_I32: Dst->I32 = -Src1->I32; break;
	  case NEG_I64: Dst->I64 = -Src1->I64; break;
	  case NEG_F64: Dst->F64 = -Src1->F64; break;
	  //==================================================================================================
	  // SHL
	  //==================================================================================================
	  case BIN_SHL32: Dst->U32 = Src1->U32 << Src2->U32; break;
	  case BIN_SHL64: Dst->U64 = Src1->U64 << Src2->U64; break;
	  //==================================================================================================
	  // SHR
	  //==================================================================================================
	  case BIN_SHR32: Dst->U32 = Src1->U32 >> Src2->U32; break;
	  case BIN_SHR64: Dst->U64 = Src1->U64 >> Src2->U64; break;
	  //==================================================================================================
	  // AND
	  //==================================================================================================
	  case BIN_AND32_C: Dst->U32 = Src1->U32 & *PUInt32(PR); break;
	  case BIN_AND64_C: Dst->U64 = Src1->U64 & *PUInt64(PR); break;
	  case BIN_AND32: Dst->U32 = Src1->U32 & Src2->U32; break;
	  case BIN_AND64: Dst->U64 = Src1->U64 & Src2->U64; break;
	  //==================================================================================================
	  // OR
	  //==================================================================================================
	  case BIN_OR32_C: Dst->U32 = Src1->U32 | *PUInt32(PR); break;
	  case BIN_OR64_C: Dst->U64 = Src1->U64 | *PUInt64(PR); break;
	  case BIN_OR32: Dst->U32 = Src1->U32 | Src2->U32; break;
	  case BIN_OR64: Dst->U64 = Src1->U64 | Src2->U64; break;
	  //==================================================================================================
	  // XOR
	  //==================================================================================================
	  case BIN_XOR32: Dst->I32 = Src1->I32 ^ Src2->I32; break;
	  case BIN_XOR64: Dst->I64 = Src1->I64 ^ Src2->I64; break;
	  //==================================================================================================
	  // NOT
	  //==================================================================================================
	  case BIN_NOT32: Dst->U32 = ~ Src1->U32; break;
	  case BIN_NOT64: Dst->U64 = ~ Src1->U64; break;
	  //==================================================================================================
	  case ETHROW: {
		#ifdef USE_EXCEPTIONS
		  CRBase::String s = GetString(Dst->PTR);
		  throw Exception(s.c_str());
		#else
		  return VM_RUN_ASSERT;
		#endif
	  };
	  //------------------------------------------------------------------------
	  case SCHECKB: {
		if ((Dst->NInt < NativeInt(*PR)) || (Dst->NInt > *PNativeInt(PByte(PR) + PTR_SIZE)))
		#ifdef USE_EXCEPTIONS
		  throw Exception(L"Range check error");
		#else
		  return VM_RUN_RANGE_CHECK_ERROR;
		#endif
		break;
	  };
	  //------------------------------------------------------------------------
	  case DCHECKB: {
		if ((Dst->NInt < 0) || (Dst->NUInt >= Src1->NUInt))
		#ifdef USE_EXCEPTIONS
		  throw Exception(L"Range check error");
		#else
		  return VM_RUN_RANGE_CHECK_ERROR;
		#endif
		break;
	  };
	  //------------------------------------------------------------------------
	  case ACHECKB: {
        if ((Src1->NInt < 0) || (Src1->NUInt >= *PNativeUInt(PByte(Dst->PTR) - 4)))
		#ifdef USE_EXCEPTIONS
		  throw Exception(L"Range check error");
		#else
		  return VM_RUN_RANGE_CHECK_ERROR;
		#endif
		break;
	  };
	  //==================================================================================================
	  // DYN ARRAYS and STRINGS
	  //==================================================================================================
	  case ARRAY_LENGTH: if (Src1->PTR) Dst->U64 = *PUInt32((PByte(Src1->PTR) - 4)); else Dst->U64 = 0; break;
	  case ARRAY_INCREF: _VM_ARRAY_INCREF(Dst->PTR); break;
	  case ARRAY_DECREF: if (Idx == 0) _VM_ARRAY_DECREF(Dst); else _VM_ARRAY_DECREF_FINAL(Dst, StackSize, GP, SP, PR); break;
	  case STR_DECREF: _VM_STR_DECREF(Dst->PTR); break;
	  case STRU_CREATE: Dst->PTR = _VM_STRU_CREATE(Src1->I32); break;
	  case STRA_CREATE: Dst->PTR = _VM_STRA_CREATE(Src1->I32); break;
	  //==================================================================================================
	  case OBJ_CREATE: Dst->PTR = _VM_OBJ_CREATE(PR); break;
	  case OBJ_INCREF: _VM_OBJ_INCREF(Dst->PTR); break;
	  case OBJ_DECREF: _VM_OBJ_DECREF(Dst->PTR, StackSize, GP, SP, PR); break;
	  case OBJ_WEAKREF: _VM_OBJ_WEAKREF(Dst->PTR); break;
	  case OBJ_STRONGREF: _VM_OBJ_STRONGREF(Dst->PTR); break;
	  case WEAK_INCREF: _VM_WEAK_INCREF(Dst->PTR); break;
	  case WEAK_DECREF: _VM_WEAK_DECREF(Dst->PTR); break;
	  case VIRT_METHOD_PTR: _VM_GET_METHOD_PTR(Dst, Src1, PR, GP); break;
	  case VM_SET_METHOD_PTR:
		PILMethod(Dst->PTR)->Proc = Src2->PTR;
		PILMethod(Dst->PTR)->Self = Src1->PTR;
		break;
	  //==================================================================================================
	  // RAW MEM ALLOCATE/FREE/SET
	  //==================================================================================================
      case MEM_ALLOC: Dst->PTR = VMAllocMem(*PR); break;
      case MEM__FREE: VMFreeMem(Dst->PTR); break;
	  case MEM_SET: memset(Dst->PTR, int(*PR), Src1->U32); break;
	  case ARRAY_MEM_ALLOC: _VM_DYNARRAY_CREATE(Dst, Src1, *PR); break;
	  case ARRAY_INIT:
		*PUInt32(PByte(Dst->PTR) + STR_REC_PADDING) = 1;               // refcount
		*PUInt32(PByte(Dst->PTR) + STR_REC_PADDING + 4) = UInt32(*PR); // length
		Dst->PTR = PByte(Dst->PTR) + 4 + PTR_SIZE + STR_REC_PADDING;
		break;
	  //==================================================================================================
      // MOVE_MEM
	  //==================================================================================================
	  case MOVE_MEM: _VM_MOVE(Dst->PTR, Src1->PTR, Src2->U32); break;
	  case MOVE_MEM_C: _VM_MOVE(Dst->PTR, Src1->PTR, *PR); break;
	  case MOVE_ARRAY: if (Src1->PTR) _VM_MOVE(Dst->PTR, Src1->PTR, *PUInt32(PByte(Src1->PTR) - 4) * (*PR)); break;
	  //==================================================================================================
	  // CONVERT
	  //==================================================================================================
	  case CNV_F64_S32: Dst->F64 = Dst->I32; break;
	  case CNV_F64_U32: Dst->F64 = Dst->U32; break;
	  case CNV_F64_S64: Dst->F64 = static_cast<Float64>(Dst->I64); break;
	  case CNV_F64_U64: Dst->F64 = static_cast<Float64>(Dst->U64); break;
	  case CNV_ACHR_TO_ASTR: {
		Pointer STR = _VM_STRA_CREATE(1);
		*PAnsiChar(STR) = AnsiChar(Src1->U8);
		Dst->PTR = STR;
		break;
	  }
	  case CNV_UCHR_TO_USTR: {
		Pointer STR = _VM_STRU_CREATE(1);
		*PUnicodeChar(STR) = UnicodeChar(Src1->U16);
		Dst->PTR = STR;
		break;
	  }
	  case CNV_ASTR_TO_USTR: Dst->PTR = _STRU_FROM_STRA(Src1); break;
	  case CNV_USTR_TO_ASTR: Dst->PTR = _STRA_FROM_STRU(Src1); break;
	  case CNV_VAR_TO_VALUE: _CNV_FROM_VARIANT(Dst, Src1, TDataTypeID(*PR)); break;
	  case CNV_VALUE_TO_VAR: _CNV_TO_VARIANT(Dst, Src1, TDataTypeID(*PR)); break;
	  case VAR_RELEASE: _VAR_RELEASE(Dst); break;
      //==================================================================================================
      // CALL обычный вызов процедуры
      //==================================================================================================
	  case CALL_PROC:
	  case CALL_NEAR:
		// вычитываем из инструкции вызова размер стека вызывающей процедуры
		Instruction = *(PNInt)(PByte(PR) + PTR_SIZE);
		// по смещению StackSize сохраняем указатель на свой стек
		*(PPtr)(SP + Instruction) = SP;
		// по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
		*(PPtr)(SP + Instruction + PTR_SIZE) = (PByte)PR + PTR_SIZE*2;
		// формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
		SP = SP + Instruction + PTR_SIZE*2;
		// загружаем указатель на код вызываемой процедуры
		MP = (PNativeUInt)(GP + *PR);
		continue;
	  //==================================================================================================
	  // CALL вызов виртуального метода
	  //==================================================================================================
	  case CALL_VIRT: MP = _VM_PROC_CALL_VIRTUAL(Dst->PTR, SP, PR, GP); continue;
	  case CALL_INTF: MP = _VM_PROC_CALL_INTERFACE(Dst->PTR, SP, PR, GP); continue;
	  //==================================================================================================
	  // CALL косвенный вызов процедуры
	  //==================================================================================================
	  case CALL_INDIRECT:
		// вычитываем из инструкции вызова размер стека вызывающей процедуры
		Instruction = *PR;
		// по смещению StackSize сохраняем указатель на свой стек
		*(PPtr)(SP + Instruction) = SP;
		// по смещению StackSize + SizeOF(Pointer) сохраняем указатель на следующую после CALL инструкцию
		*(PPtr)(SP + Instruction + PTR_SIZE) = (PByte)PR + PTR_SIZE;
		// формируем новый стек равный StackSize + SizeOF(Pointer)*2 (SP + MP)
		SP = SP + Instruction + PTR_SIZE*2;
		// загружаем указатель на код вызываемой процедуры
		MP = (PNativeUInt)Dst->PTR;
        continue;
	  //==================================================================================================
	  case CALL_EXT_FAST: InvokeExternalStatic(FHeader, PR, SP); break;
	  case CALL_EXT_FASTV: InvokeExternalVirtual(MP, SP); break;
      //==================================================================================================
	  case VM_SYSMACRO: _VM_SYSMACRO(TVMMacroID(*PR), Dst, Src1, Src2); break;
	  //==================================================================================================
	  // JUMP
	  //==================================================================================================
	  case VM_JMP:
		MP = PNativeUInt(GP + *PR);
		continue;
	  //==================================================================================================
	  // RET
	  //==================================================================================================
	  case PROC_RET:
		MP = PNativeUInt(*PNativeUInt(SP - PTR_SIZE));
		if (MP != NULL) {
		  PByte PrevSP = (PByte)(*PNativeUInt(SP - PTR_SIZE*2));
		  StackSize = SP - PrevSP - PTR_SIZE*2;
          SP = PrevSP;
		  continue;
		};
		return VM_OK; // елси указатель на память равен nil значит это был корневой вызов, выходим
	  //==================================================================================================
	  default:
		#ifdef USE_EXCEPTIONS
		  throw Exception(L"Unknown VM code: %d", ARRAYOFCONST((Code)));
		#else
		  return VM_RUN_INVALID_INSTRUCTION;
		#endif
	  };
	};
  };
}

TILMachine::TILMachine(PByte Stack, UInt32 StackSize)
{
  FMem = new TILMemoryStream;
  FStack = Stack;
  WriteInt32ToStack(FStack, 0);
  WriteInt32ToStack(FStack, 0);
  FStackSize = StackSize;
  FFreeStackWhenDestroy = False;
}


TILMachine::TILMachine(UInt32 StackSize)
{
  FMem = new TILMemoryStream;
  FStack = new Byte[StackSize];
  #ifdef _DEBUG
  memset(FStack, 0, StackSize);
  #endif
  WriteInt32ToStack(FStack, 0);
  WriteInt32ToStack(FStack, 0);
  FStackSize = StackSize;
  FFreeStackWhenDestroy = True;
}

inline void check_var_assigned(const PVMVariable Var)
{
  #ifdef USE_EXCEPTIONS
  if (!Var) throw Exception(L"Parameter must be assigned");
  #endif
}

void TILMachine::SetVarAsPointer(const PVMVariable Var, const Pointer Value)
{
  check_var_assigned(Var);
  *PPointer(Var->Addr) = Value;
}

void TILMachine::SetVarAsFloat32(const PVMVariable Var, const Float32 Value)
{
  check_var_assigned(Var);
  *(Float32*)Var->Addr = Value;
}

void TILMachine::SetVarAsFloat64(const PVMVariable Var, const Float64 Value)
{
  check_var_assigned(Var);
  *(Float64*)(Var->Addr) = Value;
}

void TILMachine::SetVarAsInt64(const PVMVariable Var, const Int64 Value)
{
  check_var_assigned(Var);
  *PInt64(Var->Addr) = Value;
}

void TILMachine::SetVarAsInt32(const PVMVariable Var, const Int32 Value)
{
  check_var_assigned(Var);
  *PInt32(Var->Addr) = Value;
}

void TILMachine::SetVarAsInt16(const PVMVariable Var, const Int16 Value)
{
  check_var_assigned(Var);
  *PInt16(Var->Addr) = Value;
}

void TILMachine::SetVarAsInt8(const PVMVariable Var, const Int8 Value)
{
  check_var_assigned(Var);
  *PInt8(Var->Addr) = Value;
}

void TILMachine::SetVarAsBool(const PVMVariable Var, const bool Value)
{
  check_var_assigned(Var);
  *PBoolean(Var->Addr) = Value;
}

}// namespace VM
