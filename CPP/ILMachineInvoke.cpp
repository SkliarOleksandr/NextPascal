//---------------------------------------------------------------------------
//#include <algorithm>
#include "ILMachineInvoke.h"
#include "CRUtils.h"

namespace VM {
  using namespace CRBase;

  typedef void (*TAdapterProc)(Pointer, Pointer);


  struct TInvokeData{
	TInvokeParams Params;
	Int32 AdapterID;
  };

  typedef TInvokeData* PInvokeData;
  typedef std::vector<TInvokeData>* TInvokeProcs;


  // 1 сегмент - нотация вызова, 2 - есть ли возвращяемое значение
  static TInvokeProcs InvokeProcs[ccSafeCall + 1][2];

  static std::vector<Pointer> InvokeAdapters;

  static std::vector<TProcRegInfo> RegisterProcs;
  static std::map<CRBase::String, TTypeRegInfo> RegisterTypes;

void TTypeRegInfo::RegisterMethod(const CRBase::String Name, Pointer Proc, Pointer Adapter)
{
  CRBase::String Key = LowerCase(Name);

  if(Methods.find(Key) != Methods.end())
	 throw Exception(("Method " + UnicodeToAnsi(Key) + " is already registered.").data());

  TProcRegInfo Item;
  Item.Proc = Proc;
  Item.Adapter = Adapter;
  Methods[Key] = Item;
}

Pointer TTypeRegInfo::FindMethod(const CRBase::String Name)
{
  CRBase::String Key = LowerCase(Name);
  auto Res = Methods.find(Key);
  if (Res != Methods.end())
	return Res->second.Proc;
  else
	return nullptr;
}

Pointer GetInvokeAdapter(Int32 AdapterID)
{
  if ((AdapterID >= 0) && (AdapterID < (Int32)InvokeAdapters.size()))
	return InvokeAdapters[(size_t)AdapterID];
  else
	return nullptr;
}

Pointer FindImportProc(const CRBase::String Lib, const CRBase::String Name)
{
  PProcRegInfo RegInfo;
  CRBase::String FullName = LowerCase(Lib + L_"." + Name);
  for(size_t i = 0; i < RegisterProcs.size(); i++)
  {
	RegInfo = &RegisterProcs[i];

	if (FullName == RegInfo->Name)
	  return RegInfo->Proc;
  }
  return nullptr;
}

PTypeRegInfo FindImportType(const CRBase::String Lib, const CRBase::String Name)
{
   CRBase::String FullName = LowerCase(Lib + L_"." + Name);
   auto Res = RegisterTypes.find(FullName);
   if (Res != RegisterTypes.end()) {
	 PTypeRegInfo Result = &(Res->second);
	 return Result;
   } else
     return nullptr;
}

void InvokeExternalStatic(PIMGHeader Header, PNativeUInt PM, PByte StackPtr)
{
  size_t ImportIndex;
  Pointer Adapter;
  PImportEntry ImportProc;

  ImportIndex = *PM;
  PM++;

  ImportProc = &(*PImportTable(Header->ImportTable))[ImportIndex];

  Adapter = Pointer(ImportProc->Adapter);
  StackPtr = StackPtr + *PM;
  (TAdapterProc(Adapter))(Pointer(ImportProc->ADDR), StackPtr);
}

void InvokeExternalVirtual(PNativeUInt PM, PByte StackPtr)
{
  size_t InvokeAID;
  Pointer Adapter;
  Pointer ProcPtr;

  InvokeAID = *PM;
  PM++;
  Adapter = InvokeAdapters[InvokeAID];

  ProcPtr = Pointer(*PPointer(*PPointer(StackPtr))); // получаем IMT
  ProcPtr = *PPointer(NativeUInt(ProcPtr) + *PM);    // складывая смещение получаем адрес метода

  StackPtr = StackPtr + *PNativeInt(PByte(PM) + PTR_SIZE);
  (TAdapterProc(Adapter))(ProcPtr, StackPtr);
}

Int32 FindInvokeAdapterID(TCallConvention CallConv, Boolean HasResult, TInvokeParams InvokeParams)
{
  PInvokeData Item;
  TInvokeProcs Items;

  Items = InvokeProcs[int(CallConv)][HasResult];

  if (Items)
	for (UInt32 i = 0; i < Items->size(); i++)
	{
	  Item = &(*Items)[i];
	  if (Item->Params == InvokeParams)
		return Item->AdapterID;
    };
  return -1;
}

TTypeRegInfo& RegisterType(const CRBase::String Lib, const CRBase::String Name)
{
  CRBase::String FullName = LowerCase(Lib + L_"." + Name);

  if(RegisterTypes.find(FullName) != RegisterTypes.end())
  {
	 throw Exception(("Type " + UnicodeToAnsi(FullName) + " is already registered.").data());
  }

  TTypeRegInfo Item;
  Item.Lib = Lib;
  Item.Name = Name;

  TTypeRegInfo& Result = (RegisterTypes[FullName] = Item);
  return Result;
}

void RegisterProc(const CRBase::String Lib, const CRBase::String Name, Pointer Proc, Pointer Adapter)
{
  CRBase::String FullName;
  PProcRegInfo RegInfo;
  TProcRegInfo NewRegInfo;
  FullName = LowerCase(Lib + L_"." + Name);

  for (UInt32 i = 0; i < RegisterProcs.size(); i++)
  {
	RegInfo = &RegisterProcs[i];
	if (RegInfo->Name == FullName)
	  throw Exception(("Procedure " + UnicodeToAnsi(FullName) +  " is already registered.").data());
  }

  NewRegInfo.Name = FullName;
  NewRegInfo.Proc = Proc;
  NewRegInfo.Adapter = Adapter;
  RegisterProcs.push_back(NewRegInfo);
}

void AddInvokeAdapter(TCallConvention CallConv, Boolean HasResult, TInvokeParams InvokeParams, Pointer AdapterProc)
{

  PInvokeData Item;
  (void)Item;
  TInvokeProcs Items;
  TInvokeData NewData;

  // поиск существующей декларации
//  if (FindInvokeAdapterID(CallConv, HasResult, InvokeParams) != -1)
//	throw Exception(L"Invoke adapter already registered");

  Items = InvokeProcs[int(CallConv)][HasResult];

  if (!Items) {
	Items = new std::vector<TInvokeData>();
	InvokeProcs[int(CallConv)][HasResult] = Items;
  }

  NewData.Params = InvokeParams;
  NewData.AdapterID = (Int32)InvokeAdapters.size();

  Items->push_back(NewData);
  InvokeAdapters.push_back(AdapterProc);
}

static void _ccAny_proc_0(Pointer Proc, Int32* Params)
{
  (void)Params;
  typedef void (*TProc)();
  (TProc(Proc))();
}

static void _ccAny_proc_i32(Pointer Proc, Int32* Params)
{
  typedef void (*TProc)(Int32);
  (TProc(Proc))(Params[0]);
}

static void _ccAny_proc_i64(Pointer Proc, Int64* Params)
{
  typedef void (*TProc)(Int64);
  (TProc(Proc))(Params[0]);
}

static void _ccAny_proc_i32_i32(Pointer Proc, Int32* Params)
{
  typedef void (*TProc)(Int32, Int32);
  (TProc(Proc))(Params[0], Params[1]);
}


static void _ccReg_proc_I32_F32(Pointer Proc, Int32* Params)
{
  typedef void (*TProc)(Int32, Float32);
  (TProc(Proc))(Params[0], *((Float32*)Params[1]));
}

static void _ccReg_proc_I32_F64(Pointer Proc, Int32* Params)
{
  typedef void (*TProc)(Int32, Float64);
  (TProc(Proc))(Params[0], *((Float64*)Params[1]));
}

void RegisterStandartInvokeAdapters()
{
  AddInvokeAdapter(ccReg, False, 0, (Pointer)&_ccAny_proc_0);
  AddInvokeAdapter(ccReg, False, 0, (Pointer)&_ccAny_proc_i32);
  AddInvokeAdapter(ccReg, False, 0, (Pointer)&_ccAny_proc_i64);
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I32_I32
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I32_I64
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I64_I64
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I32_I32_I32
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I32_I32_I32_I32

  AddInvokeAdapter(ccReg, False, 0, (Pointer)&_ccReg_proc_I32_F32);
  AddInvokeAdapter(ccReg, False, 0, (Pointer)&_ccReg_proc_I32_F64);
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I64_F32
  AddInvokeAdapter(ccReg, False, 0, nullptr); //_ccReg_proc_I64_F64

  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I32
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I64
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I32_I32
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I64_I64
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I32_I32_I32
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I32_I32_I32_I32
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I32_F64_F64
  AddInvokeAdapter(ccReg, True, 0, nullptr); //_ccReg_func_I64_F64_F64

  AddInvokeAdapter(ccReg, True, 0, nullptr);                                        // function: flt32
  AddInvokeAdapter(ccReg, True, 0, nullptr);                                        // function: flt64
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(int32): flt64
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(int64): flt64
  AddInvokeAdapter(ccReg, True, 0, nullptr);                    // function(flt64, int32): flt64
  AddInvokeAdapter(ccReg, True, 0, nullptr);                    // function(flt64, int64): flt64

  AddInvokeAdapter(ccReg, True, 0, nullptr);                                      // function: string
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(int32): string
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(int64): string
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(flt32): string
  AddInvokeAdapter(ccReg, True, 0, nullptr);                              // function(flt64): string

  AddInvokeAdapter(ccReg, True, 0, nullptr);                    // function(int32, int32): string
  AddInvokeAdapter(ccReg, True, 0, nullptr);                    // function(int32, flt64): string
  AddInvokeAdapter(ccReg, True, 0, nullptr);          // function(int32, int32, int32): string

  AddInvokeAdapter(ccReg, True, 0, nullptr);                                     // function: interface
  AddInvokeAdapter(ccReg, True, 0, nullptr);                           // function(int32): interface
  AddInvokeAdapter(ccReg, True, 0, nullptr);                   // function(int32, int32): interface
  AddInvokeAdapter(ccReg, True, 0, nullptr);         // function(int32, int32, int32): interface
  AddInvokeAdapter(ccReg, True, 0, nullptr);  // function(int32, int32, int32, int32): interface


  AddInvokeAdapter(ccReg, True, 0, nullptr);

  AddInvokeAdapter(ccReg, True, 0, nullptr);
  AddInvokeAdapter(ccReg, True, 0, nullptr);
  AddInvokeAdapter(ccReg, True, 0, nullptr);
  AddInvokeAdapter(ccReg, True, 0, nullptr);


}

}//VM
