//---------------------------------------------------------------------------

#ifndef ILMachineInvokeH
#define ILMachineInvokeH

#include <vector>
#include <map>
#include "ILMachine.h"
#include "ILMachineTypes.h"

namespace VM {

  enum TInvokeParam {
	_STR = 1,    // unicode string
	_I32 = 2,
	_I64 = 4,
	_F32 = 8,
	_F64 = 16,
	_INF = 32
  };

  typedef int TInvokeParams;

  struct TProcRegInfo
  {
	CRBase::String Name;
	Pointer Proc;
	Pointer Adapter;
  };
  typedef TProcRegInfo* PProcRegInfo;

  typedef std::map<CRBase::String, TProcRegInfo> TVMRegProcs;


  class TTypeRegInfo
  {
	public: CRBase::String Lib;
	public: CRBase::String Name;
	public: TTypeRegInfo* Parent;
	public: TVMRegProcs Methods;
	//Properties: TProperyList
	void RegisterMethod(const CRBase::String Name, Pointer Proc, Pointer Adapter = NULL);
	Pointer FindMethod(const CRBase::String Name);
  };
  typedef TTypeRegInfo* PTypeRegInfo;


void InvokeExternalStatic(PIMGHeader Header, PNativeUInt PM, PByte StackPtr);
void InvokeExternalVirtual(PNativeUInt PM, PByte StackPtr);
Int32 FindInvokeAdapterID(TCallConvention CallConv, Boolean HasResult, TInvokeParams InvokeParams);
void AddInvokeAdapter(TCallConvention CallConv, Boolean HasResult, TInvokeParams InvokeParams, Pointer AdapterProc);

void RegisterStandartInvokeAdapters();
TTypeRegInfo& RegisterType(const CRBase::String Lib, const CRBase::String Name);
void RegisterProc(const CRBase::String Lib, const CRBase::String Name, Pointer Proc, Pointer Adapter = NULL);
Pointer GetInvokeAdapter(Int32 AdapterID);
Pointer FindImportProc(const CRBase::String Lib, const CRBase::String Name);
PTypeRegInfo FindImportType(const CRBase::String Lib, const CRBase::String Name);

}//VM
//---------------------------------------------------------------------------
#endif

