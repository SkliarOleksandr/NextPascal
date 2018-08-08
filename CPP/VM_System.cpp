//---------------------------------------------------------------------------
#include "VM_System.h"
#include "ILMachineInvoke.h"

using namespace std;
using namespace CRBase;


namespace VM_System
{

const CRBase::String SYS = L_"SYSTEM";

typedef Float64* PExtended;

void F32_TO_Extended(PExtended Result, const Float32 Value)
{
  *Result = Value;
}

void F64_TO_Extended(PExtended Result, const Float64 Value)
{
  *Result = Value;
}


void Register()
{
   auto &Type = VM::RegisterType(SYS, L_"Extended");
   Type.RegisterMethod(L_"ImplicitFromF32", (void*)F32_TO_Extended);
   Type.RegisterMethod(L_"ImplicitFromF64", (void*)F64_TO_Extended);
}
}
