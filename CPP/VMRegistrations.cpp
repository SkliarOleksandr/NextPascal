#include "VMRegistrations.h"
#include "ILMachineInvoke.h"
using namespace std;
using namespace VM;

void XCreateGUID(TGUID& GUID)
{
  //CreateGUID(GUID);
}

void RegisterStandartTestProcs()
{
  VM::RegisterProc(L_"system", L_"CreateGUID", (void*)&XCreateGUID);
}
