#pragma hdrstop

#ifdef __BORLANDC__
#pragma argsused
#endif

#include <iostream>
#include <stdio.h>
#include <cstring>
#include <exception>
#include <chrono>
#include <iomanip>

#include "CRVMTestMain.h"
#include "CRUtils.h"
#include "ILMachine.h"
#include "CRStreams.h"
#include "VMRegistrations.h"
#include "ILMachineInvoke.h"

using namespace std;
using namespace VM;

static bool Silent = false;

#ifdef __BORLANDC__
System::String ExceptMsg(const Exception& E)
{
  return E.Message;
}
#else
const char * ExceptMsg(const Exception& E)
{
  return E.what();
}
#endif

void ShowVMOut(TILMachine* VM)
{
  PVMUnit pUnit;
  PVMVariable pVar;
  for (int i = 0; i < VM->UnitsCount(); i++)
  {
	  pUnit = &(*VM->Units())[i];
	  if (pUnit->VarsCount > 0)
	  {
		CRBase::String s = VM->GetUnitName(pUnit);
		cout << "------------------" << endl;
		wcout << Concat(L_"UNIT: ", s) << endl;
		cout << "------------------" << endl;
		cout << "GLOBAL VARS:" << endl;
		cout << endl;
		for (int j = 0; j < pUnit->VarsCount; j++){
		  pVar = &(*pUnit->Vars)[j];
		  CRBase::String s = VM->GetVarName(pVar);
		  wcout << Concat(s, L_" = ", VM->ReadVarValue(pVar)) << endl;
		}
	  };
  };
 cout << "------------------" << endl;
}

void Run(const _TCHAR* FilePath)
{
  TFileStream* Stream = NULL;
  
  try {
    Stream = new TFileStream(FilePath, fmOpenRead);
  }
  catch(Exception& e) {
	wcout << L"File open ERROR: " << ExceptMsg(e) << endl;
    return;
  }

  auto final_action = finally([&]{ delete Stream; });

  try
  {
    RegisterStandartInvokeAdapters();
    RegisterStandartTestProcs();
	using clock = std::chrono::high_resolution_clock;
	using us = std::chrono::microseconds;
	TILMachine* M = new TILMachine(65536);
	M->LoadVMImage(Stream);
	auto dt1 = clock::now();
    M->Run();
	auto dt2 = clock::now();
    if(!Silent) ShowVMOut(M);
	auto dur = dt2 - dt1;
	auto tp = std::chrono::duration_cast<us>(dur);
    if (!Silent) wcout << L"Run at: " << tp.count() << L_" us" << endl;
  }
  catch(Exception& e) {
	wcout << L"VM run ERROR: " << ExceptMsg(e) << endl;
  }
  catch(...) {
    cout << "Unknown exception!" << endl;
  }
}

int _tmain(int argc, _TCHAR* argv[])
{
  if(argc > 2) {
    const _TCHAR* s1 = argv[2];
    const _TCHAR* s2 = L"s";
    Silent = (wcscmp(s1, s2) == 0);
  }

  if(!Silent) {
    cout << "==========================" << endl;
    cout << "====== CORE VM TEST ======" << endl;
    cout << "==========================" << endl;
  }
  if(argc <= 1) {
    cout << "No VM image file specified. Please specify VM image file." << endl;
    return 0;
  }
  const _TCHAR* FilePath = argv[1];
  if(!Silent) wcout << L"Execute VM image file: " << FilePath << endl;
  Run(FilePath);
  if(!Silent) {
	cout << "Done." << endl;
  }
  if (!Silent)
    system("PAUSE");
  return 0;
}
