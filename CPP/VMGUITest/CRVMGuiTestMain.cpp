//---------------------------------------------------------------------------

#include <vcl.h>
#include <Xml.XMLDoc.hpp>
#include <Xml.XMLIntf.hpp>

#pragma hdrstop

#include "CRVMGuiTestMain.h"
#include "ILMachineInvoke.h"
#include "VMRegistrations.h"
#include "VM_System.h"


//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

using namespace std;
using namespace CRBase;

void __fastcall TfrmMain::ShowVMOut(TILMachine* VM)
{
  PVMUnit pUnit;
  PVMVariable pVar;
  Memo1->Clear();
  for (int i = 0; i < VM->UnitsCount(); i++)
  {
	  pUnit = &(*VM->Units())[i];
	  if (pUnit->VarsCount > 0)
	  {
		CRBase::String s = VM->GetUnitName(pUnit);
		Memo1->Lines->Add("------------------");
		Memo1->Lines->Add(Concat(L_"UNIT: ", s).c_str());
		Memo1->Lines->Add("------------------");
		Memo1->Lines->Add("GLOBAL VARS:");
		Memo1->Lines->Add("");
		for (int j = 0; j < pUnit->VarsCount; j++){
		  pVar = &(*pUnit->Vars)[j];
		  CRBase::String s = VM->GetVarName(pVar);
		  Memo1->Lines->Add(Concat(s, L_" = ", VM->ReadVarValue(pVar)).c_str());
		}
	  };
  };
  Memo1->Lines->Add("------------------");
};

void p1()
{
  ShowMessage("asdfg");
}

void p2(int i)
{
  ShowMessage("asdfg i =  " + IntToStr(i));
}

void p3(int i, int j)
{
  ShowMessage(Concat(L_"asdfg i = ", to_string(i), L_" j = ", to_string(j)).c_str());
}


void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  //TMemoryStream str;
  VM_System::Register();
  RegisterStandartInvokeAdapters();
  RegisterStandartTestProcs();
  RegisterProc(L_"SYS", L_"p1", &p1);
  RegisterProc(L_"SYS", L_"p2", &p2);
  RegisterProc(L_"SYS", L_"p3", &p3);
}

//---------------------------------------------------------------------------
void __fastcall TfrmMain::Button1Click(TObject *Sender)
{
	if (!OpenDialog1->Execute(this->Handle))
	  return;

	TFileStream* Stream = new TFileStream(OpenDialog1->FileName, fmOpenRead);
	auto final_action = finally([&]{ delete Stream; });
	TILMachine* M = new TILMachine(65536);
	M->LoadVMImage(Stream);
	TDateTime dt = Now();
	try
	{
		PVMProc PP = M->FindMethod(L_"TC1", L_"SetData1");
		M->RunInitSections();
		ShowVMOut(M);
		M->RunFinalSections();
		Memo1->Lines->Add("Run at: " + FormatDateTime("HH:NN:SS.ZZZ", Now() - dt));
	}
	catch(...)
	{
		ShowVMOut(M);
		Memo1->Lines->Add("Run at: " + FormatDateTime("HH:NN:SS.ZZZ", Now() - dt));
	}
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::Button2Click(TObject *Sender)
{
  if (!OpenDialog1->Execute(this->Handle))
	return;

  TFileStream* Stream = new TFileStream(OpenDialog1->FileName, fmOpenRead);
  try
  {
	Byte Stk[1024];

	memset(Stk, 0, 1024);

	TILMachine* M = new TILMachine(Stk, sizeof(Stk));
	M->LoadVMImage(Stream);


	PVMProc Proc = M->FindProc(L_"Test");
	if (!Proc) {
	  delete Stream;
	  return;
	}

	TDateTime dt = Now();
	for (int i = 0;i<1000;i++)
	{
	  M->RunProc(Proc, Pointer(100));
	}
	ShowVMOut(M);
	System::String Str = FormatDateTime("HH:NN:SS.ZZZ", Now()- dt);
	Memo1->Lines->Add("Total time: " + Str);
	delete Stream;
  }
  catch(Exception* e)
  {
	delete Stream;
	throw;
  }
}

   class TMyStream: public TMemoryStream
   {
   public:
	 UInt64 ReadStretchUInt();
	 void WriteStretchUInt(const UInt64 Value);
   };

   UInt64 TMyStream::ReadStretchUInt()
   {
	  Byte B;
	  // 0-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Int64 Result = (B & 127);
	  if (!(B & 128))
		return Result;
	  // 1-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + ((B & 127) << 7);
	  if (!(B & 128))
		return Result;
	  // 2-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + ((B & 127) << 14);
	  if (!(B & 128))
		return Result;
	  // 3-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + ((B & 127) << 21);
	  if (!(B & 128))
		return Result;
	  // 4-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + (UInt64(B & 127) << 28);
	  if (!(B & 128))
		return Result;
	  // 5-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + (UInt64(B & 127) << 35);
	  if (!(B & 128))
		return Result;
	  // 6-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + (UInt64(B & 127) << 42);
	  if (!(B & 128))
		return Result;
	  // 7-Byte ////////////////////////////////////////////////////
	  Read(&B, 1);
	  Result = Result + (UInt64(B) << 49); // в последнем байте учитываются все 8 бит
	  return Result;
   }

   void TMyStream::WriteStretchUInt(const UInt64 Value)
   {
	  Byte Bytes[8];
	  int ByteSize = 0;
	  if (Value > (UInt64(1) << 56))  // max value 72_057_594_037_927_985
		throw Exception("Value %d is to big for StretchUInt64", Value);
	  // 0-Byte ////////////////////////////////////////////
	  while (true) {
		Bytes[0] = (Value & 127);
		if (Value < (1 << 7)){
		  ByteSize = 1;
		  break;
		};
		Bytes[0] = Bytes[0] | 128;
		// 1-Byte ////////////////////////////////////////////////////
		Bytes[1] = (Value & (127 << 7)) >> 7;
		if (Value < (1 << 14)){
		  ByteSize = 2;
		  break;
		};
		Bytes[1] = Bytes[1] | 128;
		/// 2-Byte ///////////////////////////////////////////////////
		Bytes[2] = (Value & (127 << 14)) >> 14;
		if (Value < (1 << 21)){
		  ByteSize = 3;
		  break;
		};
		Bytes[2] = Bytes[2] | 128;
		// 3-Byte ////////////////////////////////////////////////////
		Bytes[3] = (Value & (127 << 21)) >> 21;
		if (Value < (1 << 28)){
		  ByteSize = 4;
		  break;
		};
		Bytes[3] = Bytes[3] | 128;
		// 4-Byte ////////////////////////////////////////////////////
		Bytes[4] = (Value & (UInt64(127) << 28)) >> 28;
		if (Value < (UInt64(1) << 35)){
		  ByteSize = 5;
		  break;
		};
		Bytes[4] = Bytes[4] | 128;
		// 5-Byte ////////////////////////////////////////////////////
		Bytes[5] = (Value & (UInt64(127) << 35)) >> 35;
		if (Value < (UInt64(1) << 42)){
		  ByteSize = 6;
		  break;
		};
		Bytes[5] = Bytes[5] | 128;
		// 6-Byte ////////////////////////////////////////////////////
		Bytes[6] = (Value & (UInt64(127) << 42)) >> 42;
		if (Value < (UInt64(1) << 49)){
		  ByteSize = 7;
		  break;
		};
		Bytes[6] = Bytes[6] | 128;
		// 7-Byte ////////////////////////////////////////////////////
		Bytes[7] = (Value & (UInt64(255) << 49)) >> 49;  // в последнем байте учитываются все 8 бит
		ByteSize = 8;
		break;
	  };
	  Write(&Bytes[0], ByteSize);
   }

//---------------------------------------------------------------------------

void __fastcall TfrmMain::Button3Click(TObject *Sender)
{
  TMyStream* str = new TMyStream();
  str->Size = 8;
  UInt64 A, B, C = 0;
  TDateTime dt = Now();
  Memo1->Lines->BeginUpdate();
  for (int x = 0; x < 100; x++)
  {
	for (int i = 0; i <= 56; i++)
	{
	  for (int j = 1; j<= 255; j++)
	  {
		A = UInt64(j) << i;
		if (A > (UInt64(1) << 56))
		  continue;

		str->Position = 0;
		str->WriteStretchUInt(A);
		str->Position = 0;
		B = str->ReadStretchUInt();
		C = C + 1 + (A - B);
/*		if (A == B)
		  Memo1->Lines->Add("perform: " + IntToStr(Int64(A)) + " = " + IntToStr(Int64(B)) + "    1");
		else
		  Memo1->Lines->Add("perform: " + IntToStr(Int64(A)) + " = " + IntToStr(Int64(B)) + "    0");
		  */


	  }
	}
  };
  Memo1->Lines->Add("time: " + FormatDateTime("NN:SS.ZZZ", Now() - dt) + "  " + IntToStr(Int64(C)));
  Memo1->Lines->EndUpdate();
}
//---------------------------------------------------------------------------



PVMProc TfrmMain::FindProcForType(const System::String TypeName)
{
   for (int i = 0; i < Types.size(); i++)
   {
	 if (Types[i].Name == TypeName)
	   return Types[i].ExecProc;
   }
   return nullptr;
}

PByte TfrmMain::FindObject(const System::String ObjName)
{
   for (int i = 0; i < Objects.size(); i++)
   {
	 if (Objects[i].Name == ObjName)
	   return Objects[i].Ptr;
   }
   return nullptr;
}

void __fastcall TfrmMain::Button4Click(TObject *Sender)
{
	if (!OpenDialog1->Execute(this->Handle))
	  return;

	TFileStream* Stream = new TFileStream(OpenDialog1->FileName, fmOpenRead);
	auto final_action = finally([&]{ delete Stream; });
	TILMachine* M = new TILMachine(65536);
	M->LoadVMImage(Stream);

	System::UnicodeString XMLDocFileName = ChangeFileExt(OpenDialog1->FileName, ".xml");
	auto Doc = LoadXMLDocument(XMLDocFileName);
	auto RNode = Doc->ChildNodes->FindNode("mpc_project");
	auto TNode = RNode->ChildNodes->FindNode("types");
	auto ONode = RNode->ChildNodes->FindNode("objects");
	auto DNode = RNode->ChildNodes->FindNode("devices");

	int i = 0;
    int j = 0;
	CRBase::String StdStrName;
	int IntValue = 0;
	System::String ObjName;
	System::String TypeName;
	System::String FldName;
	System::String FldType;
	System::String FldValue;
    Pointer RawPtr = nullptr;
    TObjRec Obj;
	TDateTime dt = Now();
	try
	{
		// type loading loop
		for (i = 0; i < TNode->ChildNodes->Count; i++)
		{
		  auto TypeNode = TNode->ChildNodes->Nodes[i];
		  int TypeSize = TypeNode->Attributes["size"];
		  System::String TN = TypeNode->Attributes["name"];
		  StdStrName = StringToOleStr(TN);

		  PVMProc Proc = M->FindMethod(StdStrName, L_"Execute");
		  if (Proc) {
			 TObjRec Rec;
			 Rec.Name = TN;
			 Rec.ExecProc = Proc;
			 Types.push_back(Rec);
		  }
		}

		// objects loading loop
		for (i = 0; i < ONode->ChildNodes->Count; i++)
		{
		  auto ObjNode = ONode->ChildNodes->Nodes[i];
		  ObjName = ObjNode->Attributes["name"];
		  TypeName = ObjNode->Attributes["type"];

		  PVMProc Proc = FindProcForType(TypeName);
		  Obj.Name = ObjName;
		  Obj.ExecProc = Proc;
		  RawPtr = AllocMem(256);
		  Obj.Ptr = PByte(RawPtr);
		  memset(Obj.Ptr, 0, 256);
		  Obj.Ptr = Obj.Ptr + 16;
		  Objects.push_back(Obj);
		}

		// objects setting fields loop
		for (i = 0; i < ONode->ChildNodes->Count; i++)
		{
		  auto ObjNode = ONode->ChildNodes->Nodes[i];
		  ObjName = ObjNode->Attributes["name"];
		  Obj = Objects[i];
		  PByte ObjPtr = Obj.Ptr;
		  // fields loading loop
		  auto FieldsNode = ObjNode->ChildNodes->FindNode("fields");
		  for (j = 0; j < FieldsNode->ChildNodes->Count; j++)
		  {
			auto FldNode = FieldsNode->ChildNodes->Nodes[j];
			FldName = FldNode->Attributes["name"];
			FldType = FldNode->Attributes["type"];
			System::OleVariant V = FldNode->Attributes["value"];
			if (!V.IsNull()) {
				FldValue = V;
				if (TryStrToInt(FldValue, IntValue)) {
				  *PInt32(ObjPtr) = 11;//IntValue;
				} else
				{
				  PVMProc Proc = FindProcForType(FldType);
				  if (Proc) {
					RawPtr = (Pointer)FindObject(FldValue);
					if (RawPtr)
					  *PPointer(ObjPtr) = RawPtr;
					else
					  *PPointer(ObjPtr) = nullptr;

				  } else {
					  *PInt32(ObjPtr) = 22;
                  }
				};
			}
			ObjPtr = ObjPtr + 4;
		  };

		};


		// execute loop
		int PassCount = 0;
		while (1) {
			for (i = 0; i < Objects.size(); i++)
			{
			  Obj = Objects[i];
			  NativeUInt Res;
			  try
			  {
				M->RunFunc(Obj.ExecProc, Obj.Ptr, Res);
			  }catch(...){
				 ShowMessage("Error: " + IntToStr(PassCount));
                 return;
              }

			}
			PassCount++;
			Caption = "pass: " + IntToStr(PassCount);
			Application->ProcessMessages();
			Sleep(50);

		}
	}
	catch(...)
	{
		ShowVMOut(M);
		Memo1->Lines->Add("Run at: " + FormatDateTime("HH:NN:SS.ZZZ", Now() - dt));
	}
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::Button5Click(TObject *Sender)
{
	if (!OpenDialog1->Execute(this->Handle))
	  return;

	TFileStream* Stream = new TFileStream(OpenDialog1->FileName, fmOpenRead);
	auto final_action = finally([&]{ delete Stream; });
	TILMachine* M = new TILMachine(65536);
	M->LoadVMImage(Stream);
	TDateTime dt = Now();
	try

	{
		NativeUInt Res;
		void* ObjPtr = AllocMem(64);
		memset(ObjPtr, 0, 64);
		PVMProc PP = M->FindMethod(L_"type1", L_"Execute");
		M->RunFunc(PP, ObjPtr, Res);
		ShowVMOut(M);
		Memo1->Lines->Add("Run at: " + FormatDateTime("HH:NN:SS.ZZZ", Now() - dt));
	}
	catch(...)
	{
		ShowVMOut(M);
		Memo1->Lines->Add("Run at: " + FormatDateTime("HH:NN:SS.ZZZ", Now() - dt));
	}
}
//---------------------------------------------------------------------------


