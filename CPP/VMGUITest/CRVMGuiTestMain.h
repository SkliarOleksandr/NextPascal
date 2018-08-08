//---------------------------------------------------------------------------

#ifndef CRVMGuiTestMainH
#define CRVMGuiTestMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Dialogs.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "ILMachine.h"
#include <vector>
//---------------------------------------------------------------------------
using namespace VM;

struct TTypeRec {
	System::String Name;
	PVMProc ExecProc;
};

struct TObjRec {
	System::String Name;
    PByte Ptr;
	PVMProc ExecProc;
};

class TfrmMain : public TForm
{
__published:	// IDE-managed Components
	TOpenDialog *OpenDialog1;
	TPanel *Panel1;
	TButton *Button1;
	TMemo *Memo1;
	TButton *Button2;
	TButton *Button3;
	TButton *Button4;
	TButton *Button5;
	TTimer *Timer1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
private:	// User declarations
	std::vector<TObjRec> Types;
	std::vector<TObjRec> Objects;
	PVMProc FindProcForType(const System::String TypeName);
	PByte FindObject(const System::String ObjName);
public:		// User declarations
    void __fastcall ShowVMOut(TILMachine* VM);
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
