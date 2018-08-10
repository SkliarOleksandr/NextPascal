unit IL2LLVMTranslator;

interface

uses SysUtils, Forms, ILTranslator, dwsLLVM;

type
  TLLVMTranslator = class(TILTranslator)
  private
    fContext: PLLVMContext;
  protected
    procedure LLVMDLLInitialize;
    procedure LLVMDLLUnInitialize;
    function CreateILUnit(const Name: string): TILUnit; override;
    function CreateILUnitProc(ILUnit: TILUnit): TILProc; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


  TLLVMUnit = class(TILUnit)
  private
    fModule: PLLVMModule;
    fContext: PLLVMContext;
    fTargetData: PLLVMTargetData;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;
  end;

  TLLVMProc = class(TILProc)
  private
    fModule: PLLVMModule;
  public
    procedure MakeFunc;
  end;

implementation


procedure LLVMCheck(const Errors: PAnsiChar);
begin

end;

{ TLLVMTranslator }

constructor TLLVMTranslator.Create;
begin
  inherited;
  LLVMDLLInitialize();
end;

function TLLVMTranslator.CreateILUnit(const Name: string): TILUnit;
begin
  Result := TLLVMUnit.Create(Name);
  TLLVMUnit(Result).fContext := fContext;
end;

function TLLVMTranslator.CreateILUnitProc(ILUnit: TILUnit): TILProc;
begin
  Result := TLLVMProc.Create;
  TLLVMProc(Result).fModule := TLLVMUnit(ILUnit).fModule;
end;

destructor TLLVMTranslator.Destroy;
begin
  LLVMDLLUnInitialize();
  inherited;
end;

procedure TLLVMTranslator.LLVMDLLInitialize;
var
  LibPath: string;
  PassReg: PLLVMPassRegistry;
begin
  LibPath := ExtractFilePath(Application.ExeName);
  LibPath := LibPath + 'LLVM-3.4-x86.dll';
  dwsLLVM.LoadLLVM(LibPath);

  PassReg := LLVMGetGlobalPassRegistry();
  LLVMInitializeCore(PassReg);
  fContext := LLVMGetGlobalContext;
end;

procedure TLLVMTranslator.LLVMDLLUnInitialize;
begin
  //
end;

(*procedure TLLVMTranslator.LLVMInitialize;
var
  Module: PLLVMModule;
  Builder: PLLVMBuilder;
  lConst, lVar, valFunc, Func, CallFunc, str: PLLVMValue;
  Block: PLLVMBasicBlock;
  Context: PLLVMContext;
  Errors: PAnsiChar;
  typeFunc, t: PLLVMType;
  i: Integer;
  name, CPU, triple: ansistring;
  argTypes : array of PLLVMType;
  Target: PLLVMTarget;
  TargetData: PLLVMTargetData;
  TargetMachine: PLLVMTargetMachine;
  Params: array of PLLVMValue;
  AStr: AnsiString;
begin
  LLVMInitializeCore(LLVMGetGlobalPassRegistry());


  Context := LLVMGetGlobalContext;

  name := 'test_module';


  triple := 'i686-pc-win32';

  Module := LLVMModuleCreateWithNameInContext(PansiChar(name), Context);
  LLVMSetTarget(Module, PAnsiChar(triple));
  LLVMSetDataLayout(Module, 'e-p:32:32:32-S32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32');
  Errors := LLVMGetDataLayout(Module);
  TargetData := LLVMCreateTargetData(Errors);


  t := LLVMInt32Type;




  SetLength(argTypes, 1);
  argTypes[0] := LLVMInt8Type;

  typeFunc := LLVMFunctionType(LLVMInt32Type, @argTypes[0], 1, False);
  CallFunc := LLVMAddFunction(Module, 'putchar', typeFunc);
  LLVMSetLinkage(CallFunc, LLVMDLLImportLinkage);
  //LLVMSetFunctionCallConv
  LLVMSetFunctionCallConv(CallFunc, ord(LLVMCCallConv));


  typeFunc := LLVMFunctionType(t, @argTypes[0], 0, False);
  Func := LLVMAddFunction(Module, 'main', typeFunc);


  lConst := LLVMConstInt(LLVMInt32Type, 5, False);

  LLVMSetLinkage(Func, LLVMExternalLinkage);
  LLVMSetFunctionCallConv(Func, ord(LLVMCCallConv));

  Builder := LLVMCreateBuilderInContext(Context);
  Block := LLVMAppendBasicBlockInContext(Context, Func, 'entry');
  LLVMPositionBuilderAtEnd(Builder, Block);


  // code:

  //lVar := LLVMBuildAlloca(Builder, LLVMPointerType(LLVMInt8Type, 0), 'asd');
  //LLVMBuildLoad(Builder, lVar, 'asdasdas');

  //str := LLVMBuildGlobalStringPtr(Builder, 'hello llvm', 'x');
  str := LLVMConstInt(LLVMInt8Type, 65, false);

  SetLength(Params, 1);
  Params[0] := str;

  lVar := LLVMBuildCall(Builder, CallFunc, @Params[0], 1, '');


 // LLVMConstString('hello llvm', 10, False);



  LLVMBuildRet(Builder, lConst);

  LLVMDisposeBuilder(Builder);


  Errors := nil;
  LLVMVerifyModule(Module, LLVMPrintMessageAction, Errors);
  edIR.Lines.Add(Errors);


  //LLVMWriteBitcodeToFile(Module, 'd:\Softbuild\TPC\LLVM\ttt.il');
  Errors := nil;
  Errors := LLVMPrintModuleToString(Module);
  AStr := Errors;
  edIR.Text := AStr;


  // emit code
  LLVMInitializeX86Target;
  LLVMInitializeX86TargetInfo;
  LLVMInitializeX86TargetMC;
  LLVMInitializeX86AsmPrinter;
  LLVMInitializeX86AsmParser;
  LLVMInitializeX86Disassembler;

//  LLVMInitializeNativeTarget;

  Target := LLVMGetFirstTarget;
  repeat
    CPU := LLVMGetTargetName(Target);
    if CPU = 'x86' then
       Break;
    Target := LLVMGetNextTarget(Target);
  until Target = nil;

   TargetMachine := LLVMCreateTargetMachine(Target, PAnsiChar(triple),
      PAnsiChar(CPU), PAnsiChar(''), LLVMCodeGenLevelDefault,
      LLVMRelocDefault, LLVMCodeModelDefault);

   try
      Errors := nil;
      LLVMTargetMachineEmitToFile(TargetMachine, Module,
         PAnsiChar(fRootPath + 'ttt.obj'), TLLVMCodeGenFileType.LLVMObjectFile, Errors);
      edIR.Lines.Add(Errors);
      LLVMTargetMachineEmitToFile(TargetMachine, Module,
         PAnsiChar(fRootPath + 'ttt.asm'), TLLVMCodeGenFileType.LLVMAssemblyFile, Errors);
   finally
     LLVMDisposeTargetMachine(TargetMachine);
   end;
end;*)

{ TLLVMUnit }

const
  cTargetTriple = 'i686-pc-win32';
  cDataLayout = 'e-p:32:32:32-S32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32';

constructor TLLVMUnit.Create(const Name: string);
begin
  fModule := LLVMModuleCreateWithNameInContext(PAnsiChar(Name), fContext);
  LLVMSetTarget(fModule, PAnsiChar(cTargetTriple));
  LLVMSetDataLayout(fModule, cDataLayout);
  LLVMCheck(LLVMGetDataLayout(fModule));
  fTargetData := LLVMCreateTargetData('');
end;

destructor TLLVMUnit.Destroy;
begin

  inherited;
end;

{ TLLVMProc }

procedure TLLVMProc.MakeFunc;
var
  Builder: PLLVMBuilder;
  lConst, lVar, valFunc, Func, CallFunc, str: PLLVMValue;
  Block: PLLVMBasicBlock;
  Errors: PAnsiChar;
  typeFunc, t: PLLVMType;
  i: Integer;
  name, CPU, triple: ansistring;
  argTypes : array of PLLVMType;
  Target: PLLVMTarget;
  TargetData: PLLVMTargetData;
  TargetMachine: PLLVMTargetMachine;
  Params: array of PLLVMValue;
begin
  t := LLVMInt32Type;
  SetLength(argTypes, 1);
  argTypes[0] := LLVMInt8Type;

  typeFunc := LLVMFunctionType(LLVMInt32Type, @argTypes[0], 1, False);
  CallFunc := LLVMAddFunction(fModule, 'putchar', typeFunc);
  LLVMSetLinkage(CallFunc, LLVMDLLImportLinkage);
  //LLVMSetFunctionCallConv
  LLVMSetFunctionCallConv(CallFunc, ord(LLVMCCallConv));

  Func := LLVMAddFunction(fModule, 'main', typeFunc);
end;

end.
