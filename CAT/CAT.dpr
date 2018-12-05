program CAT;

uses
  {$IFDEF DEBUG}
  FastMM4,
  {$ENDIF }
  Vcl.Forms,
  CATMain in 'CATMain.pas' {frmCATMain},
  NPCompiler in '..\Compiler\NPCompiler.pas',
  NPCompiler.Classes in '..\Compiler\NPCompiler.Classes.pas',
  IL2VMTranslator in '..\ILTranslators\VM\IL2VMTranslator.pas',
  VM.Core in '..\VM\VM.Core.pas',
  IL.Instructions in '..\Compiler\IL.Instructions.pas',
  VM.Types in '..\VM\VM.Types.pas',
  VM_INTF in 'VM_INTF.pas',
  VM.Invoke in '..\VM\VM.Invoke.pas',
  NPCompiler.Utils in '..\Compiler\NPCompiler.Utils.pas',
  iDStringParser in '..\Compiler\Parser\iDStringParser.pas',
  NPCompiler.Parser in '..\Compiler\Parser\NPCompiler.Parser.pas',
  IL.TypeInfo in '..\Compiler\IL.TypeInfo.pas',
  SystemUnit in '..\Compiler\SystemUnit.pas',
  NativeCalls in '..\VM\NativeCalls\NativeCalls.pas',
  CATNativeCallsTests in 'CATNativeCallsTests.pas',
  IL.Types in '..\Compiler\IL.Types.pas',
  VM_DateUtils in '..\VM\VMUnits\VM_DateUtils.pas',
  VM_Forms in '..\VM\VMUnits\VM_Forms.pas',
  VM_Math in '..\VM\VMUnits\VM_Math.pas',
  VM_System in '..\VM\VMUnits\VM_System.pas',
  VM_SysUtils in '..\VM\VMUnits\VM_SysUtils.pas',
  NPCompiler.Options in '..\Compiler\NPCompiler.Options.pas',
  NPCompiler.Messages in '..\Compiler\NPCompiler.Messages.pas',
  NPCompiler.Errors in '..\Compiler\NPCompiler.Errors.pas',
  DebugMasterView in '..\Debugger\DebugMasterView.pas' {frmDebugMasterView},
  VMDebuggerCore in '..\Debugger\VMDebuggerCore.pas',
  DebugUnitView in '..\Debugger\DebugUnitView.pas' {frmDebugUnitView},
  ScriptEngine in '..\Compiler\ScriptEngine.pas',
  AVL in '..\Compiler\AVL.pas',
  NPCompiler.DataTypes in '..\Compiler\NPCompiler.DataTypes.pas',
  NPCompiler.Intf in '..\Compiler\NPCompiler.Intf.pas',
  VM_Variants in '..\VM\VMUnits\VM_Variants.pas',
  Platform.IL in '..\Compiler\Platforms\Platform.IL.pas',
  VM_Canvas in 'VM_Canvas.pas',
  ILTranslator in '..\ILTranslators\ILTranslator.pas',
  WASM.Writer in '..\ILTranslators\WASM\WASM.Writer.pas',
  IL2JSTranslator in '..\ILTranslators\JS\IL2JSTranslator.pas',
  IL2WASMTranslator in '..\ILTranslators\WASM\IL2WASMTranslator.pas',
  NPCompiler.Operators in '..\Compiler\NPCompiler.Operators.pas',
  vm.sys.console in '..\VM\VMUnits\vm.sys.console.pas',
  NPCompiler.SysFunctions in '..\Compiler\NPCompiler.SysFunctions.pas',
  VM.Variant in '..\VM\VM.Variant.pas',
  VM.Core.Managed in '..\VM\VM.Core.Managed.pas',
  NPCompiler.Evaluater in '..\Compiler\NPCompiler.Evaluater.pas',
  NPCompiler.Package in '..\Compiler\NPCompiler.Package.pas',
  NPCompiler.Evaluater.VM in '..\Compiler\NPCompiler.Evaluater.VM.pas',
  NPCompiler.ConstCalculator in '..\Compiler\NPCompiler.ConstCalculator.pas',
  IL2LLVMTranslator in '..\ILTranslators\LLVM\IL2LLVMTranslator.pas',
  dwsLLVM in '..\ILTranslators\LLVM\dwsLLVM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCATMain, frmCATMain);
  Application.Run;
end.
