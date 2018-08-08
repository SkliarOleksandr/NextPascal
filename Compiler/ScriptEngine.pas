unit ScriptEngine;

interface

uses SysUtils, Classes, StrUtils, NPCompiler, NPCompiler.Classes, NPCompiler.Intf,
     IL2VMTranslator, VM.Core, VM.Invoke;

type
  TScriptEngine = class
  private
    FPackage: INPPackage;
    FVM: TILMachine;
    FUnit: TNPUnit;
    FText: string;
    procedure CheckCRes(const CompilerResult: ICompilerMessages);
  public
    constructor Create;
    destructor Destroy; override;
    function AddConstant(const Name, Value: string; const TypeName: string = ''): Integer;
    function AddType(const Name, Declaration: string): Integer;
    function AddTypeCopy(const Name, Declaration: string): Integer;
    function AddDelphiFunction(const Declaration: string; Ptr: Pointer = nil): Integer;
    function AddInterface(const Name: string; const Parent: string = ''): TTypeRegInfo;
    function FindInterface(const Name: string): TTypeRegInfo;
    function Compile: Boolean;
    property Text: string read FText write FText;
    procedure Run;
  end;

implementation

uses NPCompiler.Package;

{ TScriptEngine }

function TScriptEngine.AddConstant(const Name, Value: string; const TypeName: string = ''): Integer;
begin
  CheckCRes(FUnit.CompileSource(usInterface, 'const ' + Name + IfThen(TypeName <> '', ':' + TypeName, '') + '=' + Value + ';'));
  Result := 0;
end;

function CheckEndSemicolon(const Decl: string): string;
var
  TrimDecl: string;
begin
  TrimDecl := Trim(Decl);
  if Decl[High(TrimDecl)] = ';' then
    Result := TrimDecl
  else
    Result := TrimDecl + ';';
end;

function TScriptEngine.AddDelphiFunction(const Declaration: string; Ptr: Pointer = nil): Integer;
var
  Decl: TIDProcedure;
begin
  CheckCRes(FUnit.CompileProcDecl(usInterface, CheckEndSemicolon(Declaration) + 'external ''SYS'';', Decl));
  if Assigned(Ptr) then
    RegisterProc('SYS', Decl.Name, Ptr);
  Result := 0;
end;

function TScriptEngine.AddInterface(const Name: string; const Parent: string = ''): TTypeRegInfo;
var
  P: TTypeRegInfo;
begin
  P := FindType('SYS', Parent);
  Result := RegisterType('SYS', Name, P);
end;

function TScriptEngine.AddType(const Name, Declaration: string): Integer;
begin
  CheckCRes(FUnit.CompileSource(usInterface, 'type ' + Name + '=' + CheckEndSemicolon(Declaration)));
  Result := 0;
end;

function TScriptEngine.AddTypeCopy(const Name, Declaration: string): Integer;
begin
  CheckCRes(FUnit.CompileSource(usInterface, 'type ' + Name + '=' + CheckEndSemicolon(Declaration)));
  Result := 0;
end;

procedure TScriptEngine.CheckCRes(const CompilerResult: ICompilerMessages);
begin
  if CompilerResult.HasErrors then
    raise Exception.CreateFmt('Compile ERROR: %s', [CompilerResult.Text]);
end;

function TScriptEngine.Compile: Boolean;
var
  VMT: TVMTranslator;
  ILStream: TMemoryStream;
  VMStream: TMemoryStream;
begin
  CheckCRes(FUnit.CompileSource(usImplementation, FText));

  ILStream := TMemoryStream.Create;
  VMStream := TMemoryStream.Create;

  FPackage.SaveToStream(ILStream);
  ILStream.Position := 0;

  VMT := TVMTranslator.Create;
  try
    VMT.LoadILCode(ILStream);
    VMT.SaveTargetCode(VMStream);
    VMStream.Position := 0;
    FVM.LoadVMImage(VMStream);
  finally
    VMT.Free;
  end;

  ILStream.Free;
  VMStream.Free;

  Result := True;
end;

constructor TScriptEngine.Create;
begin
  FPackage := TNPPackage.Create('');
  FUnit := TNPUnit.Create(FPackage, '');
  FPackage.AddUnit(FUnit, nil);
  FVM := TILMachine.Create();
end;

destructor TScriptEngine.Destroy;
begin
  FVM.Free;
  inherited;
end;

function TScriptEngine.FindInterface(const Name: string): TTypeRegInfo;
begin
  Result := FindType('SYS', Name);
end;

procedure TScriptEngine.Run;
begin
  FVM.Run;
end;

end.
