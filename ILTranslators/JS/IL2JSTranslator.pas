unit IL2JSTranslator;

interface

uses System.SysUtils, System.Classes, System.Math, ILTranslator, NPCompiler.DataTypes, IL.TypeInfo, NPCompiler.Utils, IL.Types;

type
  TJSSource = class
  const
    CSpace = 4;
  private
    FStrings: TStrings;
    FSpacing: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Str: string);
    procedure AddStrings(const Strings: TStrings);
    procedure AddB(const Str: string);
    procedure AddE(const Str: string);
    procedure AddEB(const Str: string);
    procedure InsertB(Index: Integer; const Str: string);
    property Strings: TStrings read FStrings;
    procedure ClearSpacing;
  end;

  TJSProc = class(TILProc)
  private
    FJS: TJSSource;
    FBrContext: TILBranchContext;
    function GetBrContext: PILBranchContext; inline;
  public
    constructor Create;
    destructor Destroy; override;
    property JS: TJSSource read FJS;
    property BrContex: PILBranchContext read GetBrContext;
    function BranchIndex: Integer;
  end;

  TIL2JSTranslator = class(TILTranslator)
  private
    FJS: TJSSource;
    procedure WriteProcsToJS;
    procedure WriteProcToJS(const Proc: TJSProc);
    procedure WriteInitFinalProcsToJS(const UN: TILUnit);
    function GetProcParamsStr(Proc: TILProc): string;
    function GetArgProcName(const Arg: TILArgument): string;
    function GetJSProcName(const Proc: TILProc): string;
  protected
    function CreateILUnitProc(ILUnit: TILUnit): TILProc; override;
    procedure LoadILConsts(ILUnit: TILUnit; Stream: TStream); override;
    procedure WriteTypesRTTIArray(ILUnit: TILUnit); override;
    procedure MapImportMethod(aType: PRTTIType; Proc: TILProc); override;
    procedure MapImportProc(Proc: TILProc); override;
    procedure MakeImportTable; override;
    procedure WriteProcProlog(Proc: TILProc); override;

    procedure BeforeInstruction(const Ctx: TILTContext); override;
    procedure AfterInstruction(const Ctx: TILTContext); override;
    procedure AfterLoadILGlobalVars(ILUnit: TILUnit); override;

    procedure Translate_MOVE(const Ctx: TILTContext; var Args: TIL_DS_Args); override;
    procedure Translate_MOVEZERO(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_RET(const Ctx: TILTContext); override;
    procedure Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS); override;
    procedure Translate_CMP(const Ctx: TILTContext; var Args: TIL_LR_Args); override;
    procedure Translate_TEST(const Ctx: TILTContext; var Args: TIL_LR_Args); override;
    procedure Translate_JMP(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_INC(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_NOPE(const Ctx: TILTContext); override;
    procedure Translate_NOT(const Ctx: TILTContext; var Args: TIL_DS_Args); override;
    procedure Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_INIT(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_DS_Args); override;
    procedure Translate_DECREF(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_INCREF(const Ctx: TILTContext; var Args: TIL_D_Args); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveTargetCode(Stream: TStream); override;

  end;

implementation

uses StrUtils;

type
  TILProcHelper = class helper for TILProc
    function JS: TJSSource; inline;
  end;

  TILCtxHelper = record helper for TILTContext
    function JSProc: TJSProc; inline;
  end;


function GetJSCond(Condition: TILCondition): string;
begin
  case Condition of
    cEqual: Result := '==';
    cNotEqual: Result := '!=';
    cGreater: Result := '>';
    cGreaterOrEqual: Result := '>=';
    cLess: Result := '<';
    cLessOrEqual: Result := '<=';
    cZero: Result := '==0';
    cNonZero: Result := '!=0';
  else
    Result := '';
  end;
end;

function ProcResultVar(const Ctx: TILTContext): string;
begin
  if Ctx.Proc.ProcType = ptFunction then
    Result := 'Result'
  else
    Result := '';
end;

{ TIL2JSTranslator }

function TIL2JSTranslator.GetProcParamsStr(Proc: TILProc): string;
var
  i, si: Integer;
  Param: PRTTIParameter;
begin
  Result := '';
  si := ifthen(Proc.ProcType = ptFunction, 1, 0);
  for i := si to Length(Proc.Params) - 1 do
  begin
    Param := @(Proc.Params[i]);
    Result := AddStringSegment(Result, GetString(Param.Name), ', ');
  end;
end;

procedure TIL2JSTranslator.WriteProcToJS(const Proc: TJSProc);
var
  Params: string;
begin
  Params := GetProcParamsStr(Proc);
  FJS.Add('function ' + GetProcName(Proc) + '(' + Params + ')');
  FJS.AddStrings(Proc.JS.Strings);
  FJS.AddE('}');
end;

procedure TIL2JSTranslator.WriteInitFinalProcsToJS(const UN: TILUnit);
begin
  if UN.InitProc.ILCount > 1 then
  begin
    FJS.Add('function $' + UN.Name + '$initialization()');
    FJS.AddStrings(UN.InitProc.JS.Strings);
    FJS.AddE('}');
  end;

  if UN.FinalProc.ILCount > 1 then
  begin
    FJS.Add('function $' + UN.Name + '$finalization()');
    FJS.AddStrings(UN.FinalProc.JS.Strings);
    FJS.AddE('}');
  end;
end;

procedure TIL2JSTranslator.WriteProcProlog(Proc: TILProc);
begin
  Proc.JS.AddB('{');
end;

procedure TIL2JSTranslator.WriteProcsToJS;
var
  ui, pi: Integer;
  UN: TILUnit;
  P: TILProc;
begin
  for ui := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[ui];
    for pi := 0 to Length(UN.Procs) - 1 do
    begin
      P := UN.Procs[pi];
      if not P.IsImported then
        WriteProcToJS(P as TJSProc);
    end;
    WriteInitFinalProcsToJS(UN);
  end;
  // секция инициализации
  FJS.ClearSpacing;
  FJS.Add('// autorun script');
  FJS.AddB('{');
  for ui := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[ui];
    if UN.InitProc.ILCount > 1 then
      FJS.Add('$' + UN.Name + '$initialization();');
  end;
  FJS.AddE('}');
end;

procedure TIL2JSTranslator.AfterLoadILGlobalVars(ILUnit: TILUnit);
var
  i, c: Integer;
  V: PILVariable;
begin
  c := Length(ILUnit.Vars);
  for i := 0 to c - 1 do
  begin
    V := addr(ILUnit.Vars[i]);
    FJS.Add('let ' + V.Name + ';');
  end;
end;

constructor TIL2JSTranslator.Create;
begin
  inherited;
  FJS := TJSSource.Create();
end;

function TIL2JSTranslator.CreateILUnitProc(ILUnit: TILUnit): TILProc;
begin
  Result := TJSProc.Create;
end;

destructor TIL2JSTranslator.Destroy;
begin
  FJS.Free;
  inherited;
end;

procedure TIL2JSTranslator.LoadILConsts(ILUnit: TILUnit; Stream: TStream);
var
  c: Integer;
begin
  c := Stream.ReadStretchUInt;
  Assert(c = 0);
end;

procedure TIL2JSTranslator.MakeImportTable;
begin
  inherited;

end;

procedure TIL2JSTranslator.MapImportMethod(aType: PRTTIType; Proc: TILProc);
begin
  if (GetString(aType.ImportName) = 'console') and
     (GetProcName(Proc) = 'console.log') then
   Exit;

end;

procedure TIL2JSTranslator.MapImportProc(Proc: TILProc);
begin
  inherited;

end;

procedure TIL2JSTranslator.SaveTargetCode(Stream: TStream);
begin
  inherited;
  WriteProcsToJS();
  FJS.Strings.SaveToStream(Stream);
end;

function ArgToStr(const Arg: TILArgument): string;
begin
  case Arg.ArgClass of
    ARG_CONST: Result := IntToStr(Arg.I64);
    ARG_VAR: Result := Arg.AsVariable.Name;
  else
    Result := '';
  end;
end;

function ILArgsToStr(StartIndex: Integer; const Args: TILArguments): string;
var
  i: Integer;
  Arg: TILArgument;
begin
  Result := '';
  for i := StartIndex to Length(Args) - 1 do
  begin
    Arg := Args[i];
    Result := AddStringSegment(Result, ArgToStr(Arg), ', ');
  end;
end;

function TIL2JSTranslator.GetJSProcName(const Proc: TILProc): string;
var
  LName, PName: string;
begin
  if Proc.IsImported then
  begin
    Result := '';
    LName := LowerCase(GetString(Proc.ImportLib));
    if LName = 'sys.console' then
    begin
      PName := LowerCase(GetString(Proc.ImportName));
      if PName = 'log' then
         Result := 'console.log';
    end;
  end else
    Result := GetProcName(Proc);
end;

function TIL2JSTranslator.GetArgProcName(const Arg: TILArgument): string;
begin
  case Arg.ArgClass of
    ARG_PROC: Result := GetJSProcName(Arg.AsProcedure);
    ARG_METHOD: Result := GetJSProcName(Arg.AsMethod.Proc);
  else
    Result := '';
  end;
end;

procedure TIL2JSTranslator.Translate_CMP(const Ctx: TILTContext; var Args: TIL_LR_Args);
begin
  Ctx.JSProc.BrContex.Add(Args);
end;

procedure TIL2JSTranslator.Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_DS_Args);
begin
  Ctx.Proc.JS.Add(ArgToStr(Args.D) + ' = ' + ArgToStr(Args.S) + ';');
end;

procedure TIL2JSTranslator.Translate_DECREF(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  inherited;

end;

procedure TIL2JSTranslator.Translate_INC(const Ctx: TILTContext; var Args: TIL_D_Args);
var
  JS: TJSSource;
begin
  JS := Ctx.Proc.JS;
  JS.Add(ArgToStr(Args.D) + '++;');
end;

procedure TIL2JSTranslator.Translate_INCREF(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  inherited;

end;

procedure TIL2JSTranslator.Translate_INIT(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  //Ctx.Proc.JS.Add(Args.D);
end;

procedure TIL2JSTranslator.Translate_TEST(const Ctx: TILTContext; var Args: TIL_LR_Args);
begin
  Ctx.JSProc.BrContex.Add(Args);
end;

procedure TIL2JSTranslator.Translate_JMP(const Ctx: TILTContext; var Args: TIL_D_Args);
var
  Br: PILBranch;
  JS: TJSSource;
  JmpIdx: Integer;
begin
  JS := Ctx.Proc.JS;
  JmpIdx := Args.D.I32;
  if Ctx.Cond <> cNone then
  begin
    Br := Ctx.JSProc.BrContex.Last;
    Br.CmpCond := Ctx.Cond;
    if JmpIdx > Ctx.ILIndex then
    begin
      Br.ElseIdx := JmpIdx;
      JS.Add('if (' + ArgToStr(Br.CmpArgs.L) + GetJSCond(InverseCondition(Ctx.Cond)) + ArgToStr(Br.CmpArgs.R) +  ')');
      JS.AddB('{');
    end else begin
      JS.InsertB(JmpIdx + 1, 'do {');
      JS.AddE('} while (' + ArgToStr(Br.CmpArgs.L) + GetJSCond(Ctx.Cond) + ArgToStr(Br.CmpArgs.R) +  ');');
      Ctx.JSProc.BrContex.BrClose;
    end;
  end else begin
    // если идет переход на начало, то это цикл
    if JmpIdx < Ctx.ILIndex then
    begin
      JS.Strings[JmpIdx + 1] := StringReplace(JS.Strings[JmpIdx + 1], 'if', 'while', []);
      Ctx.JSProc.BrContex.BrClose;
      JS.AddE('}');
    end else
    // если переход идет вперед, то это else секция
    if Ctx.JSProc.BranchIndex > -1 then
    begin
      JS.AddEB('} else {');
      Br := Ctx.JSProc.BrContex.Last;
      Br.EndIdx := Args.D.I32;
    end;
  end;
end;

procedure TIL2JSTranslator.Translate_RET(const Ctx: TILTContext);
var
  Br: PILBranch;
  JS: TJSSource;
  JSProc: TJSProc;
begin
  JS := Ctx.Proc.JS;
  JSProc := Ctx.JSProc;
  if Ctx.Cond <> cNone then
  begin
    Br := JSProc.BrContex.Last;
    Br.CmpCond := Ctx.Cond;
    Br.ElseIdx := Ctx.ILCount - 1;
    JS.Add('if (' + ArgToStr(Br.CmpArgs.L) + ' ' + GetJSCond(InverseCondition(Ctx.Cond)) + ' ' + ArgToStr(Br.CmpArgs.R) +  ')');
    JS.AddB('{');
  end else
  begin
    // если мы в контексте ветвления
    if JSProc.BrContex.Count > 0 then
    begin
      // если это не последние RET в коде, значит это else оператор
      if Ctx.ILIndex < (Ctx.ILCount - 1) then
        JS.AddEB('} else {');

      Br := JSProc.BrContex.Last;
      Br.EndIdx := Ctx.ILCount - 1;

    end else begin
      // если это не последний RET в коде, значит это exit оператор
      if Ctx.ILIndex < (Ctx.ILCount - 1) then
        JS.Add('return ' + ProcResultVar(Ctx) + ';')
      else
      // если это последний RET в коде и это функция, делаем явый возврат результата
      if Ctx.Proc.ProcType = ptFunction then
        JS.Add('return Result;');
    end;
  end;
end;

procedure TIL2JSTranslator.Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_D_Args);
var
  JSProc: TJSProc;
  Br: PILBranch;
begin
  JSProc := Ctx.JSProc;
  Br := JSProc.BrContex.Last;
  JSProc.JS.Add(ArgToStr(Args.D) + ' = (' +  ArgToStr(Br.CmpArgs.L) + GetJSCond(Ctx.Cond) + ')? true : false;');
  JSProc.BrContex.BrClose;
end;

procedure TIL2JSTranslator.BeforeInstruction(const Ctx: TILTContext);
var
  BrContex: PILBranchContext;
begin
  BrContex := Ctx.JSProc.BrContex;
  while BrContex.Count > 0 do
  begin
    // если мы были в блоке ветвления, и подошли к концу, то закрываем этот блок
    // также несколько блоков могут иметь один конец
    if BrContex.Last.EndIdx = Ctx.ILIndex then
    begin
      Ctx.JSProc.JS.AddE('}');
      BrContex.BrClose;
      continue;
    end;
    break;
  end;
end;

procedure TIL2JSTranslator.AfterInstruction(const Ctx: TILTContext);
var
  JSProc: TJSProc;
  BRIndex: Integer;
  BrContex: PILBranchContext;
begin
  JSProc := Ctx.JSProc;
  BrContex := JSProc.BrContex;

  // закрытие скобок по окончанию ветвления
  while BrContex.Count > 0 do
  begin
    // если мы были в блоке ветвления, и подошли к концу, то закрываем этот блок
    // также несколько блоков могут иметь один конец
    if BrContex.Last.EndIdx = Ctx.ILIndex then
    begin
      JSProc.JS.AddE('}');
      BrContex.BrClose;
      continue;
    end;
    break;
  end;

  while BrContex.Count > 0 do
  begin
    BRIndex := JSProc.BranchIndex;
    // если мы были в блоке ветвления, и подошли к концу, то закрываем этот блок
    // также несколько блоков могут иметь один конец
    if (BRIndex > -1) and (BRIndex < Ctx.ILIndex) then
    begin
      JSProc.JS.AddE('}');
      BrContex.BrClose;
      continue;
    end;
    break;
  end;
end;

procedure TIL2JSTranslator.Translate_MOVE(const Ctx: TILTContext; var Args: TIL_DS_Args);
begin
  Ctx.Proc.JS.Add(ArgToStr(Args.D) + ' = ' + ArgToStr(Args.S) + ';');
end;

procedure TIL2JSTranslator.Translate_MOVEZERO(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  Ctx.Proc.JS.Add(ArgToStr(Args.D) + ' = 0;');
end;

procedure TIL2JSTranslator.Translate_NOPE(const Ctx: TILTContext);
begin
  Ctx.Proc.JS.Add('// nope');
end;

procedure TIL2JSTranslator.Translate_NOT(const Ctx: TILTContext; var Args: TIL_DS_Args);
begin
  Ctx.Proc.JS.Add(ArgToStr(Args.D) + ' = ~' + ArgToStr(Args.S));
end;

function ProcArgHasResult(const ProcArg: TILArgument): Boolean;
begin
  case ProcArg.ArgClass of
    ARG_PROC: Result := ProcArg.AsProcedure.ProcType = ptFunction;
    ARG_METHOD: Result := ProcArg.AsMethod.Proc.ProcType = ptFunction;
  else
    Result := False;
  end;
end;

procedure TIL2JSTranslator.Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS);
var
  JS: TJSSource;
  IsFunction: Boolean;
  ResultAssignment, ProcArgs: string;
begin
  JS := Ctx.Proc.JS;

  IsFunction := ProcArgHasResult(Args.PArg);
  if IsFunction then
    ResultAssignment := ArgToStr(Args.CallArgs[0]) + ' = '
  else
    ResultAssignment := '';

  ProcArgs := ILArgsToStr(ord(IsFunction), Args.CallArgs);

  JS.Add(ResultAssignment + GetArgProcName(Args.PArg) + '(' + ProcArgs +');');
end;


procedure TIL2JSTranslator.WriteTypesRTTIArray(ILUnit: TILUnit);
begin
  inherited;

end;

{ TILProcHelper }

function TILProcHelper.JS: TJSSource;
begin
  Result := TJSProc(Self).FJS;
end;

{ TJSProc }

constructor TJSProc.Create;
begin
  FJS := TJSSource.Create;
  FBrContext.Init;
end;

destructor TJSProc.Destroy;
begin
  FJS.Free;
  inherited;
end;

function TJSProc.GetBrContext: PILBranchContext;
begin
  Result := addr(FBrContext);
end;

function TJSProc.BranchIndex: Integer;
begin
  if FBrContext.Count > 0 then
    Result := FBrContext.Last.ElseIdx
  else
    Result := -1;
end;

{ TILCtxHelper }

function TILCtxHelper.JSProc: TJSProc;
begin
  Result := TJSProc(Self.Proc);
end;

{ TJSSource }

procedure TJSSource.Add(const Str: string);
begin
  FStrings.Add(DupeString(' ', FSpacing*CSpace) + Str);
end;

procedure TJSSource.AddB(const Str: string);
begin
  FStrings.Add(DupeString(' ', FSpacing*CSpace) + Str);
  Inc(FSpacing);
end;

procedure TJSSource.AddE(const Str: string);
begin
  if FSpacing > 0 then
    Dec(FSpacing);
  FStrings.Add(DupeString(' ', FSpacing*CSpace) + Str);
end;

procedure TJSSource.AddEB(const Str: string);
begin
  if FSpacing > 0 then
    Dec(FSpacing);
  FStrings.Add(DupeString(' ', FSpacing*CSpace) + Str);
  Inc(FSpacing);
end;

procedure TJSSource.AddStrings(const Strings: TStrings);
begin
  FStrings.AddStrings(Strings);
end;

procedure TJSSource.ClearSpacing;
begin
  FSpacing := 0;
end;

constructor TJSSource.Create;
begin
  FStrings := TStringList.Create;
end;

destructor TJSSource.Destroy;
begin
  FStrings.Free;
  inherited;
end;

procedure TJSSource.InsertB(Index: Integer; const Str: string);
begin
  FStrings.Insert(Index, DupeString(' ', FSpacing*CSpace) + Str);
  Inc(FSpacing);
end;

end.
