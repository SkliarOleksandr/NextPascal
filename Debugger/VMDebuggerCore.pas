unit VMDebuggerCore;

interface

uses SysUtils, Classes, System.SyncObjs, Generics.Collections, NPCompiler, NPCompiler.Utils, NPCompiler.Classes,
     IL2VMTranslator, VM.Core, VM.Types, IL.TypeInfo, NPCompiler.DataTypes, Forms, Winapi.Windows, Messages;

const WM_VMCHANGE = WM_USER + 1;

type

{  TVMDebugger = class;

  TVMDebuggerTread = class(TThread)
  private
    FDebugger: TVMDebugger;
    procedure OnBreak(var Context: TVMDebugContext);
  protected
    procedure Execute; override;
  end;}


  TVMDebugVar = record
    Name: string;
    TypeInfo: PRTTIType;
    Reference: Boolean;
    IsParam: Boolean;
    Addr: Pointer;
  end;
  PVMDebugVar = ^TVMDebugVar;

  TVMDebugVars = array of TVMDebugVar;


  TVMDebugProc = record
    RTTI: PRTTIProcedure;
    Name: string;
    Vars: TVMDebugVars;
    Stack: PByte;
  end;

  TVMDebugProcs = TDictionary<NativeUInt, TVMDebugProc>;

  TVMCallStack = TList<TVMDebugProc>;

  TVMDBreakPoint = record
    VMSrcLine: Integer;
    IMGOffset: TOffset;
  end;

  TVMDBreakPoints = TList<TVMDBreakPoint>;

  TStepType = (StepASM, StepSRC);

  TVMDebugger = class(TThread)
  type
    TDebugState = (dsStopped, dsWaiting, dsRunning, dsPaused, dsBreakPoint);
    TTraceMode = (tmStepOne, tmStepOver, tmRunUntilRet, tmRunToBreakPoint);
  private
    FPackage: INPPackage;
    FVMTranslator: TVMTranslator;
    FVM: TILMachine;
    FASMTextLine: Integer;
    FSRCTextLine: Integer;
    //FVMTextLine: Integer;
    [volatile]FStepMode: TStepType;
    [volatile]FState: TDebugState;
    [volatile]FTraceMode: TTraceMode;
    FRegisters: PVMRegisters;
    FIMGPosition: Integer;
    FVMText: string;
    FGlobalVars: TVMDebugVars;
    FProcedures: TVMDebugProcs;
    FCurrentStack: PByte;
    FStopProcIndex: Integer;
    FCallStack: TVMCallStack;
    //FCallCount: Int64;
    FBreakPoints: TVMDBreakPoints;
    FDebugViewer: TForm;
    FCurUnitID: Integer;
    //FInstructions: array of TOffset;
    procedure GetASMCodeLine(IMGOffset: Integer);
    function GetCurrentProc: TVMDebugProc;
  protected
    procedure Execute; override;
    procedure PrepareDebug;
    procedure MapGlobalVars;
    procedure MapProcedures; // получет список процедур из VM и информацию о локальных переменных/параметрах
    procedure MapBreakPoints;
    procedure MapProcLocalVars(var Proc: TVMDebugProc);
    procedure MapProcParams(var Proc: TVMDebugProc);
    procedure OnVMStep(var Context: TVMDebugContext);
    procedure OnVMCall(Stack: PByte; ProcOffset: TOffset);
    procedure OnVMRet(Stack: PByte);
    function FindBP(IMGOffset: TOffset): Boolean;
  public
    constructor Create(const Package: INPPackage; DebugViewer: TForm);
    destructor Destroy; override;
    ////////////////////////////////////////////
    procedure Run;
    procedure Stop;
    procedure StepInto(StepType: TStepType);
    procedure StepOver(StepType: TStepType);
    procedure RunUntilRet;
    procedure RunToBreakPoint;
    procedure AddBreakPoint(IMGOffset: TOffset; VMSrcLine: Integer); overload;
    function AddBreakPoint(VMSrcLine: Integer): Boolean; overload;
    procedure _DeleteBreakPoint(IMGOffset: TOffset);
    procedure DeleteBreakPoint(VMSrcLine: Integer);
    property ASMText: string read FVMText;
    property ASMTextLine: Integer read FASMTextLine;
    property SRCTextLine: Integer read FSRCTextLine;
    property CurrUnitID: Integer read FCurUnitID;
    property Registers: PVMRegisters read FRegisters;
    property State: TDebugState read FState;
    property IMGPosition: Integer read FIMGPosition;
    property GlobalVars: TVMDebugVars read FGlobalVars;
    property CurrentProc: TVMDebugProc read GetCurrentProc;
    property VM: TILMachine read FVM;
    property BreakPoints: TVMDBreakPoints read FBreakPoints;
    property CallStack: TVMCallStack read FCallStack;
    function GetGlobalVarValue(V: PVMDebugVar): string;
    function GetLocalVarValue(V: PVMDebugVar): string;
    function BreakPointExist(VMSrcLine: Integer): Boolean;
  end;


implementation

{ TVMDebugger }

uses ILTranslator;

procedure TVMDebugger.AddBreakPoint(IMGOffset: TOffset; VMSrcLine: Integer);
var
  BP: TVMDBreakPoint;
begin
  BP.IMGOffset := IMGOffset;
  BP.VMSrcLine := VMSrcLine;
  FBreakPoints.Add(BP);
end;

procedure TVMDebugger._DeleteBreakPoint(IMGOffset: TOffset);
var
  i: Integer;
begin
  for i := FBreakPoints.Count - 1 downto 0 do
    if FBreakPoints[i].IMGOffset = IMGOffset then
      FBreakPoints.Delete(i);
end;

procedure TVMDebugger.DeleteBreakPoint(VMSrcLine: Integer);
var
  i: Integer;
begin
  for i := FBreakPoints.Count - 1 downto 0 do
    if FBreakPoints[i].VMSrcLine = VMSrcLine then
      FBreakPoints.Delete(i);
end;

function TVMDebugger.AddBreakPoint(VMSrcLine: Integer): Boolean;
var
  i: Integer;
  Lines: TVMCodeLines;
begin
  Lines := FVMTranslator.VMCodeLines;
  for i := 0 to Lines.Count - 1 do
  begin
    if Lines[i].AsmTextLine = VMSrcLine then
    begin
      _DeleteBreakPoint(Lines[i].Offset);
      AddBreakPoint(Lines[i].Offset, VMSrcLine);
    end;
  end;
  Result := True;
end;

function TVMDebugger.BreakPointExist(VMSrcLine: Integer): Boolean;
var
  i: Integer;
begin
  for i := FBreakPoints.Count - 1 downto 0 do
    if FBreakPoints[i].VMSrcLine = VMSrcLine then
      Exit(True);
  Result := False;
end;

constructor TVMDebugger.Create(const Package: INPPackage; DebugViewer: TForm);
begin
  inherited Create(True);
  FDebugViewer := DebugViewer;
  FPackage := Package;
  FVM := TILMachine.Create();
  FVM.OnBreakEvent := OnVMStep;
  FVM.OnVMCallEvent := OnVMCall;
  FVM.OnVMRetEvent := OnVMRet;
  FState := dsStopped;
  FreeOnTerminate := False;
  FVMTranslator := TVMTranslator.Create;
  FVMTranslator.IncludeRTTI := True;
  FVMTranslator.RTTICharset := Package.RTTICharset;
  FProcedures := TVMDebugProcs.Create(10);
  FCallStack := TVMCallStack.Create;
  FCallStack.Capacity := 10;
  FStopProcIndex := -1;
  FBreakPoints := TVMDBreakPoints.Create;
end;

destructor TVMDebugger.Destroy;
begin
  FState := dsStopped;
  sleep(10);
  FVM.Free;
  FVMTranslator.Free;
  FProcedures.Free;
  FCallStack.Free;
  FBreakPoints.Free;
  FPackage.Clear;
  inherited;
end;

procedure TVMDebugger.Execute;
begin
  while not Terminated do
  begin
    while (FState <> dsRunning) and not Terminated do
      sleep(10);

    if not Terminated then
      FVM.DebugRun;

    FState := dsStopped;
    while (FState = dsStopped) and not Terminated do
      sleep(10);
  end;
end;

function TVMDebugger.FindBP(IMGOffset: TOffset): Boolean;
var
  i: Integer;
begin
  for i := 0 to FBreakPoints.Count - 1 do
    if FBreakPoints[i].IMGOffset = IMGOffset then
      Exit(True);
  Result := False;
end;

function TVMDebugger.GetCurrentProc: TVMDebugProc;
begin
  if FCallStack.Count > 0 then
    Result := FCallStack[FCallStack.Count - 1]
  else begin
    Result.Name := '';
    Result.Vars := [];
    Result.Stack := nil;
  end;
end;

function TVMDebugger.GetGlobalVarValue(V: PVMDebugVar): string;
var
  Ptr: Pointer;
begin
  Ptr := V.Addr;
  if V.Reference then
    Ptr := PPointer(Ptr)^;
  Result := FVM.ReadValue(Ptr, V.TypeInfo);
end;

function TVMDebugger.GetLocalVarValue(V: PVMDebugVar): string;
var
  Ptr: Pointer;
begin
  try
    Ptr := FCurrentStack + NativeUInt(V.Addr);
    if V.Reference and not V.IsParam then
      Ptr := PPointer(Ptr)^;
    if (Pos('$', V.Name) >= Low(string)) and
       (V.TypeInfo.DataTypeID in [dtString, dtAnsiString]) then
      Result := '<unknown>'
    else
      Result := FVM.ReadValue(Ptr, V.TypeInfo);
  except
    Result := '<unknown>';
  end;
end;

procedure TVMDebugger.GetASMCodeLine(IMGOffset: Integer);
var
  i: Integer;
  VMCL: TVMCodeLine;
begin
  for i := 0 to FVMTranslator.VMCodeLines.Count - 1 do
  begin
    VMCL := FVMTranslator.VMCodeLines[i];
    if VMCL.Offset = IMGOffset then
    begin
      FASMTextLine := VMCL.AsmTextLine;
      FSRCTextLine := VMCL.SrcTextLine;
      FCurUnitID := VMCL.UnitID;
      Exit;
    end;
  end;
end;

procedure TVMDebugger.MapBreakPoints;
var
  ui, bpi: Integer;
  U: TILUnit;
begin
  for ui := 0 to FVMTranslator.UnitsCount - 1 do
  begin
    U := FVMTranslator.Units[ui];
    for bpi := 0 to Length(U.BreakPoints) - 1 do
    begin
      AddBreakPoint(U.BreakPoints[bpi].Offset, U.BreakPoints[bpi].VMTextLine);
    end;
  end;
end;

procedure TVMDebugger.MapGlobalVars;
var
  ui, vi, c: Integer;
  UN: PVMUnit;
  GVar: PVMVariable;
  DVar: PVMDebugVar;
begin
  for ui := 0 to  FVM.UnitsCount - 1 do
  begin
    UN := @(FVM.Units[ui]);
    c := Length(FGlobalVars);
    SetLength(FGlobalVars, c + UN.VarsCount);
    for vi := 0 to UN.VarsCount - 1 do
    begin
      GVar := @(UN.Variables[vi]);
      DVar := @(FGlobalVars[c + vi]);
      DVar.Name := FVM.GetVarName(GVar);
      DVar.TypeInfo := GVar.DataType;
      DVar.Addr := GVar.Addr;
    end;
  end;
end;

procedure TVMDebugger.MapProcLocalVars(var Proc: TVMDebugProc);
var
  i, cnt: Integer;
  LVar: PRTTILocalVar;
  LVars: TRTTILocalVars;
  DebugVar: PVMDebugVar;
begin
  if Proc.RTTI.LocalVars = 0 then
  begin
    Proc.Vars := nil;
    Exit;
  end;

  LVars := TRTTILocalVars(Proc.RTTI.LocalVars);
  cnt := Length(LVars);
  SetLength(Proc.Vars, cnt);
  for i := 0 to cnt - 1 do
  begin
    LVar := @(LVars[i]);
    DebugVar := @(Proc.Vars[i]);
    DebugVar.Name := FVM.GetLocalVarName(LVar);
    DebugVar.TypeInfo := PRTTIType(LVar.DataType);
    DebugVar.Reference := LVar.IsReference;
    DebugVar.IsParam := LVar.IsParam;
    DebugVar.Addr := Pointer(LVar.Offset);
  end;
end;

procedure TVMDebugger.MapProcParams(var Proc: TVMDebugProc);
var
  i: Integer;
  Param: PVMParam;
  Params: TVMParamsArray;
  ParamsCnt: Integer;
  DebugParam: PVMDebugVar;
begin
  if Proc.RTTI.Params = 0 then
  begin
    Proc.Vars := nil; // params
    Exit;
  end;

  Params := TVMParamsArray(Proc.RTTI.Params);
  ParamsCnt := Length(Params);
  SetLength(Proc.Vars, ParamsCnt);
  for i := 0 to ParamsCnt - 1 do
  begin
    Param := @(Params[i]);
    DebugParam := @(Proc.Vars[i]);
    DebugParam.Name := FVM.GetParamName(Param);
    if DebugParam.Name = '' then
      DebugParam.Name := 'Self';
    DebugParam.TypeInfo := PRTTIType(Param.DataType);
    DebugParam.Reference := Param.Reference;
    DebugParam.Addr := Pointer(Param.Offset);
  end;
end;

function GetString(Name: TOffset): string;
begin
  Result := string(Name);
end;

procedure TVMDebugger.MapProcedures;
type
    TPtrSArray = array [0..65535] of Pointer;
    PPtrSArray = ^TPtrSArray;
var
  ui, pi: Integer;
  UN: PVMUnit;
  P: PRTTIProcedure;
  DProc: TVMDebugProc;
begin
  for ui := 0 to FVM.UnitsCount - 1 do
  begin
    UN := @(FVM.Units[ui]);
    for pi := 0 to UN.ProcsCount - 1 do
    begin
      P := PRTTIProcedure(PByte(PPtrSArray(UN.Procs)[pi]) - SizeOf(TVMObjHeader));
      DProc.RTTI := P;
      DProc.Name := GetString(P.Name);
      MapProcParams(DProc);
      MapProcLocalVars(DProc);
      FProcedures.Add(FVM.GetProcOffset(P), DProc);
    end;
  end;
end;

procedure TVMDebugger.OnVMStep(var Context: TVMDebugContext);
var
  PrevSrcLine: Integer;
begin
  if FState = dsRunning then
    FState := dsWaiting;

  if FTraceMode = tmRunToBreakPoint then
  begin
    if FindBP(Context.IMGOffset) then
      FTraceMode := tmStepOne;
  end;

  if (FTraceMode = tmStepOne) or
     ((FTraceMode = tmStepOver) and (CurrentProc.Stack = Context.Stack)) then
  begin
    FIMGPosition := Context.IMGOffset;
    PrevSrcLine := FSRCTextLine;
    GetASMCodeLine(Context.IMGOffset);
    FRegisters := Context.Registers;
    FCurrentStack := Context.Stack;

    if (FStepMode = StepASM) or
       ((FStepMode = StepSRC) and (PrevSrcLine <> FSRCTextLine)) then
    begin
      PostMessage(FDebugViewer.Handle, WM_VMCHANGE, 0, 0);
      while (FState <> dsRunning) and (FState <> dsStopped) do
        Sleep(10);
    end;
  end;
  Context.Terminate := (FState = dsStopped) or Terminated;
end;

procedure TVMDebugger.OnVMCall(Stack: PByte; ProcOffset: TOffset);
var
  Proc: TVMDebugProc;
begin
  if FState = dsStopped then
    Exit;

  if (FTraceMode = tmStepOver) and (FProcedures.Count > 1) then
    Exit;

  if not FProcedures.TryGetValue(ProcOffset, Proc) then
    Exit;

  Proc.Stack := Stack;
  FCallStack.Add(Proc);
  PostMessage(FDebugViewer.Handle, WM_VMCHANGE, 0, 0);
end;

procedure TVMDebugger.OnVMRet(Stack: PByte);
var
  i: Integer;
  P: TVMDebugProc;
begin
  i := FCallStack.Count - 1;
  if i >= 0 then begin
    P := FCallStack[i];
    if i = FStopProcIndex then
    begin
      FTraceMode := tmStepOne;
      PostMessage(FDebugViewer.Handle, WM_VMCHANGE, 0, 0);
    end;

    if P.Stack = Stack then
      FCallStack.Delete(i);
  end;
end;

procedure TVMDebugger.PrepareDebug;
var
  ILStream, VMStream: TMemoryStream;
  //CResult: TCompilerResult;
begin
  ILStream := TILMemoryStream.Create;
  try
    FPackage.IncludeDebugInfo := True;
    FPackage.SaveToStream(ILStream);

    ILStream.Position := 0;
    FVMTranslator.LoadILCode(ILStream);
    FVMText := FVMTranslator.VMCodeAsString;

    VMStream := TILMemoryStream.Create;
    try
      FVMTranslator.SaveTargetCode(VMStream);
      VMStream.Position := 0;
      FVM.LoadVMImage(VMStream);
    finally
      VMStream.Free;
    end;

    MapGlobalVars;
    MapProcedures;
    MapBreakPoints;
  finally
    ILStream.Free;
  end;
end;

procedure TVMDebugger.Run;
begin
  if not Started then
  begin
    PrepareDebug;
    Start;
  end;
  FState := dsRunning;
  FTraceMode := tmStepOne;
end;

procedure TVMDebugger.RunToBreakPoint;
begin
  FTraceMode := tmRunToBreakPoint;
  FState := dsRunning;
  FStopProcIndex := FCallStack.Count - 1;
end;

procedure TVMDebugger.RunUntilRet;
begin
  FTraceMode := tmRunUntilRet;
  FState := dsRunning;
  FStopProcIndex := FCallStack.Count - 1;
end;

procedure TVMDebugger.StepInto(StepType: TStepType);
begin
  FTraceMode := tmStepOne;
  FState := dsRunning;
  FStepMode := StepType;
  FStopProcIndex := -1;
end;

procedure TVMDebugger.StepOver(StepType: TStepType);
begin
  FTraceMode := tmStepOver;
  FState := dsRunning;
  FStepMode := StepType;
  FStopProcIndex := -1;
end;

procedure TVMDebugger.Stop;
begin
  FState := dsStopped;
  FCallStack.Clear;
end;

end.

