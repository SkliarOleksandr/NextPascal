unit IL2WASMTranslator;

{$I compilers.inc}

interface

uses
  System.SysUtils, System.Classes, Generics.Defaults, Generics.Collections, System.Math, NPCompiler.Utils,
  NPCompiler.DataTypes, IL.Types, IL.TypeInfo, VM.Core, VM.Types, VM.Invoke, ILTranslator, WASM.Writer; // system

type
  TWASMTranslator = class(TILTranslator)
  private
    FWASM: IWASMWriter;
    FJSImportTable: string;
  protected
    procedure WriteTypesRTTIArray(ILUnit: TILUnit); override;
    procedure LoadILConsts(ILUnit: TILUnit; Stream: TStream); override;
    procedure LoadILGlobalVars(ILUnit: TILUnit; Stream: TStream); override;
    procedure LoadILProcBody(Stream: TStream; Proc: TILProc); override;
    procedure WriteVMExportProcs(ILUnit: TILUnit); override;
    procedure WriteProcsRTTI(ILUnit: TILUnit); override;
    procedure WriteProcProlog(Proc: TILProc); override;
    procedure WriteProcEpilog(Proc: TILProc; LastILCode: TILCode); override;
    procedure WriteLDInstruction(const Ctx: TILTContext; Arg: TILArgument);
    procedure WriteSTInstruction(Arg: TILArgument);

    procedure Translate_NOPE(const Ctx: TILTContext); override;
    procedure Translate_2OperandOpCode(const Ctx: TILTContext; var Args: TIL_DS_Args); override;
    procedure Translate_3OperandOpCode(const Ctx: TILTContext; var Args: TIL_DSS_Args); override;
    procedure Translate_LDPTR_Instruction(const Ctx: TILTContext; var Dst: TILArgument; const Src: TILArgument); override;
    procedure Translate_LEA(const Ctx: TILTContext; var Args: TIL_LEA_Args); override;
    procedure Translate_ARRDALLOC(const Ctx: TILTContext; var Args: TIL_ARRDALLOC_ARGS); override;
    procedure Translate_ARRAYLENGTH(const Ctx: TILTContext; var Args: TIL_ARRAYLENGTH_ARGS); override;
    procedure Translate_ARRAYCOPY(const Ctx: TILTContext; var Args: TIL_ARRAYCOPY_ARGS); override;
    procedure Translate_ARRAYMOVE(const Ctx: TILTContext; var Args: TIL_ARRAYMOVE_ARGS); override;
    procedure Translate_TYPEINFO(const Ctx: TILTContext; var Args: TIL_TYPEINFO_ARGS); override;
    procedure Translate_QUERYTYPE(const Ctx: TILTContext; var Args: TIL_QUERYTYPE_ARGS); override;
    procedure Translate_UNIQUE(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_CMP(const Ctx: TILTContext; var Args: TIL_CMP_ARGS); override;
    procedure Translate_CMPJ(const Ctx: TILTContext; var Args: TIL_CMPJ_ARGS); override;
    procedure Translate_TEST(const Ctx: TILTContext; var Args: TIL_TEST_ARGS); override;
    procedure Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_CONVERT_ARGS); override;
    procedure Translate_MOVE(const Ctx: TILTContext; var Args: TIL_MOVE_ARGS); override;
    procedure Translate_MOVEZERO(const Ctx: TILTContext; var Args: TIL_MOVEZERO_ARGS); override;
    procedure Translate_NOT(const Ctx: TILTContext; var Args: TIL_NOT_ARGS); override;
    procedure Translate_INC(const Ctx: TILTContext; var Args: TIL_INC_ARGS); override;
    procedure Translate_GETBIT(const Ctx: TILTContext; var Args: TIL_GETBIT_ARGS); override;
    procedure Translate_SETBIT(const Ctx: TILTContext; var Args: TIL_SETBIT_ARGS); override;
    procedure Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_SETBOOL_ARGS); override;
    procedure Translate_TRYBEGIN(const Ctx: TILTContext); override;
    procedure Translate_TRYEND(const Ctx: TILTContext); override;
    procedure Translate_NEARCALL(const Ctx: TILTContext; var Args: TIL_NEARCALL_ARGS); override;
    procedure Translate_ETHROW(const Ctx: TILTContext; var Args: TIL_ETHROW_ARGS); override;
    procedure Translate_INCREF(const Ctx: TILTContext; var Args: TIL_INCREF_ARGS); override;
    procedure Translate_DECREF(const Ctx: TILTContext; var Args: TIL_DECREF_ARGS); override;
    procedure Translate_DECREFFINAL(const Ctx: TILTContext; var Args: TIL_DECREFFINAL_ARGS); override;
    procedure Translate_INIT(const Ctx: TILTContext; var Args: TIL_INIT_ARGS); override;
    procedure Translate_WEAKREF(const Ctx: TILTContext; var Args: TIL_WEAKREF_ARGS); override;
    procedure Translate_STRONGREF(const Ctx: TILTContext; var Args: TIL_STRONGREF_ARGS); override;
    procedure Translate_MEMSET(const Ctx: TILTContext; var Args: TIL_MEMSET_ARGS); override;
    procedure Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS); override;
    procedure Translate_CHKBND(const Ctx: TILTContext; var Args: TIL_CHCKBND_ARGS); override;
    procedure Translate_MEMGET(const Ctx: TILTContext; var Args: TIL_DNEW_ARGS); override;
    procedure Translate_MEMFREE(const Ctx: TILTContext; var Args: TIL_DFREE_ARGS); override;
    procedure Translate_DNEWOBJ(const Ctx: TILTContext; var Args: TIL_DNEWOBJ_ARGS); override;
    procedure Translate_JMP(const Ctx: TILTContext; var Args: TIL_JMP_ARGS); override;
    procedure Translate_RET(const Ctx: TILTContext); override;
    procedure Translate_GETPTR(const Ctx: TILTContext; var Args: TIL_GETPTR_ARGS); override;
    procedure Translate_LDMETHOD(const Ctx: TILTContext; var Args: TIL_LDMETHOD_ARGS); override;
    procedure Translate_LDSMETHOD(const Ctx: TILTContext; var Args: TIL_LDSMETHOD_ARGS); override;
    procedure Translate_GETSPTR(const Ctx: TILTContext; var Args: TIL_GETSPTR_ARGS); override;
    procedure Translate_RDREF(const Ctx: TILTContext; var Args: TIL_RDREF_ARGS); override;
    procedure Translate_WDREF(const Ctx: TILTContext; var Args: TIL_WDREF_ARGS); override;
    procedure Translate_REFCNT(const Ctx: TILTContext; var Args: TIL_DS_ARGS); override;
    procedure Translate_NOW(const Ctx: TILTContext; var Args: TIL_D_Args); override;
    procedure Translate_FMACRO(const Ctx: TILTContext; var Args: TIL_FMACRO_ARGS); override;
    procedure Translate_LDMETHODPTR_Instruction(const Ctx: TILTContext; const Dst, Method: TILArgument); override;
    procedure WriteProcsDecls;
    procedure WriteProcDeclToWASM(Proc: TILProc);
    function GetOpCode(ILCode: TILCode; TypeInfo: PRTTIType): TWasmOpCode;
    function ILDataType2WASMDataType(TypeInfo: PRTTIType): TWASMDataType;
    function CreateILUnit(const Name: string): TILUnit; override;
    function CreateILUnitProc(ILUnit: TILUnit): TILProc; override;
    procedure MapImportProc(Proc: TILProc); override;
    procedure MakeImportTable; override;
  public
    procedure SaveTargetCode(Stream: TStream); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  {TWASMILUnit = class(TILUnit)
  private
    FWASM: IWASMWriter;
  public
    property WASM: IWASMWriter read FWASM;
  end;}

  TWASMILProc = class(TILProc)
  private
    FWASMProc: IWASMCodeWriter;
  public
    property WASMProc: IWASMCodeWriter read FWASMProc;
  end;

implementation

type
  TILProcHelper = class helper for TILProc
    function WASMProc: IWASMCodeWriter; inline;
  end;


var
  _WASMInstructions: array [TILCode] of array [TDataTypeID] of TWasmOpCode;

procedure RegInstruction(ILCode: TILCode; const DataTypes: array of TDataTypeID; OpCode: TWasmOpCode);
var
  i: Integer;
begin
  for i := 0 to Length(DataTypes) - 1 do
    if _WASMInstructions[ILCode, DataTypes[i]] <> op_unreachable then
      AbortWork('Register instruction already exist');
  for i := 0 to Length(DataTypes) - 1 do
    _WASMInstructions[ILCode, DataTypes[i]] := OpCode;
end;

{ TWASMTranslator }

constructor TWASMTranslator.Create;
begin
  inherited;
  FWASM := TWASMWriter.Create;
end;

function TWASMTranslator.CreateILUnit(const Name: string): TILUnit;
begin
  Result := TILUnit.Create;
end;

function TWASMTranslator.CreateILUnitProc(ILUnit: TILUnit): TILProc;
begin
  Result := TWASMILProc.Create;
end;

destructor TWASMTranslator.Destroy;
begin
  inherited;
end;

function TWASMTranslator.GetOpCode(ILCode: TILCode; TypeInfo: PRTTIType): TWasmOpCode;
begin
  Result := _WASMInstructions[ILCode, TypeInfo.DataTypeID];
  Assert(Result <> op_unreachable);
end;

function TWASMTranslator.ILDataType2WASMDataType(TypeInfo: PRTTIType): TWASMDataType;
begin
  case TypeInfo.DataTypeID of
    dtInt8, dtInt16, dtInt32: Result := dt_i32;
    dtUInt8, dtUInt16, dtUInt32: Result := dt_i32;
    dtInt64, dtUInt64: Result := dt_i64;
    dtFloat32: Result := dt_f32;
    dtFloat64: Result := dt_f64;
  else
    AbortWork('type is not supported');
    Result := dt_i32;
  end;
end;

procedure TWASMTranslator.LoadILConsts(ILUnit: TILUnit; Stream: TStream);
var
  c: Integer;
begin
  c := Stream.ReadStretchUInt;
  Assert(c = 0);
end;

procedure TWASMTranslator.LoadILGlobalVars(ILUnit: TILUnit; Stream: TStream);
var
  c: Integer;
begin
  c := Stream.ReadStretchUInt;
  Assert(c = 0);
end;

procedure TWASMTranslator.LoadILProcBody(Stream: TStream; Proc: TILProc);
begin
  inherited;

end;

procedure TWASMTranslator.MakeImportTable;
begin
  inherited;

end;

procedure TWASMTranslator.MapImportProc(Proc: TILProc);
var
  ImportLib, ImportName: string;
begin
  ImportLib := GetString(Proc.ImportLib);
  ImportName := GetString(Proc.ImportName);

  FWASM.ImportSection.AddImportFunction(ImportLib, ImportName, 1{Proc.ImportIndex});
end;

procedure TWASMTranslator.SaveTargetCode(Stream: TStream);
begin
  FWASM.TableSection.AddTable;
  FWASM.MemorySection.AddMemory;
//  FWASM.TypesSection.AddSignature([], dt_i32);
//  FWASM.ExportSection.AddExportFunction('memory', 0, 2);

  WriteProcsDecls;
  FWASM.SaveToStream(Stream);
end;

procedure TWASMTranslator.Translate_2OperandOpCode(const Ctx: TILTContext; var Args: TIL_DS_Args);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_3OperandOpCode(const Ctx: TILTContext; var Args: TIL_DSS_Args);
var
  OpCode: TWasmOpCode;
  ComDataType: PRTTIType;
begin
  ComDataType := GetSysTypeInfo(ord(dtInt32));
  OpCode := GetOpCode(Ctx.ILCode, ComDataType);

  WriteLDInstruction(Ctx, Args.L);
  WriteLDInstruction(Ctx, Args.R);

  Ctx.Proc.WASMProc.WriteOpCode(OpCode);
end;

procedure TWASMTranslator.Translate_ARRAYCOPY(const Ctx: TILTContext; var Args: TIL_ARRAYCOPY_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_ARRAYLENGTH(const Ctx: TILTContext; var Args: TIL_ARRAYLENGTH_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_ARRAYMOVE(const Ctx: TILTContext; var Args: TIL_ARRAYMOVE_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_ARRDALLOC(const Ctx: TILTContext; var Args: TIL_ARRDALLOC_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_CHKBND(const Ctx: TILTContext; var Args: TIL_CHCKBND_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_CMP(const Ctx: TILTContext; var Args: TIL_CMP_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_CMPJ(const Ctx: TILTContext; var Args: TIL_CMPJ_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_CONVERT_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_DECREF(const Ctx: TILTContext; var Args: TIL_DECREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_DECREFFINAL(const Ctx: TILTContext; var Args: TIL_DECREFFINAL_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_MEMFREE(const Ctx: TILTContext; var Args: TIL_DFREE_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_MEMGET(const Ctx: TILTContext; var Args: TIL_DNEW_ARGS);
begin
  inherited;
  {Ctx.Proc.WASMProc.WriteOpCode(op_grow_memory);
  Ctx.Proc.WASMProc.Write_VarUInt32(0);}
  Ctx.Proc.WASMProc.WriteOpCode(op_current_memory);
  Ctx.Proc.WASMProc.Write_VarUInt32(0);
end;

procedure TWASMTranslator.Translate_DNEWOBJ(const Ctx: TILTContext; var Args: TIL_DNEWOBJ_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_ETHROW(const Ctx: TILTContext; var Args: TIL_ETHROW_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_FMACRO(const Ctx: TILTContext; var Args: TIL_FMACRO_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_GETBIT(const Ctx: TILTContext; var Args: TIL_GETBIT_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_GETPTR(const Ctx: TILTContext; var Args: TIL_GETPTR_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_GETSPTR(const Ctx: TILTContext; var Args: TIL_GETSPTR_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_INC(const Ctx: TILTContext; var Args: TIL_INC_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_INCREF(const Ctx: TILTContext; var Args: TIL_INCREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_INIT(const Ctx: TILTContext; var Args: TIL_INIT_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_JMP(const Ctx: TILTContext; var Args: TIL_JMP_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_LDMETHOD(const Ctx: TILTContext; var Args: TIL_LDMETHOD_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_LDMETHODPTR_Instruction(const Ctx: TILTContext; const Dst, Method: TILArgument);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_LDPTR_Instruction(const Ctx: TILTContext; var Dst: TILArgument; const Src: TILArgument);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_LDSMETHOD(const Ctx: TILTContext; var Args: TIL_LDSMETHOD_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_LEA(const Ctx: TILTContext; var Args: TIL_LEA_Args);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_MEMSET(const Ctx: TILTContext; var Args: TIL_MEMSET_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_MOVE(const Ctx: TILTContext; var Args: TIL_MOVE_ARGS);
begin
  Assert(not Check(Args.D.AsVariable.Flags, ILVAR_TEMP));

  if (Args.D.AsVariable.IsResult) and
     (Check(Args.D.AsVariable.Flags, ILVAR_TEMP)) then
    Exit;

  WriteLDInstruction(Ctx, Args.S);
end;

procedure TWASMTranslator.Translate_MOVEZERO(const Ctx: TILTContext; var Args: TIL_MOVEZERO_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_NEARCALL(const Ctx: TILTContext; var Args: TIL_NEARCALL_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_NOPE(const Ctx: TILTContext);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_NOT(const Ctx: TILTContext; var Args: TIL_NOT_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_NOW(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS);
begin
  inherited;
  WriteLDInstruction(Ctx, Args.CallArgs[0]);
  Ctx.Proc.WASMProc.Write_CALL(Args.PArg.AsProcedure.ImportIndex);
  //Ctx.Proc.WASMProc.WriteOpCode(op_drop); // skip result
end;

procedure TWASMTranslator.Translate_QUERYTYPE(const Ctx: TILTContext; var Args: TIL_QUERYTYPE_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_RDREF(const Ctx: TILTContext; var Args: TIL_RDREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_REFCNT(const Ctx: TILTContext; var Args: TIL_DS_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_RET(const Ctx: TILTContext);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_SETBIT(const Ctx: TILTContext; var Args: TIL_SETBIT_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_SETBOOL_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_STRONGREF(const Ctx: TILTContext; var Args: TIL_STRONGREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_TEST(const Ctx: TILTContext; var Args: TIL_TEST_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_TRYBEGIN(const Ctx: TILTContext);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_TRYEND(const Ctx: TILTContext);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_TYPEINFO(const Ctx: TILTContext; var Args: TIL_TYPEINFO_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_UNIQUE(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_WDREF(const Ctx: TILTContext; var Args: TIL_WDREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.Translate_WEAKREF(const Ctx: TILTContext; var Args: TIL_WEAKREF_ARGS);
begin
  inherited;

end;

procedure TWASMTranslator.WriteLDInstruction(const Ctx: TILTContext; Arg: TILArgument);
var
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(Arg);
  case Arg.ArgClass of
    ARG_CONST: begin
      case TypeInfo.DataTypeID of
        dtInt8, dtInt16, dtInt32, dtUInt8, dtUInt16, dtUInt32,
        dtChar, dtAnsiChar, dtBoolean: Ctx.Proc.WASMProc.Write_I32_CONST(Arg.I32);
        dtInt64, dtUInt64: Ctx.Proc.WASMProc.Write_I64_CONST(Arg.I64);
        dtFloat32: Ctx.Proc.WASMProc.Write_F32_CONST(Arg.I32);
        dtFloat64: Ctx.Proc.WASMProc.Write_F64_CONST(Arg.I64);
      else
        AbortWorkNotSupported();
      end;
    end;
    ARG_VAR: begin
      {временные переменные находятся на вершине стека, поэтому явно не загружаются}
      if not Check(Arg.AsVariable.Flags, ILVAR_TEMP) then
      case Arg.ArgScope of
        ARG_SCOPE_LOCAL: Ctx.Proc.WASMProc.Write_GET_LOCAL(0);
        ARG_SCOPE_GLOBAL: Ctx.Proc.WASMProc.Write_GET_GLOBAL(0);
      else
        AbortWorkNotSupported();
      end;
    end;
  else
    AbortWorkNotSupported();
  end;
end;

procedure TWASMTranslator.WriteProcDeclToWASM(Proc: TILProc);
var
  WASMParams: TWASMDataTypes;
  Param: PRTTIParameter;
  si, i, c: Integer;
  ParamType: PRTTIType;
  ResultType: TWASMDataType;
  ProcInfo: PRTTIProcedure;
  RetType: PRTTIType;
  WasmProcIdx: TWASMIndex;
begin
  c := Length(Proc.Params);

  if Proc.ProcType = ptFunction then
    si := 1
  else
    si := 0;

  SetLength(WASMParams, c - si);
  for i := si to c - 1 do
  begin
    Param := Addr(Proc.Params[i]);
    ParamType := GetTypeInfo(Param.DataType);
    WASMParams[i - si] := ILDataType2WASMDataType(ParamType);
  end;

  ProcInfo := GetProcInfo(Proc);

  if Proc.ProcType = ptFunction then
  begin
    RetType := GetTypeInfo(ProcInfo.ResultType);
    case RetType.DataTypeID of
      dtInt64, dtUInt64: ResultType := dt_i64;
      dtFloat32: ResultType := dt_f32;
      dtFloat64: ResultType := dt_f64;
    else
      ResultType := dt_i32;
    end;
    FWASM.TypesSection.AddSignature(WASMParams, ResultType);
  end else
    FWASM.TypesSection.AddSignature(WASMParams);

  if not Proc.IsImported then
  begin
    WasmProcIdx := FWASM.ExportSection.AddExportFunction(Proc.Name, FWASM.ExportSection.Count, 0);
    FWASM.FunctionSection.AddFunctionSignatureIndex(WasmProcIdx);
  end;
end;

procedure TWASMTranslator.WriteProcEpilog(Proc: TILProc; LastILCode: TILCode);
begin
  inherited;
  if Proc.WASMProc <> nil then
    Proc.WASMProc.WriteEnd;
end;

procedure TWASMTranslator.WriteProcProlog(Proc: TILProc);
begin
  inherited;
  if (not Proc.IsImported) and not Proc.Name.StartsWith('$') then
    TWASMILProc(Proc).FWASMProc := FWASM.CodeSection.AddFunctionBody;
end;

procedure TWASMTranslator.WriteProcsDecls;
var
  ui, pi: Integer;
  U: TILUnit;
  P: TILProc;
begin
  for ui := 0 to Length(FUnits) - 1 do
  begin
    U := FUnits[ui];
    for pi := 0 to Length(U.Procs) - 1 do
    begin
      P := U.Procs[pi];
      WriteProcDeclToWASM(P);
    end;
  end;
end;

procedure TWASMTranslator.WriteProcsRTTI(ILUnit: TILUnit);
begin
  inherited;

end;

procedure TWASMTranslator.WriteSTInstruction(Arg: TILArgument);
begin

end;

procedure TWASMTranslator.WriteTypesRTTIArray(ILUnit: TILUnit);
begin
  inherited;

end;

procedure TWASMTranslator.WriteVMExportProcs(ILUnit: TILUnit);
begin
  inherited;

end;

{ TILProcHelper }

function TILProcHelper.WASMProc: IWASMCodeWriter;
begin
  Result := TWASMILProc(Self).WASMProc;
end;

initialization
  FillChar(_WASMInstructions, SizeOf(_WASMInstructions), 0);
  RegInstruction(icAdd, [dtInt8, dtInt16, dtInt32], op_i32_add);
  RegInstruction(icSub, [dtInt8, dtInt16, dtInt32], op_i32_sub);

  RegInstruction(icMul, [dtInt8, dtInt16, dtInt32], op_i32_mul);

end.
