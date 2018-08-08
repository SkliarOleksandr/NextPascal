unit IL2VMTranslator;

//{$DEFINE USE_ONLY_FAST_INVOKE}

{$I compilers.inc}
interface

uses
  System.SysUtils, System.Classes, Generics.Defaults, Generics.Collections, System.Math,
  NPCompiler.Utils, NPCompiler.DataTypes, IL.Types, IL.TypeInfo, VM.Core, VM.Types, VM.Invoke,
  ILTranslator; // system

type

  TVMRegister = (
    VM_R0,
    VM_R1,
    VM_R2,
    VM_R3,
    VM_R4,
    VM_R5,
    VM_R6,
    VM_R7,
    VM_R8,
    VM_R9,
    VM_R10,
    VM_R11,
    VM_R12,
    VM_R13,
    VM_R14,
    VM_R15
  );

  TVMCodeRegs = (
    vmcrNone,         // регистры не используются
    vmcrDst,          // 1 регистр - Dst
    vmcrSrc,          // 1 регистр - Src
    vmcrDstSrc,       // оба регистра Dst и Src
    vmcrDstSrc1Src2   // три регистра Dst, Src1 и Src2
  );

  TVMTranslator = class(TILTranslator)
  private
    FStrictRTTI: TILMemoryStream;           // обязательная RTTI сборки (если не пишется основная RTTI)
    FVMCodeLines: TVMCodeLines;
    FFixTable: TList;                       // таблица корректировки-смещений
  protected
    procedure MakeImportTable; override;
    {процедура генерирует сортированный список всех процедур и кадров их стека}
    procedure MakeVMProcsSearchList;
    function CheckOpt1(ILCode: TVMCode; Data: NativeUInt): Boolean;
    function CHECK_ST_C_I32_APPLY(const Src: TILArgument; Dst: PRTTIType; DstIsReference: Boolean): Boolean; overload;
    function CHECK_ST_C_I32_APPLY(const Src, Dst: TILArgument): Boolean; overload;
    function CHECK_MOVE_L_I32_APPLY(const Src: TILArgument; Dst: PRTTIType; DstIsReference: Boolean): Boolean; overload;
    function CHECK_MOVE_L_I32_APPLY(const Src, Dst: TILArgument): Boolean; overload;
    function CHECK_CMP_L_I32_APPLY(const Left, Right: TILArgument): Boolean; overload;
    function CHECK_CMP_L_C32_APPLY(const Left, Right: TILArgument): Boolean; overload;
    function GetDstInstruction(VMCodeClass: TVMCodeClass; const Arg: TILArgument): TVMCode;
    function GetLDStrongPTRInstruction(const Source: TILArgument): TVMCode;
    function GetVMConstsAsString: string;
    function GetVMCodeAsString: string;
    procedure SetBreakPoints;
    procedure GetProcVMCodeText(Proc: TILProc; AStr: TStrings);
    procedure WriteExportProcParams(Proc: TILProc; EProc: PVMExportProc);
    procedure WriteVMExportProcs(ILUnit: TILUnit); override;
    function GetMaxTypeInfo(const L, R: TILArgument): TOffset;
    function GetRAWMemAsString(Offset: TOffset; Size: Integer): string;
  protected
    function GetProcDump(const Proc: TILProc): string;
    procedure LoadILSimpleConst(Stream: TStream; DataType: PRTTIType); override;
    procedure LoadILConsts(ILUnit: TILUnit; Stream: TStream); override;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode); overload; inline;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister); overload;

    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Data: NativeUInt); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst: TVMRegister; Data: NativeUInt); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister; Data: NativeUInt); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister; Data: NativeUInt); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Data1, Data2: NativeUInt); overload;

    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; const Arg: TVMCodeArg); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst: TVMRegister; const Arg: TVMCodeArg); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister; const Arg: TVMCodeArg); overload;
    procedure WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister; const Arg: TVMCodeArg); overload;

    procedure WriteLDSelfInstruction(const Context: TILTContext; const ARG: TILArgument; Dst: TVMRegister = VM_R0);
    procedure WriteLDInstruction(CND: TILCondition; Variable: PILVariable; Dst: TVMRegister = VM_R0); overload; inline;
    procedure WriteLDInstruction(const Context: TILTContext; const ARG: TILArgument; Dst: TVMRegister = VM_R0); overload;
    procedure WriteLDConstant(CND: TILCondition; const Data: NativeUInt; Dst: TVMRegister = VM_R0);
    procedure WriteLDConstant64(CND: TILCondition; const Data: Int64; Dst: TVMRegister);
    procedure WriteSTInstruction(const Context: TILTContext; const ARG: TILArgument; Src: TVMRegister = VM_R0); overload; inline;
    procedure WriteSTInstruction(const Context: TILTContext; OverrideCND: TILCondition; const ARG: TILArgument; Src: TVMRegister = VM_R0); overload; inline;

    procedure WriteExternalCALLInstruction(const Context: TILTContext; P: TILProc);
    procedure WriteInternalCALLInstruction(const Context: TILTContext; PROCADDR: NativeUInt);
    procedure WriteVirtualCALLInstruction(const Context: TILTContext; Method: PILMethod);
    procedure WriteInterfaceCALLInstruction(const Context: TILTContext; const Method: PILMethod);
    procedure WriteInderectInternalCALLInstruction(const Context: TILTContext; const ARG: TILArgument);

    procedure Translate_NOPE(const Ctx: TILTContext); override;
    procedure Translate_2OperandOpCode(const Ctx: TILTContext; var Args: TIL_DS_Args); override;
    procedure Translate_3OperandOpCode(const Ctx: TILTContext; var Args: TIL_DSS_Args); override;
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
    procedure Translate_MOVEZERO(const Context: TILTContext; var Args: TIL_MOVEZERO_ARGS); override;
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
    procedure Translate_LDPTR_Instruction(const Ctx: TILTContext; var Dst: TILArgument; const Src: TILArgument); override;
    procedure Translate_LDMETHODPTR_Instruction(const Ctx: TILTContext; const Dst, Method: TILArgument); override;
    ////////////////////////////////////////////////////////////
    procedure CheckConvert(const Context: TILTContext; Src: TVMRegister; Destination, Source: TOffset);
    procedure DoCorrectILOffsets(Proc: TILProc);
    procedure CorrectVMTOffsets(const ClassInfo: PRTTIClass);
    procedure CorrectIMTOffsets(const ClassInfo: PRTTIClass);
    procedure CorrectRTTIOffsets;
    procedure CorrectOffsets; override;
    procedure CorrectProcJMPOffsets(Proc: TILProc); override;

    // возвращает инструкцию загрузки адреса аргумента в регистр
    function GetLDPTRInstruction(const Source: TILArgument): TVMCode;

    // возвращает инструкцию загрузки аргумента в регистр
    function GetLDInstruction(const Source: TILArgument): TVMCode; overload; inline;
    function GetLDInstruction(TypeInfoOffset: TOffset; ArgType: TILArgumentType): TVMCode; overload;
    function GetSTInstruction(const Arg: TILArgument): TVMCode;  overload; inline;
    function GetSTPTRInstruction(ArgType: TILArgumentType): TVMCode;  overload;
    function GetSTInstruction(TypeInfoOffset: TOffset; ArgType: TILArgumentType): TVMCode; overload;
    function GetInstruction(ILCode: TILCode; TypeInfoOffset: TOffset): TVMCode;
    function ArrayRTTI(const TypeInfoOffset: TOffset): PRTTIArray; inline;

    function GetString(const Name: TOffset; DataTypeID: TDataTypeID): UnicodeString; overload; inline;
    function GetDimOffset(ArrayTypeInfo: TOffset; DimNumber: UInt32): Integer;
    function GetInvokeAdapterID(Proc: TILProc): Integer;

    function GetResultParamDataSize(TypeInfo: PRTTIType): Integer;
    function GetMemberOffset(RecordTypeInfo: TOffset; MemberIndex: Integer): Int32;
    function GetMemberTypeInfo(RecordTypeInfo: TOffset; MemberIndex: Integer; out FieldRTTI: PRTTIField): TOffset;
    function GetFullImportName(TypeInfo: PRTTIType): string;
    // ищет и связывает импортируемую процедру
    procedure MapImportProc(Proc: TILProc); override;
    procedure MapImportMethod(aType: PRTTIType; Proc: TILProc); override;
    procedure MakeNCallArgsInfo(ImportProc: TILProc);
    {закгружает тела методов}
    procedure WriteStrictRTTISection;
    procedure WriteProcsRTTI(ILUnit: TILUnit); override;
    procedure WriteTypesRTTIArray(ILUnit: TILUnit); override;
    procedure WriteUnitsRTTIArray(UnitsCount: Integer); override;
    procedure WriteFixTable(Stream: TStream);
    procedure AddFixOffset(Offset: TOffset); override;
    procedure WriteProcProlog(Proc: TILProc); override;
    procedure WriteProcEpilog(Proc: TILProc; LastILCode: TILCode); override;
    procedure AfterLoadILGlobalVars(ILUnit: TILUnit); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    ////////////////////////////////////////////////////////
    procedure SaveTargetCode(Stream: TStream); override;
    property VMCodeAsString: string read GetVMCodeAsString;
    property VMCodeLines: TVMCodeLines read FVMCodeLines;
    function GetVMCodeLineOffset(SrcTextLine: Integer): TOffset;
    function GetVMCodeLine(SrcTextLine: Integer): Integer;
  end;

implementation

uses TypInfo;// {$IFDEF DEBUG}, Dialogs{$ENDIF};

const
  ST_L_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}ST_L_I32{$ELSE}ST_L_I64{$ENDIF};
  ST_R_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}ST_R_I32{$ELSE}ST_R_I64{$ENDIF};
  ST_G_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}ST_G_I32{$ELSE}ST_G_I64{$ENDIF};
  ST_D_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}ST_D_I32{$ELSE}ST_D_I64{$ENDIF};
  FMA_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}FMA_U32{$ELSE}FMA_U64{$ENDIF};
  ADD_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}ADD_U32{$ELSE}ADD_U64{$ENDIF};
  SUB_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}SUB_I32{$ELSE}SUB_I64{$ENDIF};
  LD_C_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}LD_C_I32{$ELSE}LD_C_I64{$ENDIF};
  LD_L_F64: TVMCode = LD_L_I64;
  LD_G_F64: TVMCode = LD_G_I64;
  LD_L_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}LD_L_I32{$ELSE}LD_L_I64{$ENDIF};
  LD_R_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}LD_R_I32{$ELSE}LD_R_I64{$ENDIF};
  LD_G_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}LD_G_I32{$ELSE}LD_G_I64{$ENDIF};
  LD_D_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}LD_D_I32{$ELSE}LD_D_I64{$ENDIF};
  CLR_L_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}CLR_L_I32{$ELSE}CLR_L_I64{$ENDIF};
  CLR_R_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}CLR_R_I32{$ELSE}CLR_R_I64{$ENDIF};
  CLR_G_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}CLR_G_I32{$ELSE}CLR_G_I64{$ENDIF};
  CMP_NATIVE: TVMCode = {$IFDEF PTR_SIZE4}CMP_I32{$ELSE}CMP_I64{$ENDIF};
  {$IFNDEF CPUX64}
  STR_MOVE: TVMCode = MOVE_ARRAY;
  STR_LENGTH: TVMCode = ARRAY_LENGTH;
  STR_INCREF: TVMCode = ARRAY_INCREF;
  {$ENDIF}


var
  _RegInstructions: array [TILCode] of array [TDataTypeID] of TVMCode;
  _LoadInstructions: array [0..1] of array [TDataTypeID] of array [TILArgumentType] of TVMCode;
  _ConvertInstructions: array [0..1] of array [TDataTypeID] of array [TDataTypeID] of TVMCode;
  _StoreInstructions: array [TDataTypeID] of array [TILArgumentType] of TVMCode;
  _ClearInstructions: array [TDataTypeID] of array [TILArgumentType] of TVMCode;
  _DestInstructions: array [TVMCodeClass] of array [TDataTypeID] of TVMCode;
  _LDS: array [TVMCode] of TVMCode;

function MakeInstruction(ILCode: TVMCode; Condition: TILCondition; ArgsCount: Integer; ArgType: TVMArgType = VMARG_IMM): NativeUInt; overload; inline;
begin
  Result := (UInt32(ArgType) shl 31) + UInt32(ArgsCount shl 24) + (UInt32(Condition) shl 8) + UInt32(ILCode);
end;

function MakeInstruction(ILCode: TVMCode; Dst, Src: TVMRegister; Condition: TILCondition; ArgsCount: Integer; ArgType: TVMArgType = VMARG_IMM): NativeUInt; overload; inline;
begin
  Result := (UInt32(ArgType) shl 31) + UInt32(ArgsCount shl 24) + (UInt32(Dst) shl 20) + (UInt32(Src) shl 16) + (UInt32(Condition) shl 8) + UInt32(ILCode);
end;

function MakeInstruction(ILCode: TVMCode; Dst, Src1, Src2: TVMRegister; Condition: TILCondition; ArgsCount: Integer; ArgType: TVMArgType = VMARG_IMM): NativeUInt; overload; inline;
begin
  Result := (UInt32(ArgType) shl 31) + UInt32(ArgsCount shl 24) + (UInt32(Dst) shl 20) + (UInt32(Src1) shl 16) + (UInt32(Src2) shl 12) + (UInt32(Condition) shl 8) + UInt32(ILCode);
end;

function MemAlign(Value: Integer; Align: Integer = 4): Integer; overload;
var
  M: Integer;
begin
  M := (Value mod Align);
  if M = 0 then
    Result := Value
  else
    Result := Value + (Align - M);
end;

procedure CheckVMCode(Code: TVMCode);
begin
  if Code = VM_NOPE then
    raise Exception.Create('Invalid VM code');
end;

destructor TVMTranslator.Destroy;
begin
  FFixTable.Free;
  FVMCodeLines.Free;
  FreeAndNil(FStrictRTTI);
  inherited;
end;

function TVMTranslator.GetString(const Name: TOffset; DataTypeID: TDataTypeID): UnicodeString;
begin
  if Name > 0 then
  case DataTypeID of
    dtAnsiString: Result := UnicodeString(AnsiString(PByte(IMG.Memory) + Name));
    dtString: Result := UnicodeString(PByte(IMG.Memory) + Name);
  end else
    Result := '';
end;

procedure TVMTranslator.LoadILSimpleConst(Stream: TStream; DataType: PRTTIType);
var
  Idx: Integer;
begin
  case DataType.DataTypeID of
    dtInt8: IMG.WriteInt8(Stream.ReadInt8);
    dtUInt8, dtAnsiChar, dtBoolean: IMG.WriteUInt8(Stream.ReadUInt8);
    dtInt16: IMG.WriteInt16(Stream.ReadInt16);
    dtUInt16, dtChar: IMG.WriteUInt16(Stream.ReadUInt16);
    dtInt32: IMG.WriteInt32(Stream.ReadInt32);
    dtUInt32: IMG.WriteUInt32(Stream.ReadUInt32);
    dtInt64: IMG.WriteInt64(Stream.ReadInt64);
    dtUInt64: IMG.WriteUInt64(Stream.ReadUInt64);
    dtString, dtAnsiString: begin
      Idx := Stream.ReadStretchUInt;
      IMG.WriteUInt32(FStrLiterals[Idx].Offset);
    end;
    dtFloat32: IMG.WriteFloat32(Stream.ReadFloat32);
    dtFloat64: IMG.WriteFloat64(Stream.ReadFloat64);
    dtEnum, dtSet, dtRange: begin
      case DataType.DataSize of
        1: IMG.WriteUInt8(Stream.ReadUInt8);
        2: IMG.WriteUInt16(Stream.ReadUInt16);
        4: IMG.WriteUInt32(Stream.ReadUInt32);
        8: IMG.WriteUInt64(Stream.ReadUInt64);
      else
        raise Exception.CreateFmt('Type "%s" is not supported', [GetDataTypeName(DataType.DataTypeID)]);
      end;
    end;
    dtVariant: begin
      IMG.WriteUInt64(Stream.ReadUInt64);
      IMG.WriteUInt64(Stream.ReadUInt64);
    end;
  else
    raise Exception.CreateFmt('Type "%s" is not supported', [GetDataTypeName(DataType.DataTypeID)]);
  end;
end;

procedure TVMTranslator.LoadILConsts(ILUnit: TILUnit; Stream: TStream);
var
  i, j, c, Offset, ElementCount, CSize: Integer;
  ArrayTypeInfo: PRTTIArray;
  TypeInfo, ElementTypeInfo: PRTTIType;
  CItem: PConstInfoRec;
begin
  c := Stream.ReadStretchUInt;
  if c = 0 then
    Exit;

  SetLength(ILUnit.Consts, c);
  for i := 0 to c - 1 do begin
    Offset := ReadTypeSpecInfo(Stream);
    TypeInfo := GetTypeInfo(Offset);
    CItem := Addr(ILUnit.Consts[i]);
    CItem.TypeInfo := Offset;
    case TypeInfo.DataTypeID of
      dtDynArray: begin
        ArrayTypeInfo := PRTTIArray(TypeInfo);
        ElementCount := Stream.ReadStretchUInt;
        ElementTypeInfo := GetTypeInfo(ArrayTypeInfo.ElementTypeInfoOffset);
        if ElementCount = 0 then
          continue; // tmp

        IMG.WriteInt32(-1);                 // refcaunt = -1
        IMG.WriteNativeInt(ElementCount);   // array length
        CItem.Value := IMG.Position;
        for j := 0 to ElementCount - 1 do
          LoadILSimpleConst(Stream, ElementTypeInfo);

        CItem.Size := IMG.Position - CItem.Value + (4 + PTR_SIZE);
      end;
      dtRecord: LoadILRecordConst(ILUnit, Stream, CItem);
    else
      CSize := TypeInfo.DataSize;
      CItem.Value := IMG.Position;
      CItem.Size := CSize;
      IMG.CopyFrom(Stream, CSize);
    end;
  end;
end;

procedure TVMTranslator.MakeImportTable;
var
  ImportItemIdx, ui, pi, ti: Int32;
  U: TILUnit;
  P: TILProc;
  T: PILType;
  ImportTable: PImportTable;
  ImportEntry: PImportEntry;
  Struct: PRTTIType;
begin
  ImportItemIdx := 0;
  FIMGHeader.ImportTable := IMG.Position;
  for ui := 0 to Length(FUnits) - 1 do
  begin
    U := FUnits[ui];
    for ti := 0 to Length(U.Types) - 1 do
    begin
      T := addr(U.Types[ti]);
      Struct := GetTypeInfo(T.Offset);
      if Struct.ImportName <> 0 then
      for pi := 0 to Length(T.Methods) - 1 do
      begin
        P := T.Methods[pi];
        if P.IsImported then begin
          ImportTable := GetIMGPtr(FIMGHeader.ImportTable);
          ImportEntry := addr(ImportTable[ImportItemIdx]);
          ImportEntry.Struct := Struct.Name;
          ImportEntry.Name := P.ImportName;
          ImportEntry.LibName := P.ImportLib;
          if Struct.DataTypeID = dtInterface then
          begin
            ImportEntry.ADDR := P.Offset;
            ImportEntry.IsIntfMethod := True;
          end else begin
            ImportEntry.ADDR := 0;
            ImportEntry.IsIntfMethod := False;
          end;
          ImportEntry.Adapter := P.InvokeAdapterID;
          Inc(ImportItemIdx);
        end;
      end;
    end;
    for pi := 0 to Length(U.Procs) - 1 do
    begin
      P := U.Procs[pi];
      if P.IsImported then begin
        ImportTable := GetIMGPtr(FIMGHeader.ImportTable);
        ImportEntry := addr(ImportTable[ImportItemIdx]);
        ImportEntry.Struct := 0;
        ImportEntry.Name := P.ImportName;
        ImportEntry.LibName := P.ImportLib;
        ImportEntry.ADDR := 0;
        ImportEntry.Adapter := P.InvokeAdapterID;
        Inc(ImportItemIdx);
      end;
    end;
  end;
  IMG.IncPosition(SizeOF(TIMGImportEntry)*FImportTableCount);
end;

type
  TArgTypeID = (
    atInt8,
    atInt16,
    atInt32,
    atInt64,
    atNativeInt,
    atDynArray,
    atString,
    atInterface,
    atFloat32,
    atFloat64,

    atSet1,
    atSet2,
    atSet3,
    atSet4,
    atSet5,
    atSet6,
    atSet7,
    atSet8,
    atSetN,

    atRecord1,
    atRecord2,
    atRecord3,
    atRecord4,
    atRecord5,
    atRecord6,
    atRecord7,
    atRecord8,
    atRecordN,

    atStaticArray1,
    atStaticArray2,
    atStaticArray3,
    atStaticArray4,
    atStaticArray5,
    atStaticArray6,
    atStaticArray7,
    atStaticArray8,
    atStaticArrayN,

    atVariant
  );

function GetArgTID(DTID: TDataTypeID; TypeSize: Integer): TArgTypeID;
begin
  case DTID of
    dtInt8, dtUInt8, dtBoolean, dtAnsiChar: Result := atInt8;
    dtInt16, dtUInt16, dtChar: Result := atInt16;
    dtInt32, dtUInt32, dtEnum: Result := atInt32;
    dtInt64, dtUInt64: Result := atInt64;
    dtNativeInt, dtNativeUInt, dtOpenArray, dtPointer, dtClass, dtClassOf: Result := atNativeInt;
    dtFloat32: Result := atFloat32;
    dtFloat64: Result := atFloat64;
    dtAnsiString, dtString: Result := atString;
    dtDynArray: Result := atDynArray;
    dtVariant: Result := atVariant;
    dtSet: case TypeSize of
             1: Result := atSet1;
             2: Result := atSet2;
             3: Result := atSet3;
             4: Result := atSet4;
             5: Result := atSet5;
             6: Result := atSet6;
             7: Result := atSet7;
             8: Result := atSet8;
           else
             Result := atSetN;
           end;
    dtStaticArray: case TypeSize of
                     1: Result := atStaticArray1;
                     2: Result := atStaticArray2;
                     3: Result := atStaticArray3;
                     4: Result := atStaticArray4;
                     5: Result := atStaticArray5;
                     6: Result := atStaticArray6;
                     7: Result := atStaticArray7;
                     8: Result := atStaticArray8;
                   else
                     Result := atStaticArrayN;
                   end;
    dtRecord: case TypeSize of
                1: Result := atRecord1;
                2: Result := atRecord2;
                3: Result := atRecord3;
                4: Result := atRecord4;
                5: Result := atRecord5;
                6: Result := atRecord6;
                7: Result := atRecord7;
                8: Result := atRecord8;
              else
                Result := atRecordN;
              end;
    dtInterface: Result := atInterface;
    dtGuid: Result := atRecordN;
  else
    raise Exception.CreateFmt('type %d is not supported', [Ord(DTID)]);
  end;
end;

procedure TVMTranslator.MakeNCallArgsInfo(ImportProc: TILProc);
var
  i, sp, c: Integer;
  Params: TParams;
  Param: PRTTIField;
  PType: PRTTIType;
  TID: TArgTypeID;
begin
  Params := ImportProc.Params;
  c := Length(Params);
  if ImportProc.ProcType = ptFunction then
  begin
    PType := GetTypeInfo(ImportProc.Params[0].DataType);
    TID := GetArgTID(PType.DataTypeID, PType.DataSize);
    IMG.WriteNativeUInt(Ord(TID));
    sp := 1;
  end else begin
    sp := 0;
  end;

  if ImportProc.IsIntfMethod then
  begin
    IMG.WriteNativeInt(c + 1 - sp  {$IFDEF FPC} - 1 {$ENDIF}); // В FPC для динимаческих массивово хранится LENGTH - 1
    IMG.WriteNativeUInt(Ord(atInterface));
  end else
    IMG.WriteNativeInt(c - sp  {$IFDEF FPC} - 1 {$ENDIF}); // В FPC для динимаческих массивово хранится LENGTH - 1

  for i := sp to c - 1 do
  begin
    Param := addr(Params[i]);
    PType := GetTypeInfo(Param.DataType);
    if Param.IsReference then
      TID := atNativeInt
    else
      TID := GetArgTID(PType.DataTypeID, PType.DataSize);
    IMG.WriteNativeUInt(Ord(TID) + (PType.DataSize shl 8));
  end;
end;

procedure TVMTranslator.MakeVMProcsSearchList;
var
  Procs: TList<TILProc>;
  ui, ti, pi, c: Integer;
  UN: TILUnit;
  TP: PILType;
  PR: TILProc;
  s: string;
begin
  Procs := TList<TILProc>.Create;
  try
    for ui := 0 to Length(FUnits) - 1 do
    begin
      UN := FUnits[ui];
      for ti := 0 to Length(UN.Types) - 1 do
      begin
        TP := Addr(UN.Types[ti]);
        for pi := 0 to Length(TP.Methods) - 1 do
          Procs.Add(TP.Methods[pi]);
      end;
      for pi := 0 to Length(UN.Procs) - 1 do
        Procs.Add(UN.Procs[pi]);
      Procs.Add(UN.InitProc);
      Procs.Add(UN.FinalProc);
    end;

    // сортируем список для быстрого поиска
    Procs.Sort(TComparer<TILProc>.Construct(
      function(const L, R: TILProc): Integer
      begin
        Result := NativeInt(L.Offset) - NativeInt(R.Offset);
      end
    ));

    c := Procs.Count;
    // записываем кол-во процедур (всех в сборке)
    FIMGHeader.ProcFramesCount := c;
    FIMGHeader.ProcFramesSection := IMG.Position;
    // пишем элементы
    for pi := 0 to c - 1 do
    begin
      PR := Procs[pi];
      s := GetProcName(PR);
      IMG.WriteNativeUInt(PR.Offset);    // смещение(в последствии адрес) процедуры
      IMG.WriteNativeUInt(PR.ProcInfo);  // смещение(в последствии адрес) RTTI процедуры
    end;
  finally
    Procs.Free;
  end;
end;

function TVMTranslator.CheckOpt1(ILCode: TVMCode; Data: NativeUInt): Boolean;
var
  LastCMD: TVMCode;
  LastDATA: NativeUInt;
begin
  Exit(False);
  //==========================================================================================================
  // оптимизация: если предыдущая инструкция - STORE, то следующую инструкцию LOAD нет смысла генерировать
  // если обращение в обоих случаях было к одному и тому же обьекту
  //==========================================================================================================
  if (IMG.Position > PTR_SIZE*2) then
  begin
    LastCMD := TVMCode(PNUInt(PByte(IMG.MemoryPosition) - PTR_SIZE*2)^);
    LastData := PNUInt(PByte(IMG.MemoryPosition) - PTR_SIZE*1)^;
    if (_LDS[ILCode] = LastCMD) and (Data = LastDATA) then
      Exit(True);
  end;
  //==========================================================================================================begin

  Result := False
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Data: NativeUInt);
begin
  if CheckOpt1(ILCode, Data) then Exit;
  IMG.WriteNativeUInt(MakeInstruction(ILCode, VM_R0, VM_R0, CND, 1));
  IMG.WriteNativeUInt(Data);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, CND, 0));
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, Dst, Src, VM_R0, CND, 0));
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, Dst, Src1, Src2, CND, 0));
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst: TVMRegister; Data: NativeUInt);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, Dst, VM_R0, VM_R0, CND, 1));
  IMG.WriteNativeUInt(Data);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister; Data: NativeUInt);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, Dst, Src, VM_R0, CND, 1));
  IMG.WriteNativeUInt(Data);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister; Data: NativeUInt);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, Dst, Src1, Src2, CND, 1));
  IMG.WriteNativeUInt(Data);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Data1, Data2: NativeUInt);
begin
  IMG.WriteNativeUInt(MakeInstruction(ILCode, CND, 2));
  IMG.WriteNativeUInt(Data1);
  IMG.WriteNativeUInt(Data2);
end;

procedure TVMTranslator.WriteInternalCALLInstruction(const Context: TILTContext; PROCADDR: NativeUInt);
var
  CallCode: NativeUInt;
begin
  CallCode := MakeInstruction(CALL_PROC, Context.Cond, 2);
  IMG.WriteNativeUInt(CallCode);
  IMG.WriteNativeUInt(PROCADDR);
  IMG.WriteNativeUInt(Context.Proc.GetStackSize());
end;

procedure TVMTranslator.WriteVirtualCALLInstruction(const Context: TILTContext; Method: PILMethod);
var
  CallCode: NativeUInt;
begin
  WriteLDInstruction(Context, Method.Self);
  CallCode := MakeInstruction(CALL_VIRT, Context.Cond, 2);
  IMG.WriteNativeUInt(CallCode);
  IMG.WriteNativeUInt(Method.Proc.VirtualIndex);
  IMG.WriteNativeUInt(Context.Proc.GetStackSize());
end;

procedure TVMTranslator.WriteInterfaceCALLInstruction(const Context: TILTContext; const Method: PILMethod);
var
  CallCode: NativeUInt;
  Intf: PRTTIInterface;
begin
  Intf := PRTTIInterface(GetTypeInfo(Method.Proc.Struct.Offset));
  WriteLDInstruction(Context, Method.Self);
  CallCode := MakeInstruction(CALL_INTF, Context.Cond, 3);
  IMG.WriteNativeUInt(CallCode);
  IMG.WriteNativeUInt(Intf.InterfaceID);
  IMG.WriteNativeUInt(Method.Proc.Offset);
  IMG.WriteNativeUInt(Context.Proc.GetStackSize());
end;

procedure TVMTranslator.WriteInderectInternalCALLInstruction(const Context: TILTContext; const ARG: TILArgument);
var
  CallCode: NativeUInt;
begin
  case ARG.ArgumentType of
    atLocal: WriteInstruction(Context.Cond, LD_L_NATIVE, ARG);
    atGlobal: WriteInstruction(Context.Cond, LD_G_NATIVE, ARG);
    atReference: begin
      WriteInstruction(Context.Cond, LD_L_NATIVE, ARG);
      WriteInstruction(Context.Cond, LD_D_NATIVE);
    end;
  end;
  CallCode := MakeInstruction(CALL_INDIRECT, Context.Cond, 1);
  IMG.WriteNativeUInt(CallCode);
  IMG.WriteNativeUInt(Context.Proc.GetStackSize());
end;

procedure TVMTranslator.WriteExternalCALLInstruction(const Context: TILTContext; P: TILProc);
var
  CallCode, Data: NativeUInt;
  ArgsCnt: Integer;
begin
  if P.InvokeAdapterID <> -1 then
  begin
    if ((P.Flags and ILPROC_VIRTUAL) <> 0) and not P.IsIntfMethod then
      CallCode := MakeInstruction(CALL_EXT_FASTV, Context.Cond, 2)
    else
    if ((P.Flags and ILPROC_VIRTUAL) <> 0) and P.IsIntfMethod then
    begin
      if P.ProcType = ptProcedure then
        CallCode := MakeInstruction(CALL_EXT_FAST_INTF_PROC, Context.Cond, 2)
      else
        CallCode := MakeInstruction(CALL_EXT_FAST_INTF_FUNC, Context.Cond, 2)
    end else
      CallCode := MakeInstruction(CALL_EXT_FAST, Context.Cond, 2);
    Data := P.ImportIndex;
  end else begin
    {$IFDEF USE_ONLY_FAST_INVOKE}
    raise Exception.Create('Unknown procedure invoke type is not supported');
    {$ELSE}
    ArgsCnt := Length(P.Params);
    if P.IsIntfMethod then
      Inc(ArgsCnt);
    if P.ProcType = ptProcedure then
      CallCode := MakeInstruction(CALL_EXT_COMMON_PROC, Context.Cond, 3 + ArgsCnt)
    else
      CallCode := MakeInstruction(CALL_EXT_COMMON_FUNC, Context.Cond, 4 + ArgsCnt - 1);
    Data := P.ImportIndex;
    Assert(Data < (1 shl 24)); // проверка на максамальный размер индекса
    Data := Data + NativeUInt(Ord(P.CallConvention)) shl 24;
    {$ENDIF}
  end;
  IMG.WriteNativeUInt(CallCode);                     // VM opcode
  IMG.WriteNativeUInt(Data);                         // import table index and calling convention
  IMG.WriteNativeUInt(Context.Proc.GetStackSize());  // current proc stack size
  if P.InvokeAdapterID = -1 then
    MakeNCallArgsInfo(P);
end;

procedure TVMTranslator.WriteFixTable(Stream: TStream);
var
  i, c: Integer;
begin
  c := FFixTable.Count;
  if c > 0 then
  begin
    Stream.WriteUInt32(c);
    for i := 0 to c - 1 do
      Stream.WriteUInt32(Uint32(FFixTable[i]));
  end;
end;

function TVMTranslator.CHECK_MOVE_L_I32_APPLY(const Src: TILArgument; Dst: PRTTIType; DstIsReference: Boolean): Boolean;
var
  CVCode: TVMCode;
  SrcTypeInfo: PRTTIType;
begin
  SrcTypeInfo := GetTypeInfo(Src.TypeInfo);
  CVCode := _ConvertInstructions[0, Dst.DataTypeID, SrcTypeInfo.DataTypeID];

  Result := (CVCode = VM_NOPE) and
            (Src.IsLocalVar) and
            (not DstIsReference) and
            (SrcTypeInfo.DataSize = Dst.DataSize) and
            (Dst.DataTypeID in [dtInt8, dtInt16, dtInt32, dtUInt8, dtUInt16, dtUInt32, dtChar, dtAnsiChar]);
end;

function TVMTranslator.CHECK_CMP_L_C32_APPLY(const Left, Right: TILArgument): Boolean;
var
  CVCode: TVMCode;
  LTypeInfo,
  RTypeInfo: PRTTIType;
begin
  LTypeInfo := GetTypeInfo(Left.TypeInfo);
  RTypeInfo := GetTypeInfo(Right.TypeInfo);

  CVCode := _ConvertInstructions[0, RTypeInfo.DataTypeID, LTypeInfo.DataTypeID];

  Result := (CVCode = VM_NOPE) and
            (Left.IsLocalVar) and
            (Right.ArgumentType = atImmConst) and
            (LTypeInfo.DataTypeID in [dtInt32, dtUInt32]) and
            (RTypeInfo.DataTypeID in [dtInt8, dtInt16, dtInt32, dtUInt8, dtUInt16, dtUInt32, dtChar, dtAnsiChar]);
end;

function TVMTranslator.CHECK_CMP_L_I32_APPLY(const Left, Right: TILArgument): Boolean;
var
  CVCode: TVMCode;
  SrcTypeInfo,
  DstTypeInfo: PRTTIType;
begin
  SrcTypeInfo := GetTypeInfo(Left.TypeInfo);
  DstTypeInfo := GetTypeInfo(Right.TypeInfo);

  CVCode := _ConvertInstructions[0, DstTypeInfo.DataTypeID, SrcTypeInfo.DataTypeID];

  Result := (CVCode = VM_NOPE) and
            (Left.IsLocalVar) and
            (Right.IsLocalVar) and
            (SrcTypeInfo.DataSize = DstTypeInfo.DataSize) and
            (SrcTypeInfo.DataTypeID in [dtInt32, dtUInt32]) and
            (DstTypeInfo.DataTypeID in [dtInt32, dtUInt32]);
end;

function TVMTranslator.CHECK_MOVE_L_I32_APPLY(const Src, Dst: TILArgument): Boolean;
var
  DstTypeInfo: PRTTIType;
begin
  if Dst.IsLocalVar then
  begin
    DstTypeInfo := GetTypeInfo(Dst.TypeInfo);
    Result := CHECK_MOVE_L_I32_APPLY(Src, DstTypeInfo, Dst.AsVariable.RTTI.IsReference);
  end else
    Result := False;
end;

function TVMTranslator.CHECK_ST_C_I32_APPLY(const Src, Dst: TILArgument): Boolean;
var
  DstTypeInfo: PRTTIType;
begin
  if Dst.IsLocalVar then
  begin
    DstTypeInfo := GetTypeInfo(Dst.TypeInfo);
    Result := CHECK_ST_C_I32_APPLY(Src, DstTypeInfo, Dst.AsVariable.RTTI.IsReference);
  end else
    Result := False;
end;

function TVMTranslator.CHECK_ST_C_I32_APPLY(const Src: TILArgument; Dst: PRTTIType; DstIsReference: Boolean): Boolean;
var
  CVCode: TVMCode;
  SrcTypeInfo: PRTTIType;
begin
  SrcTypeInfo := GetTypeInfo(Src.TypeInfo);
  CVCode := _ConvertInstructions[0, Dst.DataTypeID, SrcTypeInfo.DataTypeID];

  Result := (CVCode = VM_NOPE) and
            (not DstIsReference) and
            (Src.ArgumentType = atImmConst) and
            (Src.ArgClass = ARG_CONST) and
            (Dst.DataTypeID in [dtInt32, dtUInt32]);
end;

function TVMTranslator.ArrayRTTI(const TypeInfoOffset: TOffset): PRTTIArray;
begin
  Result := PRTTIArray(PByte(FRTTI.Memory) + TypeInfoOffset);
  Assert(Result.DataTypeID in [dtStaticArray, dtDynArray, dtString, dtAnsiString]);
end;

procedure TVMTranslator.CheckConvert(const Context: TILTContext; Src: TVMRegister; Destination, Source: TOffset);
var
  ILCode: TVMCode;
  DTypeInfo,
  STypeInfo: PRTTIType;
begin
  DTypeInfo := GetTypeInfo(Destination);
  STypeInfo := GetTypeInfo(Source);
  ILCode := _ConvertInstructions[0, DTypeInfo.DataTypeID, STypeInfo.DataTypeID];
  if ILCode <> VM_NOPE then
     WriteInstruction(Context.Cond, ILCode, Src, Src);
end;

function GetVMCodeRegParamsCout(VMCode: TVMCode): TVMCodeRegs;
begin
  case VMCode of
    LD_C_I32,
    LD_C_U32,
    LD_C_I64,
    LD_C_F32,
    LD_C_F64,
    //-----------
    LD_L_I8,
    LD_L_U8,
    LD_L_I16,
    LD_L_U16,
    LD_L_I32,
    LD_L_I64,
    LD_L_F32,
    LD_L_PTR,
    //-----------
    LD_R_I8,
    LD_R_I16,
    LD_R_I32,
    LD_R_I64,
    LD_R_F32,
    //-----------
    LD_G_I8,
    LD_G_U8,
    LD_G_I16,
    LD_G_U16,
    LD_G_I32,
    LD_G_I64,
    LD_G_F32,
    LD_G_PTR: Result := vmcrDst;
    //-----------
    LD_D_I8,
    LD_D_I16,
    LD_D_I32,
    LD_D_I64: Result := vmcrDstSrc;
    LD_G_PROC: Result := vmcrDst;
    //-----------
    LD_C_ZERO: Result := vmcrDst;
    LD_C_ONE: Result := vmcrDst;
    //-----------
    MOVE_L_I32: Result := vmcrNone;
    ST_C_I32: Result := vmcrNone;
    //-----------
    ST_L_I8,
    ST_L_I16,
    ST_L_I32,
    ST_L_I64,
    ST_L_F32,
    ST_L_VAR,
    //-----------
    ST_R_I8,
    ST_R_I16,
    ST_R_I32,
    ST_R_I64,
    ST_R_F32,
    //-----------
    ST_G_I8,
    ST_G_I16,
    ST_G_I32,
    ST_G_I64,
    ST_G_F32,
    ST_G_VAR: Result := vmcrSrc;
    //-----------
    ST_D_I8,
    ST_D_I16,
    ST_D_I32,
    ST_D_I64,
    ST_D_F32: Result := vmcrDstSrc;
    //-----------
    CLR_L_I8,
    CLR_L_I16,
    CLR_L_I32,
    CLR_L_I64,
    CLR_L_F32,
    CLR_L_F64,
    //-----------
    CLR_R_I8,
    CLR_R_I16,
    CLR_R_I32,
    CLR_R_I64,
    CLR_R_F32,
    CLR_R_F64,
    //-----------
    CLR_G_I8,
    CLR_G_I16,
    CLR_G_I32,
    CLR_G_I64,
    CLR_G_F32,
    CLR_G_F64: Result := vmcrNone;
    //-----------
    CLR_D_I8,
    CLR_D_I16,
    CLR_D_I32,
    CLR_D_I64,
    CLR_D_F32,
    CLR_D_F64: Result := vmcrDst;
    //-----------
    MEM_SET,
    ARRAY_LENGTH,

    MOVE_REG,
    MOVE_REG_TO_MEM,
    MOVE_MEM_C: Result := vmcrDstSrc;
    MOVE_MEM: Result := vmcrDstSrc1Src2;

    ARRAY_INCREF,
    ARRAY_DECREF: Result := vmcrDst;

    OBJ_INCREF,
    OBJ_DECREF: Result := vmcrDst;

    OBJ_WEAKREF: Result := vmcrDst;
    OBJ_STRONGREF: Result := vmcrDst;
    WEAK_INCREF: Result := vmcrDst;
    WEAK_DECREF: Result := vmcrDst;

    STR_DECREF: Result := vmcrDst;
    STRU_CREATE: Result := vmcrDstSrc;
    STRA_CREATE: Result := vmcrDstSrc;

    ARRAY_MEM_ALLOC: Result := vmcrDstSrc;
    MEM_ALLOC: Result := vmcrDst;
    MEM_FREE: Result := vmcrDst;
    ARRAY_INIT: Result := vmcrDst;
    MOVE_ARRAY: Result := vmcrDstSrc;
    //-----------
    CMP_I32_C: Result := vmcrDst;
    CMP_L_C32: Result := vmcrNone;
    CMP_I32: Result := vmcrDstSrc;
    CMP_L_I32: Result := vmcrNone;
    CMP_I64: Result := vmcrDstSrc;
    CMP_F64: Result := vmcrDstSrc;
    CMP_ASTR: Result := vmcrDstSrc;
    CMP_USTR: Result := vmcrDstSrc;
    CMP_VAR: Result := vmcrDstSrc;
    CMP_TEST32: Result := vmcrDstSrc;
    CMP_TEST64: Result := vmcrDstSrc;
    CMP_MEM: Result := vmcrDstSrc;
    CMP_MEM_VS_REG: Result := vmcrDstSrc;
    //-----------
    INC_L_I32: Result := vmcrNone;
    ADD_I32_C: Result := vmcrDstSrc;
    ADD_L_I32_C: Result := vmcrNone;
    ADD_NUINT_C: Result := vmcrDstSrc;
    ADD_U32: Result := vmcrDstSrc1Src2;
    ADD_U64: Result := vmcrDstSrc1Src2;
    ADD_I32: Result := vmcrDstSrc1Src2;
    ADD_L_I32: Result := vmcrNone;
    ADD_I64: Result := vmcrDstSrc1Src2;
    ADD_F64: Result := vmcrDstSrc1Src2;
    ADD_F64_CI32: Result := vmcrDstSrc;
    ADD_ASTR: Result := vmcrDstSrc1Src2;
    ADD_USTR: Result := vmcrDstSrc1Src2;
    //-----------
    SUB_I32_C     : Result := vmcrDstSrc;
    SUB_F64_CI32  : Result := vmcrDstSrc;
    SUB_I32       : Result := vmcrDstSrc1Src2;
    SUB_I64       : Result := vmcrDstSrc1Src2;
    SUB_F64       : Result := vmcrDstSrc1Src2;
    //-----------
    MUL_C32       : Result := vmcrDstSrc;
    MUL_I32       : Result := vmcrDstSrc1Src2;
    MUL_I64       : Result := vmcrDstSrc1Src2;
    MUL_F64       : Result := vmcrDstSrc1Src2;
    //-----------
    FMA_U32       : Result := vmcrDstSrc1Src2;
    FMA_U64       : Result := vmcrDstSrc1Src2;
    //-----------
    IDIV_U32_C    : Result := vmcrDst;
    IDIV_I32      : Result := vmcrDstSrc1Src2;
    IDIV_I64      : Result := vmcrDstSrc1Src2;
    //-----------
    IMOD_U32_C    : Result := vmcrDst;
    IMOD_I32      : Result := vmcrDstSrc1Src2;
    IMOD_I64      : Result := vmcrDstSrc1Src2;
    //-----------
    DIV_I32      : Result := vmcrDstSrc1Src2;
    DIV_I64      : Result := vmcrDstSrc1Src2;
    DIV_F64      : Result := vmcrDstSrc1Src2;
    //-----------
    NEG_I32,
    NEG_I64,
    NEG_F64      :Result := vmcrDstSrc;
    //-----------
    BIN_SHL32,
    BIN_SHL64,
    BIN_SHR32,
    BIN_SHR64,
    //-----------
    BIN_AND32: Result := vmcrDstSrc1Src2;
    BIN_AND64: Result := vmcrDstSrc1Src2;
    BIN_AND32_C: Result := vmcrDst;
    BIN_AND64_C: Result := vmcrDst;
    //-----------
    BIN_OR32: Result := vmcrDstSrc1Src2;
    BIN_OR64: Result := vmcrDstSrc1Src2;
    BIN_OR32_C: Result := vmcrDst;
    BIN_OR64_C: Result := vmcrDst;
    //-----------
    BIN_XOR32: Result := vmcrDstSrc1Src2;
    BIN_XOR64: Result := vmcrDstSrc1Src2;
    //-----------
    BIN_NOT32,
    BIN_NOT64: Result := vmcrDstSrc;

    ETHROW: Result := vmcrSrc;
    SCHECKB: Result := vmcrSrc;
    DCHECKB: Result := vmcrDstSrc;
    ACHECKB: Result := vmcrDstSrc;
    VM_SET_METHOD_PTR: Result := vmcrDstSrc1Src2;

    CNV_VAR_TO_VALUE: Result := vmcrDstSrc;
    CNV_VALUE_TO_VAR: Result := vmcrDstSrc;

    CNV_UCHR_TO_USTR,
    CNV_ACHR_TO_ASTR: Result := vmcrDstSrc;
    CNV_ASTR_TO_USTR: Result := vmcrDstSrc;
    CNV_USTR_TO_ASTR: Result := vmcrDstSrc;

    VAR_RELEASE: Result := vmcrDst;
    VM_SYSMACRO: Result := vmcrDstSrc1Src2;

    {$IFDEF CPUX64}
    STR_INCREF: Result := vmcrDestination;
    STR_LENGTH: Result := vmcrBoth;
    STR_MOVE: Result := vmcrBoth;
    {$ENDIF}
  else
    Result := vmcrNone;
  end;
end;

procedure TVMTranslator.GetProcVMCodeText(Proc: TILProc; AStr: TStrings);
  function GetVarSize(V: PILVariable): Integer;
  var
    TI: PRTTIType;
  begin
    if V.RTTI.IsReference then
      Result := PTR_SIZE
    else begin
      TI := GetTypeInfo(V.RTTI.DataType);
      if TI.DataTypeID = dtClass then
        Result := PTR_SIZE
      else
        Result := TI.DataSize;
    end;
  end;
var
  M, MEnd, DPtr: PNUInt;
  Instruction: NativeUInt;
  MCode: TVMCode;
  i, Idx, DataCnt: integer;
  ADDR: TOffset;
  Str, CodeParams: string;
  NProc: TILProc;
  CND: TILCondition;
  DstReg, SrcReg1, SrcReg2: Integer;
  VR: PILVariable;
  VMCodeLine: TVMCodeLine;
begin
  for i := 0 to Length(Proc.NestedProcs) - 1 do begin
    NProc := Proc.NestedProcs[i];
    GetProcVMCodeText(NProc, AStr);
  end;
  ADDR := Proc.Offset - 0;
  AStr.Add(format('%d: proc %s.%s (stack: %d)', [ADDR, Proc.ILUnit.Name, GetProcName(Proc), Proc.GetStackSize()]));
  for i := 0 to Length(Proc.Vars) - 1 do
  begin
    VR := @(Proc.Vars[i]);
    AStr.Add('VAR ' + IntToStr(VR.RTTI.Offset) + ': ' + VR.Name + ' (size: ' + IntToStr(GetVarSize(VR))+ ')');
  end;
  Idx := 0;
  M := GetIMGPtr(Proc.Offset);
  MEnd := PNUInt(PByte(M) + Proc.CodeSize);
  while PByte(M) < PByte(MEnd) do
  begin
    ADDR := GetOffset(IMG.Memory, M);
    Instruction := M^;
    CND := TILCondition(Byte(Instruction shr 8) and 15);
    MCode := TVMCode(Instruction and 255);
    DataCnt := ((Instruction shr 24) and 127);
    Instruction := Instruction and (not (1 shl 31));
    SrcReg2 := (Instruction shr 12) and 15;
    SrcReg1 := (Instruction shr 16) and 15;
    DstReg := (Instruction shr 20) and 15;
    Inc(M);

    Str := TypInfo.GetEnumName(TypeInfo(TVMCode), Integer(MCode));

    if CND <> cNone then
      Str := Str + '[' + GetILConditionSymbol(CND) + ']';

    case GetVMCodeRegParamsCout(MCode) of
      vmcrNone: CodeParams := '';
      vmcrDst: CodeParams := 'R' + IntToStr(DstReg);
      vmcrSrc: CodeParams := 'R' + IntToStr(SrcReg1);
      vmcrDstSrc: CodeParams := format('R%d, R%d', [DstReg, SrcReg1]);
      vmcrDstSrc1Src2: CodeParams := format('R%d, R%d, R%d', [DstReg, SrcReg1, SrcReg2]);
    end;

    DPtr := M;
    for i := 0 to DataCnt - 1 do
    begin
      CodeParams := AddStringSegment(CodeParams, IntToStr(Int64(DPtr^)), ', ');
      Inc(DPtr);
    end;

    Str := Str + ' ' + CodeParams;

    AStr.Add(format('%d: %s', [ADDR, Str]));

    if Length(Proc.IL) > Idx then
    begin
      if (Idx < Length(Proc.IL) - 1) and (ADDR = Proc.IL[Idx + 1].Offset) then
        Inc(Idx);
      VMCodeLine.SrcTextLine := Proc.IL[Idx].Line;
      VMCodeLine.AsmTextLine := AStr.Count;
      VMCodeLine.UnitID := Proc.ILUnit.Index;
      VMCodeLine.Offset := ADDR;
      FVMCodeLines.Add(VMCodeLine);
    end;

    Inc(M, DataCnt);
  end;
  SetBreakPoints;
  AStr.Add('-----------------------------------');
end;

function TVMTranslator.GetRAWMemAsString(Offset: TOffset; Size: Integer): string;
var
  Ptr: PByte;
  i: Integer;
begin
  Result := '';
  Ptr := GetIMGPtr(Offset);
  for i := 1 to Size do
  begin
    Result := Result + IntToHex(Ptr^, 2) + ' ';
    Inc(Ptr);
  end;
end;

function TVMTranslator.GetVMConstsAsString: string;
var
  ui, ci: integer;
  UN: TILUnit;
  CItem: PConstInfoRec;
  CTypeInfo: PRTTIType;
  CName, ItemsStr, CItemStr: string;
begin
  Result := '';
  for ui := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[ui];
    if not Assigned(UN) then
      Exit;
    if Length(UN.Consts) = 0 then
      continue;
    ItemsStr := '';
    for ci := 0 to Length(UN.Consts) - 1 do
    begin
      CItem := addr(UN.Consts[ci]);
      CTypeInfo := GetTypeInfo(CItem.TypeInfo);                          //GetString(CTypeInfo.Name)
      CName := UN.Name + '.CONST$' + IntToStr(ci);
      CItemStr := format('%d: %s: %s(size: %d) = %s', [CItem.Value, CName, GetDataTypeName(CTypeInfo.DataTypeID), CItem.Size,
                         GetRAWMemAsString(CItem.Value, CItem.Size)]);
      ItemsStr := AddStringSegment(ItemsStr, CItemStr, #10);
    end;
    Result := Result + ItemsStr;
  end;
end;

function TVMTranslator.GetVMCodeAsString: string;
var
  ui, ti, i, j: integer;
  UN: TILUnit;
  VR: PILVariable;
  PR: TILProc;
  VT: PILType;
  Str: TStrings;
  S: string;
begin
  FVMCodeLines.Clear;
  Str := TStringList.Create;
  try
    Str.Add('STRING LITERALS:');
    // строковые летиралы
    for i := 0 to Length(FStrLiterals) - 1 do
    begin
      case FStrLiterals[i].DTID of
        dtString: S := '(U)';
        dtAnsiString: S := '(A)';
      end;
      Str.Add(IntToStr(FStrLiterals[i].Offset) + ': ' + S + GetString(FStrLiterals[i].Offset, FStrLiterals[i].DTID));
    end;
    Str.Add('-----------------------------------');
    // типы
    Str.Add('SYSTEM TYPES (offset in RTTI section):');
    if Length(FSystemTypes) > 0 then
    for i := 0 to Length(FSystemTypes) - 1 do begin
      VT := addr(FSystemTypes[i]);
      Str.Add(IntToStr(VT.Offset) + ': ' + GetDataTypeName(VT.ID));
    end;
    Str.Add('-----------------------------------');
    Str.Add('CONSTANTS:');
    Str.Add(GetVMConstsAsString);
    Str.Add('-----------------------------------');
    for ui := 0 to Length(FUnits) - 1 do
    begin
      UN := FUnits[ui];
      Str.Add('===================================');
      Str.Add('Unit: ' + UN.Name);
      Str.Add('===================================');
      // типы
      Str.Add('TYPES (offset in RTTI section):');
      if Length(UN.Types) > 0 then
      for i := 0 to Length(UN.Types) - 1 do begin
        VT := addr(UN.Types[i]);
        Str.Add(IntToStr(VT.Offset) + ': ' + VT.Name + ' = ' + GetDataTypeName(VT.ID));
      end;
      Str.Add('-----------------------------------');
      // RTTI процедур
      Str.Add('PROC RTTI (offset in RTTI section):');
      if Length(UN.Procs) > 0 then
      for i := 0 to Length(UN.Procs) - 1 do begin
        PR := UN.Procs[i];
        Str.Add(IntToStr(PR.ProcInfo) + ': ' + PR.Name);
      end;
      // RTTI методов
      Str.Add('METHODS RTTI (offset in RTTI section):');
      if Length(UN.Types) > 0 then
      for i := 0 to Length(UN.Types) - 1 do begin
        VT := addr(UN.Types[i]);
        for j := 0 to Length(VT.Methods) - 1 do
        begin
          PR := VT.Methods[j];
          Str.Add(IntToStr(PR.ProcInfo) + ': ' + VT.Name + '.' + PR.Name);
        end;
      end;
      Str.Add('-----------------------------------');
      Str.Add('GLOBAL VARS:');
      // глобальные переменные
      if Length(UN.Vars) > 0 then
      for i := 0 to Length(UN.Vars) - 1 do begin
        VR := addr(UN.Vars[i]);
        Str.Add(IntToStr(VR.RTTI.Offset) + ': ' + GetString(VR.RTTI.Name));
      end;
      Str.Add('-----------------------------------');
      Str.Add('PROCS:');
      // глобальные процедуры
      if Length(UN.Procs) > 0 then
      for i := 0 to Length(UN.Procs) - 1 do begin
        PR := UN.Procs[i];
        GetProcVMCodeText(PR, Str);
      end;
      Str.Add('-----------------------------------');
      Str.Add('METHODS:');
      // методы
      for ti := 0 to Length(UN.Types) - 1 do
      begin
        for i := 0 to Length(UN.Types[ti].Methods) - 1 do
        begin
          PR := UN.Types[ti].Methods[i];
          GetProcVMCodeText(PR, Str);
        end;
      end;
      // init proc
      if Assigned(UN.InitProc) then
        GetProcVMCodeText(UN.InitProc, Str);
      // final proc
      if Assigned(UN.FinalProc) then
        GetProcVMCodeText(UN.FinalProc, Str);
    end;
    Result := Str.Text;
  finally
    Str.Free;
  end;
end;

function TVMTranslator.GetVMCodeLine(SrcTextLine: Integer): Integer;
var
  i, PrevLine: Integer;
begin
  PrevLine := 0;
  for i := 0 to FVMCodeLines.Count - 1 do
  begin
    if (SrcTextLine <= FVMCodeLines[i].SrcTextLine) and
       (SrcTextLine >= PrevLine) then
      Exit(FVMCodeLines[i].AsmTextLine);
    PrevLine := FVMCodeLines[i].SrcTextLine;
  end;
  Result := 0;
end;

function TVMTranslator.GetVMCodeLineOffset(SrcTextLine: Integer): TOffset;
var
  i, PrevLine: Integer;
begin
  PrevLine := 0;
  for i := 0 to FVMCodeLines.Count - 1 do
  begin
    if (SrcTextLine <= FVMCodeLines[i].SrcTextLine) and
       (SrcTextLine >= PrevLine) then
      Exit(FVMCodeLines[i].Offset);
    PrevLine := FVMCodeLines[i].SrcTextLine;
  end;
  Result := 0;
end;

procedure TVMTranslator.DoCorrectILOffsets(Proc: TILProc);
var
  IP, MEnd: PNUInt;
  RawData: NativeUInt;
  VMCode: TVMCode;
  i, DataCnt: integer;
  NestedProc: TILProc;
  VMArgType: TVMArgType;
  VMArg: TVMCodeArg;
begin
  for i := 0 to Length(Proc.NestedProcs) - 1 do begin
    NestedProc := Proc.NestedProcs[i];
    DoCorrectILOffsets(NestedProc);
  end;
  IP := GetIMGPtr(Proc.Offset);
  MEnd := PNUInt(PByte(IP) + Proc.CodeSize);
  while PByte(IP) < PByte(MEnd) do begin
    RawData := IP^;
    VMCode := TVMCode(RawData and 255);
    VMArgType := TVMArgType((RawData shr 31) and 1);
    DataCnt := ((RawData shr 24) and 127);
    RawData := RawData and (not (1 shl 31));
    IP^ := RawData;
    Inc(IP);
    try
      if VMArgType = VMARG_VAR then
      begin
        VMArg := TVMCodeArg(IP^);
        VMArg.WriteData(Self, Proc, IP);
      end else
      case VMCode of
        CALL_PROC, LD_G_PROC: IP^ := TILProc(IP^).Offset;
      end;
    except
      on e: exception do
        raise Exception.CreateFmt('Correct VM code offsets ERROR[proc: %s code: %s addr: %d]: %s',
                                  [GetProcName(Proc), GetEnumName(TypeInfo(TVMCode), Ord(VMCode)), GetOffset(IMG.Memory, IP) - PTR_SIZE, e.Message]);
    end;
    Inc(IP, DataCnt);
  end;
end;

procedure TVMTranslator.CorrectVMTOffsets(const ClassInfo: PRTTIClass);
var
  i: Integer;
  VMT: PVMT;
begin
  VMT := GetIMGPtr(ClassInfo.VMT);
  for i := 0 to ClassInfo.VMTCount - 1 do
    VMT[i] := TILProc(VMT[i]).Offset;
end;

procedure TVMTranslator.CorrectIMTOffsets(const ClassInfo: PRTTIClass);
var
  i, j: Integer;
  Offset: TOffset;
  IMTS: PIMTS;
  IMT: PIMT;
  Intf: PRTTIInterface;
begin
  if ClassInfo.IMTS = 0 then
    Exit;
  IMTS := GetIMGPtr(ClassInfo.IMTS);
  for i := 0 to FInterfaceCnt - 1 do
  begin
    Offset := IMTS[i];
    if Offset = 0 then
      continue;

    Intf := FInterfaces[i];
    IMT := GetIMGPtr(Offset);
    for j := 0 to Intf.MethodsCount - 1 do
      IMT[j] := TILProc(IMT[j]).Offset;
  end;
end;

function TVMTranslator.GetProcDump(const Proc: TILProc): string;
var
  M, MEnd: PNativeUInt;
begin
  Result := '';
  M := GetIMGPtr(Proc.Offset);
  MEnd := PNUInt(PByte(M) + Proc.CodeSize);
  while PByte(M) < PByte(MEnd) do begin
    Result := Result + IntToHex(M^, 8) + #10;
    Inc(M);
  end;
end;

procedure TVMTranslator.CorrectOffsets;
var
  Ui, i, j, Pos, VarSize: Integer;
  UN: TILUnit;
  Proc: TILProc;
  Variable: PILVariable;
  TypeInfo: PRTTIType;
  VMVar: PVMVariable;
  IMGPtr: Pointer;
begin
  CorrectRTTIOffsets;
  for Ui := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[Ui];
    // установка смещений для глобальных переменных
    Pos := MemAlign(IMG.Position, FDataAlign);
    IMGPtr := IMG.MemoryPosition;
    for i := 0 to Length(UN.Vars) - 1 do begin
      Variable := addr(UN.Vars[i]);
      VMVar := addr(UN.VMVars[i]);
      if IncludeRTTI then
        VMVar.DataType := PRTTIType(Variable.RTTI.DataType)
      else
        VMVar.DataType := nil;

      VMVar.TypeInfo := nil;
      VMVar.RefCount := -1;
      VMVar.WeekInfo := nil;
      VMVar.SyncInfo := nil;
      VMVar.FClassID := rttiVar;
      VMVar.Reference := Variable.RTTI.IsReference;

      if Assigned(Variable.AbsoluteTo) then
        VMVar.Addr := Pointer(Variable.AbsoluteTo.RTTI.Offset)
      else begin
        VMVar.Name := Variable.RTTI.Name;
        VMVar.Addr := Pointer(Pos);
        Variable.RTTI.Offset := Pos;
        //SetOffsetsElements(Variable);
        TypeInfo := GetTypeInfo(Variable.RTTI.DataType);

        if (Variable.RTTI.IsReference) or (TypeInfo.DataTypeID = dtClass) then
          VarSize := PTR_SIZE
        else
          VarSize := TypeInfo.DataSize;

        {инициализируем все глобальные переменные нулем}
        FillChar(IMGPtr^, VarSize, #0);

        {простановка значений по умолчанию глобальных переменных}
        if Assigned(Variable.DefaultValue) then
        begin
          Move(Variable.DefaultValue^, IMGPtr^, TypeInfo.DataSize);
          FreeMemory(Variable.DefaultValue);
          Variable.DefaultValue := nil;
        end;

        Inc(Pos, VarSize);
        Inc(PByte(IMGPtr), VarSize);
      end;
      Variable.RTTI.Offset := Toffset(VMVar.Addr);
    end;
    IMG.Position := Pos;

    // методы
    for i := 0 to Length(UN.Types) - 1 do begin
      for j := 0 to Length(UN.Types[i].Methods) - 1 do
      begin
        Proc := UN.Types[i].Methods[j];
        DoCorrectILOffsets(Proc);
      end;
      TypeInfo := GetTypeInfo(UN.Types[i].Offset);
      if TypeInfo.DataTypeID = dtClass then
      begin
        CorrectVMTOffsets(PRTTIClass(TypeInfo));
        CorrectIMTOffsets(PRTTIClass(TypeInfo));
      end;
    end;

    // замена указателей на переменные модуля и процедуры на смещения
    for i := 0 to Length(UN.Procs) - 1 do begin
      Proc := UN.Procs[i];
      DoCorrectILOffsets(Proc);
    end;

    // секция INITIALIZATION
    if Assigned(UN.InitProc.IL) then
      DoCorrectILOffsets(UN.InitProc);

    // секция FINALIZATION
    if Assigned(UN.FinalProc.IL) then
      DoCorrectILOffsets(UN.FinalProc);
  end;

  {генерируим список поиска процедур по адресам}
  MakeVMProcsSearchList;

  {если в образ не пишется RTTI, то подготавливаем сокращенную RTTI секцию}
  if not IncludeRTTI then
    WriteStrictRTTISection;
end;

procedure TVMTranslator.CorrectProcJMPOffsets(Proc: TILProc);
var
  i: Integer;
  IL: TILInstructions;
  ILInstr: ^TILInstruction;
  ILCode: TILCode;
  Addr: PNativeUInt;
begin
  IL := Proc.IL;
  for i := 0 to Length(IL) - 1 do
  begin
    ILInstr := @(IL[i]);
    ILCode := ILInstr.Code;
    if ILCode in [icJmp, icNearCall] then
    begin
      Addr := PNUInt(PByte(IMG.Memory) + ILInstr.Offset + SizeOf(NativeUint));
      if Addr^ = MaxUInt32 then
        Addr^ := IMG.Position
      else
        Addr^ := IL[Addr^].Offset;
    end;
  end;
end;

procedure TVMTranslator.CorrectRTTIOffsets;
var
  ui, ti, pi, i: Integer;
  ILUnit: TILUnit;
  ILType: PILType;
  ILProc: TILProc;
  RTTISize: UInt32;
  RttiList: TNativeUIntArray;
  Ptr: PNativeUInt;
  // Struct: PRTTIStruct;
begin
  RTTISize := FRTTI.Position;
  for ui := 0 to Length(FUnits) - 1 do
  begin
    ILUnit := FUnits[ui];
    // methods
    for ti := 0 to Length(ILUnit.Types) - 1 do
    begin
      ILType := addr(ILUnit.Types[ti]);
      if ILType.ID in [dtRecord, dtClass, dtInterface] then
      begin
        // todo
        // Struct := PRTTIStruct(GetTypeInfo(ILType.Offset));
        // if Struct.Methods > 0 then  //  Struct.Methods должен указывать на дин. массив!!!
        // Inc(Struct.Methods, RTTISize);
      end;
      for pi := 0 to Length(ILType.Methods) - 1 do
      begin
        ILProc := ILType.Methods[pi];
        Inc(ILProc.ProcInfo, RTTISize);
      end;
    end;
    // global procs
    for pi := 0 to Length(ILUnit.Procs) - 1 do
    begin
      ILProc := ILUnit.Procs[pi];
      Inc(ILProc.ProcInfo, RTTISize);
    end;
    // init
    ILProc := ILUnit.InitProc;
    if ILProc.ProcInfo > 0 then
      Inc(ILProc.ProcInfo, RTTISize);
    // final
    ILProc := ILUnit.FinalProc;
    if ILProc.ProcInfo > 0 then
      Inc(ILProc.ProcInfo, RTTISize);
    //
    RttiList := TNativeUIntArray(PByte(FRTTIProcs.Memory) + ILUnit.VMUnit.Procs);
    for i := 0 to Length(RttiList) - 1 do
    begin
      Ptr := addr(RttiList[i]);
      Ptr^ := Ptr^ + RTTISize;
    end;
    RttiList := TNativeUIntArray(PByte(FRTTIProcs.Memory) + ILUnit.VMUnit.Procs);
    inc(ILUnit.VMUnit.Procs, RTTISize);
  end;
end;

constructor TVMTranslator.Create;
begin
  inherited Create;
  FStrictRTTI := TILMemoryStream.Create;
  IncludeRTTI := True;
  FVMCodeLines := TVMCodeLines.Create;
  FFixTable := TList.Create;
end;

function TVMTranslator.GetMemberOffset(RecordTypeInfo: TOffset; MemberIndex: Integer): Int32;
var
  Struct: PRTTIStruct;
  Field: PILVariable;
begin
  Struct := PRTTIStruct(GetTypeInfo(RecordTypeInfo));
  Field := GetILField(Struct, MemberIndex);
  Result := Field.RTTI.Offset;
end;

function TVMTranslator.GetMemberTypeInfo(RecordTypeInfo: TOffset; MemberIndex: Integer; out FieldRTTI: PRTTIField): TOffset;
  function GetMember(TypeInfo: PRTTIRecord; out MemberTypeInfo: TOffset; out FieldRTTI: PRTTIField): Integer;
  var
    AInfo: PRTTIType;
    Fields: TRTTIFields;
    i: Integer;
  begin
    if TypeInfo.Ancestor <> 0 then
    begin
      AInfo := GetTypeInfo(TypeInfo.Ancestor);
      Result := GetMember(PRTTIRecord(AInfo), MemberTypeInfo, FieldRTTI);
      if Result = MemberIndex then
        Exit;
    end else
      Result := -1;

    Fields := TRTTIFields(GetTypeInfo(TypeInfo.Fields));
    for i := 0 to TypeInfo.FieldsCount - 1 do
    begin
      Inc(Result);
      if Result = MemberIndex then
      begin
        FieldRTTI := addr(Fields[i]);
        MemberTypeInfo := FieldRTTI.DataType;
        Exit;
      end;
    end;
    MemberTypeInfo := 0;
  end;
var
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(RecordTypeInfo);
  GetMember(PRTTIRecord(TypeInfo), Result, FieldRTTI);
end;

procedure TVMTranslator.WriteLDInstruction(CND: TILCondition; Variable: PILVariable; Dst: TVMRegister);
var
  MCode: TVMCode;
begin
  case Variable.VarScope of
    ARG_SCOPE_LOCAL: MCode := GetLDInstruction(Variable.RTTI.DataType, atLocal);
    ARG_SCOPE_GLOBAL: MCode := GetLDInstruction(Variable.RTTI.DataType, atGlobal);
  else
    raise Exception.Create('not supported');
  end;
  WriteInstruction(CND, MCode, Dst, VM_R0, ArgGet(Variable));
end;

procedure TVMTranslator.WriteLDConstant(CND: TILCondition; const Data: NativeUInt; Dst: TVMRegister);
begin
  WriteInstruction(CND, LD_C_NATIVE, Dst, VM_R0, Data);
end;

procedure TVMTranslator.WriteLDConstant64(CND: TILCondition; const Data: Int64; Dst: TVMRegister);
begin
  {$IF PTR_SIZE = 4}
  IMG.WriteNativeUInt(MakeInstruction(LD_C_I64, Dst, VM_R0, VM_R0, CND, 2));
  {$ELSE}
  IMG.WriteNativeUInt(MakeInstruction(LD_C_I64, Dst, VM_R0, VM_R0, CND, 1));
  {$ENDIF}
  IMG.WriteInt64(Data);
end;

procedure TVMTranslator.WriteLDInstruction(const Context: TILTContext; const ARG: TILArgument; Dst: TVMRegister);
var
  MCode: TVMCode;
begin
  // если источник - поле стуктуры этого метода,- загружаем self в R0
  if ARG.ArgumentType = atField then
    WriteLDSelfInstruction(Context, Context.Proc.SelfArg, Dst);

  if (ARG.ArgScope = ARG_SCOPE_LOCAL) and (ARG.ArgClass = ARG_VAR) and (ARG.AsVariable.VarClass = VarConst) then
    MCode := LD_L_NATIVE
  else
    MCode := GetLDInstruction(ARG);

  WriteInstruction(Context.Cond, MCode, Dst, Dst, ARG);
end;

procedure TVMTranslator.WriteLDSelfInstruction(const Context: TILTContext; const ARG: TILArgument; Dst: TVMRegister);
begin
  WriteInstruction(Context.Cond, LD_L_NATIVE, Dst, ARG);
end;

function TVMTranslator.GetDimOffset(ArrayTypeInfo: TOffset; DimNumber: UInt32): Integer;
var
  i: Integer;
  r: PRTTIOrdinal;
  AInfo: PRTTIArray;
  ElTypeInfo: PRTTIType;
  Dimensions: PRTTIDimensions;
begin
  Result := 1;
  AInfo := PRTTIArray(GetTypeInfo(ArrayTypeInfo));
  for i := AInfo.DimensionsCount - 1 downto DimNumber + 1 do begin
    Dimensions := PRTTIDimensions(GetTypeInfo(AInfo.Dimensions));
    r := PRTTIOrdinal(GetTypeInfo(Dimensions[i]));
    Result := Result * (r.HiBound - r.LoBound + 1)
  end;
  ElTypeInfo := GetTypeInfo(AInfo.ElementTypeInfoOffset);
  Result := Result * GetResultParamDataSize(ElTypeInfo);
end;

function TVMTranslator.GetResultParamDataSize(TypeInfo: PRTTIType): Integer;
begin
  if TypeInfo.DataTypeID = dtClass then
    Result := PTR_SIZE
  else
    Result := TypeInfo.DataSize;
end;

function TVMTranslator.GetFullImportName(TypeInfo: PRTTIType): string;
var
  LibName, DeclName: string;
begin
  if TypeInfo.ImportLib = 0 then
    Exit('');
  LibName := GetString(TypeInfo.ImportLib);
  DeclName := GetString(TypeInfo.ImportName);
  Result := LibName + '.' + DeclName;
end;

procedure TVMTranslator.Translate_LDMETHOD(const Ctx: TILTContext; var Args: TIL_LDMETHOD_ARGS);
begin
  Translate_LDMETHODPTR_Instruction(Ctx, Args.D, Args.S.Next);
end;

procedure TVMTranslator.Translate_LDSMETHOD(const Ctx: TILTContext; var Args: TIL_LDSMETHOD_ARGS);
begin
  Translate_LDMETHODPTR_Instruction(Ctx, Args.D, Args.S);
end;

procedure TVMTranslator.Translate_LDMETHODPTR_Instruction(const Ctx: TILTContext; const Dst, Method: TILArgument);
var
  Proc: TILProc;
  //Struct: PRTTIStruct;
begin
  WriteInstruction(Ctx.Cond, GetLDPTRInstruction(Dst), VM_R0, VM_R0, Dst);
  WriteLDInstruction(Ctx, Method.AsMethod.Self, VM_R1);
  Proc := Method.AsMethod.Proc;
  if (ILPROC_VIRTUAL and Proc.Flags) <> 0 then
  begin
    WriteInstruction(Ctx.Cond, VIRT_METHOD_PTR, VM_R2, VM_R1, Proc.VirtualIndex);
  end else begin
    //Struct := PRTTIStruct(GetTypeInfo(Proc.RTTI.StructInfo));
    WriteInstruction(Ctx.Cond, LD_G_PROC, VM_R2, VM_R0, NativeUInt(Method.AsMethod.Proc));
  end;
  WriteInstruction(Ctx.Cond, VM_SET_METHOD_PTR, VM_R0, VM_R1, VM_R2);
end;

procedure TVMTranslator.Translate_LDPTR_Instruction(const Ctx: TILTContext; var Dst: TILArgument; const Src: TILArgument);
// процедура генерирует код получения ссылки на элемент, заданный зепочкой
// [Item1].[Item2].[ItemN]...
var
  DimIndex: UInt32;
  MBOffset: Int32;
  BaseInfoOffset: TOffset;
  BaseInfo: PRTTIType;
  AInfo: PRTTIArray;
  Arg: TILArgument;
  AccessCnt: Integer;
  VMCode: TVMCode;
  Field: PRTTIField;
begin
  // загружаем адрес базы в R0
  VMCode := GetLDPTRInstruction(Src);
  WriteInstruction(Ctx.Cond, VMCode, VM_R0, VM_R0, Src);
  BaseInfoOffset := Src.TypeInfo;
  DimIndex := 0;
  MBOffset := 0;
  AccessCnt := 0;
  Arg := Src.Next;
  Field := nil;
  while Assigned(Arg) do begin
    BaseInfo := GetTypeInfo(BaseInfoOffset);
    Inc(AccessCnt);
    case BaseInfo.DataTypeID of
      dtPointer: begin
        BaseInfoOffset := PRTTIPointer(BaseInfo).RefTypeInfoOffset;
        Field := nil;
        Continue;
      end;
      dtStaticArray, dtString, dtAnsiString, dtDynArray, dtOpenArray: begin

        if Assigned(Field) and (BaseInfo.DataTypeID in [dtString, dtAnsiString, dtDynArray, dtClass]) then
        begin
          WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, VM_R0);
        end;

        // оптимизация: если константное смещение = 0 то операция вычисления смещения не нужна
        if (Arg.ArgumentType <> atImmConst) or ((Arg.ArgumentType = atImmConst) and (Arg.I32 <> 0)) then
        begin
          WriteLDInstruction(Ctx, Arg, VM_R1); // индекс в R1
          // третий аргумент FMA_NATIVE - размер в байтах измерения массива
          // после этой инструкции в R0 - адрес искомой ячайки
          WriteInstruction(Ctx.Cond, FMA_NATIVE, VM_R0, VM_R0, VM_R1, GetDimOffset(BaseInfoOffset, DimIndex));
        end;
        Inc(AccessCnt);
        Inc(DimIndex);
        // Если массив закончился, выбираем следующий TypeInfo, иначе работаем со следующим индексом
        AInfo := PRTTIArray(GetTypeInfo(BaseInfoOffset));
        if DimIndex >= AInfo.DimensionsCount then
        begin
          BaseInfoOffset := AInfo.ElementTypeInfoOffset;
          DimIndex := 0;
        end;

        // если это не последний элемент в цепочке
        if Assigned(Arg.Next) then begin
          BaseInfo := GetTypeInfo(BaseInfoOffset);
          if BaseInfo.DataTypeID <> dtRecord then
          begin
            // если смещение = 0 - инструкцию сложениея не пишем
            if MBOffset <> 0 then
              WriteInstruction(Ctx.Cond, ADD_NUINT_C, MBOffset);
            MBOffset := 0;
            // если поле структуры оказался указателем
            if (BaseInfo.DataTypeID = dtPointer) or Assigned(Field) then
            begin
              BaseInfoOffset := PRTTIPointer(BaseInfo).RefTypeInfoOffset;
              WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, VM_R0);
            end else
            // если поле структуры оказался классом
            if BaseInfo.DataTypeID = dtClass then
              WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, VM_R0);
          end;
        end;
        Field := nil;
      end;

      dtRecord, dtGuid, dtClass: begin
        MBOffset := MBOffset + GetMemberOffset(BaseInfoOffset, Arg.I32);
        // выбираем следующий TypeInfo
        BaseInfoOffset := GetMemberTypeInfo(BaseInfoOffset, Arg.I32, Field);
        if Assigned(Arg.Next) then begin
          // если это не последний элемент в цепочке
          BaseInfo := GetTypeInfo(BaseInfoOffset);
          if BaseInfo.DataTypeID <> dtRecord then
          begin
            // если смещение = 0 - инструкцию сложениея не пишем
            if MBOffset <> 0 then
              WriteInstruction(Ctx.Cond, ADD_NUINT_C, MBOffset);
            MBOffset := 0;
            // если поле структуры оказался указателем
            if BaseInfo.DataTypeID = dtPointer then
            begin
              BaseInfoOffset := PRTTIPointer(BaseInfo).RefTypeInfoOffset;
              WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, VM_R0);
            end else
            // если поле структуры оказался классом
            if BaseInfo.DataTypeID = dtClass then
              WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, VM_R0);
          end;
        end else begin
          // если смещение = 0 - инструкцию сложениея не пишем
          if MBOffset <> 0 then // оптимизация
            WriteInstruction(Ctx.Cond, ADD_NUINT_C, NativeUInt(MBOffset));
        end;
      end;
    else
      AbortWork('[Unit: "%s"][Proc: "%s"]: Type "%s" is not supported',
                [Ctx.PUnit.Name, GetProcName(Ctx.Proc), GetDataTypeName(GetTypeInfo(BaseInfoOffset).DataTypeID)]);
    end;
    Arg := Arg.Next;
  end;

  // сохраняем вычесленный адрес во временную переменную Dst
  if (AccessCnt > 0) or (MBOffset <> 0) then
    WriteInstruction(Ctx.Cond, ST_L_NATIVE, Dst)
  else begin
    {оптимизация, удаляем ненужные пересылки}
    //FIMG.Position := FIMG.Position - SizeOF(NativeInt)*2;
    //Dst := Src;
  end;
  Dst.TypeInfo := BaseInfoOffset;
end;

procedure TVMTranslator.Translate_MOVEZERO(const Context: TILTContext; var Args: TIL_MOVEZERO_ARGS);
var
  DTypeInfo: PRTTIType;
  VMCode: TVMCode;
begin
  with Args do
  begin
    DTypeInfo := GetTypeInfo(D.TypeInfo);
    VMCode := _ClearInstructions[DTypeInfo.DataTypeID, D.ArgumentType];
    if VMCode <> VM_NOPE then
      WriteInstruction(Context.Cond, VMCode, ArgGet(D.AsVariable))
    else begin
      WriteInstruction(Context.Cond, LD_C_ZERO);
      WriteSTInstruction(Context, D);
    end;
  end;
end;

procedure TVMTranslator.Translate_GETSPTR(const Ctx: TILTContext; var Args: TIL_GETSPTR_ARGS);
begin
  if Args.S.ArgClass = ARG_METHOD then
    Translate_LDMETHODPTR_Instruction(Ctx, Args.D, Args.S)
  else
    Translate_LDPTR_Instruction(Ctx, Args.D, Args.B);
end;

procedure TVMTranslator.Translate_GETPTR(const Ctx: TILTContext; var Args: TIL_GETPTR_ARGS);
begin
  if Assigned(Args.S.Next) and (Args.S.Next.ArgClass = ARG_METHOD) then
    Translate_LDMETHODPTR_Instruction(Ctx, Args.D, Args.S.Next)
  else
    Translate_LDPTR_Instruction(Ctx, Args.D, Args.S);
end;

procedure TVMTranslator.Translate_LEA(const Ctx: TILTContext; var Args: TIL_LEA_Args);
var
  LDCode: TVMCode;
begin
  with Args do begin
    case B.ArgumentType of
      atLocal: LDCode:= LD_L_PTR;
      atReference: LDCode := LD_L_PTR;
      atGlobal: LDCode := LD_G_PTR;
      atImmConst: LDCode := LD_G_PTR;
    else
      LDCode := VM_NOPE;
    end;
    WriteInstruction(Ctx.Cond, LDCode, B);
    if Ctx.ILCode = icLea then begin
      // смещение
      if Offset.ArgumentType = atImmConst then
        if Offset.NUInt <> 0 then // оптимизация - нулевое смещение не считаем
          WriteInstruction(Ctx.Cond, ADD_NUINT_C, Offset)
      else begin
        WriteLDInstruction(Ctx, Offset, VM_R1);
        WriteInstruction(Ctx.Cond, ADD_NATIVE);
      end;
    end;
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.WriteSTInstruction(const Context: TILTContext; const ARG: TILArgument; Src: TVMRegister = VM_R0);
var
  VMCode: TVMCode;
  DstReg: TVMRegister;
begin
  // если приемник - поле стуктуры этого метода,- загружаем self в свободный регистр
  if ARG.ArgumentType = atField then
  begin
    DstReg := VM_R15; // tmp !!!
    WriteLDInstruction(Context, Context.Proc.SelfArg, DstReg);
  end else
    DstReg := Src;

  VMCode := GetSTInstruction(ARG);
  WriteInstruction(Context.Cond, VMCode, DstReg, Src, ARG);
end;

procedure TVMTranslator.WriteSTInstruction(const Context: TILTContext; OverrideCND: TILCondition; const ARG: TILArgument;
  Src: TVMRegister);
var
  VMCode: TVMCode;
  DstReg: TVMRegister;
begin
  // если приемник - поле стуктуры этого метода,- загружаем self в свободный регистр
  if ARG.ArgumentType = atField then
  begin
    DstReg := VM_R15; // tmp !!!
    WriteLDInstruction(Context, Context.Proc.SelfArg, DstReg);
  end else
    DstReg := Src;

  VMCode := GetSTInstruction(ARG);
  WriteInstruction(OverrideCND, VMCode, DstReg, Src, ARG);
end;

procedure TVMTranslator.Translate_MEMSET(const Ctx: TILTContext; var Args: TIL_MEMSET_ARGS);
var
  DTypeInfo, ElTypeInfo: PRTTIType;
begin
  with Args do begin
    DTypeInfo := GetTypeInfo(Dst);
    WriteInstruction(Ctx.Cond, GetLDPTRInstruction(Dst), Dst);
    if DTypeInfo.DataTypeID in [dtString, dtAnsiString, dtDynArray] then
    begin
      ElTypeInfo := GetTypeInfo(PRTTIArray(DTypeInfo).ElementTypeInfoOffset);
      WriteInstruction(Ctx.Cond, ARRAY_LENGTH, VM_R1, VM_R0);
      WriteInstruction(Ctx.Cond, MUL_C32, VM_R1, VM_R1, ElTypeInfo.DataSize);
    end else begin
      WriteLDConstant(Ctx.Cond, DTypeInfo.DataSize, VM_R1);
    end;
    WriteInstruction(Ctx.Cond, MEM_SET, VM_R0, VM_R1, Bpt.U32);
  end;
end;

procedure TVMTranslator.Translate_MOVE(const Ctx: TILTContext; var Args: TIL_MOVE_ARGS);
var
  DTypeInfo, STypeInfo: PRTTIType;
  ILData: Integer;
  VMCode: TVMCode;
begin
  if Assigned(Args.D.Next) then
    CheckListArg(Ctx, Args.D);

  if Assigned(Args.S.Next) then
    CheckListArg(Ctx, Args.S);

  with Args do
  begin
    DTypeInfo := GetTypeInfo(D.TypeInfo);
    STypeInfo := GetTypeInfo(S.TypeInfo);

    // если источник - процедура
    if (S.ArgClass = ARG_PROC) then
    begin
      WriteInstruction(Ctx.Cond, LD_G_PROC, VM_R0, S);
      WriteSTInstruction(Ctx, D);
      Exit;
    end;

    ILData := 0;
    VMCode := VM_NOPE;
    if (DTypeInfo.DataTypeID = dtVariant) and (STypeInfo.DataTypeID <> dtVariant) then
    begin
      VMCode := CNV_VALUE_TO_VAR;
      ILData := Integer(STypeInfo.DataTypeID);
    end else
    if (STypeInfo.DataTypeID = dtVariant) and (DTypeInfo.DataTypeID <> dtVariant) then
    begin
      VMCode := CNV_VAR_TO_VALUE;
      ILData := Integer(DTypeInfo.DataTypeID);
    end;
    if ILData <> 0 then
    begin
      WriteLDInstruction(Ctx, D, VM_R0);
      WriteLDInstruction(Ctx, S, VM_R1);
      WriteInstruction(Ctx.Cond, VMCode, VM_R0, VM_R1, ILData);
      if VMCode = CNV_VAR_TO_VALUE then
        WriteSTInstruction(Ctx, D);
      Exit;
    end;

    if (DTypeInfo.DataTypeID = dtProcType) and (DTypeInfo.DataSize = PTR_SIZE*2) then
    begin
      WriteLDInstruction(Ctx, D, VM_R0);
      WriteLDInstruction(Ctx, S, VM_R1);
      WriteInstruction(Ctx.Cond, MOVE_MEM_C, VM_R0, VM_R1, PTR_SIZE*2);
    end else
    if {(DTypeInfo.DataTypeID <> dtRecord) and }(DTypeInfo.DataSize in [1, 2, 4, 8]) or (DTypeInfo.DataTypeID = dtClass)  then
    begin
      if CHECK_ST_C_I32_APPLY(S, D) then // оптимизация команд
        WriteInstruction(Ctx.Cond, ST_C_I32, D.AsVariable.RTTI.Offset, S.NUInt)
      else
      if CHECK_MOVE_L_I32_APPLY(S, D) then // оптимизация команд
         WriteInstruction(Ctx.Cond, MOVE_L_I32, D.GetVarOffset, S.GetVarOffset)
      else
      begin
        WriteLDInstruction(Ctx, S);
        CheckConvert(Ctx, VM_R0, D.TypeInfo, S.TypeInfo);
        WriteSTInstruction(Ctx, D);
      end;
    end else begin
      WriteLDInstruction(Ctx, D, VM_R0);
      WriteLDInstruction(Ctx, S, VM_R1);
      if (S.ArgumentType = atImmConst) and (DTypeInfo.DataSize <= 8) then
        WriteInstruction(Ctx.Cond, MOVE_REG_TO_MEM, VM_R0, VM_R1, DTypeInfo.DataSize)
      else
        WriteInstruction(Ctx.Cond, MOVE_MEM_C, VM_R0, VM_R1, DTypeInfo.DataSize);
    end;
  end;
end;

procedure TVMTranslator.Translate_TEST(const Ctx: TILTContext; var Args: TIL_TEST_ARGS);
var
  MCode: TVMCode;
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, L, VM_R0);
    WriteLDInstruction(Ctx, R, VM_R1);
    MCode := GetInstruction(icTest, L.TypeInfo);
    CheckConvert(Ctx, VM_R1, L.TypeInfo, R.TypeInfo);
    WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R0, VM_R1);
  end;
end;

function TVMTranslator.GetMaxTypeInfo(const L, R: TILArgument): TOffset;
var
  LTI, RTI: PRTTIType;
  LT, RT: TOffset;
begin
  LT := L.TypeInfo;
  RT := R.TypeInfo;

  LTI := GetTypeInfo(LT);
  RTI := GetTypeInfo(RT);

  if LTI.DataSize > RTI.DataSize then
    Result := LT
  else
    Result := RT;
end;

procedure TVMTranslator.Translate_2OperandOpCode(const Ctx: TILTContext; var Args: TIL_DS_Args);
var
  MCode: TVMCode;
  CommonType: TOffset;
begin
  with Args do begin
    WriteLDInstruction(Ctx, D, VM_R1);

    CommonType := GetMaxTypeInfo(D, S);
    MCode := GetInstruction(Ctx.ILCode, CommonType);

    {оптимизация, если "источник 2" константа, заменяем инструкцию}
    if (S.ArgumentType = atImmConst) and (MCode in [ADD_I32, SUB_I32]) then
    begin
      case MCode of
        ADD_I32: MCode := ADD_I32_C;
        SUB_I32: MCode := SUB_I32_C;
      end;
      WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R1, S.NUInt);
    end else begin
      WriteLDInstruction(Ctx, S, VM_R2);
      CheckConvert(Ctx, VM_R2, D.TypeInfo, S.TypeInfo);
      WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R1, VM_R2);
    end;
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_3OperandOpCode(const Ctx: TILTContext; var Args: TIL_DSS_Args);
var
  MCode: TVMCode;
  CommonType: TOffset;
  RTypeInfo: PRTTIType;
begin
  with Args do begin
    CommonType := GetMaxTypeInfo(L, R);
    MCode := GetInstruction(Ctx.ILCode, CommonType);
    {оптимизация команд: начало}
    if D.IsLocalVar and L.IsLocalVar and (MCode = ADD_I32) then
    begin
      RTypeInfo := GetTypeInfo(R);
      if R.IsLocalVar and (RTypeInfo.DataSize = 4) then begin
        WriteInstruction(Ctx.Cond, ADD_L_I32, L.GetVarOffset, R.GetVarOffset);
        WriteInstruction(Ctx.Cond, GetSTInstruction(D), D);
        Exit;
      end else
      if R.ArgumentType = atImmConst then begin
        WriteInstruction(Ctx.Cond, ADD_L_I32_C, L.GetVarOffset, R.NUInt);
        WriteInstruction(Ctx.Cond, GetSTInstruction(D), D);
        Exit;
      end;
    end;
    {оптимизация команд: конец}
    WriteLDInstruction(Ctx, L, VM_R1);
    CheckConvert(Ctx, VM_R1, D.TypeInfo, L.TypeInfo);
    if (R.ArgumentType = atImmConst) and (MCode in [ADD_I32, SUB_I32]) then
    begin
      case MCode of
        ADD_I32: MCode := ADD_I32_C;
        SUB_I32: MCode := SUB_I32_C;
      end;
      WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R1, R.NUInt);
    end else begin
      WriteLDInstruction(Ctx, R, VM_R2);
      CheckConvert(Ctx, VM_R2, D.TypeInfo, R.TypeInfo);
      WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R1, VM_R2);
    end;
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_ARRAYCOPY(const Ctx: TILTContext; var Args: TIL_ARRAYCOPY_ARGS);
var
  Arr: PRTTIArray;
  ETypeInfo: PRTTIType;
  EDataSize: UInt32;
begin
  with Args do
  begin
    Arr := PRTTIArray(GetTypeInfo(D.TypeInfo));

    ETypeInfo := GetTypeInfo(Arr.ElementTypeInfoOffset);
    EDataSize := ETypeInfo.DataSize;

    WriteLDInstruction(Ctx, D, VM_R0); // содержит адрес Dst
    WriteLDInstruction(Ctx, S, VM_R1); // содержит адрес Src

    // модифицируем указатель Src на значение From
    if F.ArgumentType = atImmConst then
    begin
      // если From - константа
      if F.U32 > 0 then
        WriteInstruction(cNone, ADD_I32_C, VM_R1, VM_R1, F.U32*EDataSize);
    end else begin
      // если From - переменная
      WriteLDInstruction(Ctx, F, VM_R2);
      WriteInstruction(cNone, FMA_NATIVE, VM_R1, VM_R1, VM_R2, EDataSize);
    end;

    if L.ArgumentType = atImmConst then
      // если Length - константа
      WriteInstruction(cNone, MOVE_MEM_C, VM_R0, VM_R1, L.U32*EDataSize)
    else begin
      // если Length - константа
      WriteLDInstruction(Ctx, L, VM_R2);
      WriteInstruction(cNone, MUL_C32, VM_R2, VM_R2, EDataSize);
      WriteInstruction(cNone, MOVE_MEM, VM_R0, VM_R1, VM_R2);
    end;
  end;
end;

procedure TVMTranslator.Translate_ARRAYMOVE(const Ctx: TILTContext; var Args: TIL_ARRAYMOVE_ARGS);
var
  ARTTI: PRTTIArray;
  ElTypeInfo: PRTTIType;
begin
  with Args do
  begin
    ARTTI := ArrayRTTI(SrcArr.TypeInfo);
    ElTypeInfo := GetTypeInfo(ARTTI.ElementTypeInfoOffset);

    // источник
    WriteLDInstruction(Ctx, SrcArr, VM_R0);
    if SrcIdx.ArgumentType <> atImmConst then
    begin
      WriteLDInstruction(Ctx, SrcIdx, VM_R1);
      WriteInstruction(cNone, ADD_NATIVE, VM_R0, VM_R0, VM_R1);
    end else
      if SrcIdx.NInt > 0 then
        WriteInstruction(cNone, ADD_NUINT_C, VM_R0, VM_R0, VM_R0, SrcIdx.NInt);

    // приемник
    WriteLDInstruction(Ctx, DstArr, VM_R1);
    if DstIdx.ArgumentType <> atImmConst then
    begin
      WriteLDInstruction(Ctx, DstIdx, VM_R2);
      WriteInstruction(cNone, ADD_NATIVE, VM_R1, VM_R1, VM_R2);
    end else
      if DstIdx.NInt > 0 then
        WriteInstruction(cNone, ADD_NUINT_C, VM_R1, VM_R1, VM_R1, DstIdx.NInt);

    if Cnt.ArgumentType = atImmConst then
      WriteInstruction(cNone, MOVE_MEM_C, VM_R1, VM_R0, VM_R0, Cnt.NUInt*NativeUInt(ElTypeInfo.DataSize))
    else begin
      WriteLDInstruction(Ctx, Cnt, VM_R2);
      if ElTypeInfo.DataSize > 1 then
        WriteInstruction(cNone, MUL_C32, VM_R2, VM_R2, VM_R0, ElTypeInfo.DataSize);
      WriteInstruction(cNone, MOVE_MEM_C, VM_R1, VM_R0, VM_R2);
    end;
  end;
end;

procedure TVMTranslator.Translate_ARRAYLENGTH(const Ctx: TILTContext; var Args: TIL_ARRAYLENGTH_ARGS);
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, S);
    WriteInstruction(Ctx.Cond, GetInstruction(icArrayLength, S.TypeInfo));
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_ARRDALLOC(const Ctx: TILTContext; var Args: TIL_ARRDALLOC_ARGS);
const
  STR_REC_PADDING = {$IFDEF CPUX64} 4 {$ELSE} 0 {$ENDIF};
var
  Size: NativeUInt;
  ArrayTypeInfo: PRTTIArray;
  ArrayElementTypeInfo: PRTTIType;
  FinalProc: TILProc;
begin
  with Args do
  begin
    ArrayTypeInfo := PRTTIArray(GetTypeInfo(D.TypeInfo));
    ArrayElementTypeInfo := GetTypeInfo(ArrayTypeInfo.ElementTypeInfoOffset);

    // динамический массив
    if ArrayTypeInfo.DataTypeID = dtDynArray then
    begin
      if S.ArgumentType = atImmConst then
      begin
        Size := STR_REC_PADDING + 4 {refcnt} + PTR_SIZE {length} + S.NUInt * NativeUInt(ArrayElementTypeInfo.DataSize);
        WriteInstruction(Ctx.Cond, MEM_ALLOC, Size);
        WriteInstruction(Ctx.Cond, ARRAY_INIT, S.NUInt);
      end else begin
        WriteLDInstruction(Ctx, S, VM_R1);
        WriteInstruction(Ctx.Cond, ARRAY_MEM_ALLOC, VM_R0, VM_R1, ArrayElementTypeInfo.DataSize);
      end;
      if Ctx.ILCode = icArrayRAlloc then begin
        WriteLDInstruction(Ctx, D, VM_R1);
        WriteInstruction(Ctx.Cond, MOVE_ARRAY, VM_R0, VM_R1, ArrayElementTypeInfo.DataSize);
        if PRTTIDynArray(ArrayTypeInfo).FinalProc > 0 then
        begin
          FinalProc := FUnits[PRTTIDynArray(ArrayTypeInfo).FinalUIdx].Procs[PRTTIDynArray(ArrayTypeInfo).FinalProc];
          WriteInstruction(Ctx.Cond, ARRAY_DECREF, VM_R1, VM_R1, ArgGet(FinalProc));
        end else
          WriteInstruction(Ctx.Cond, ARRAY_DECREF, VM_R1, VM_R1);
      end;
    end else
    // строка
    begin
      WriteLDInstruction(Ctx, S, VM_R1);
      if ArrayTypeInfo.DataTypeID = dtString then
        WriteInstruction(Ctx.Cond, STRU_CREATE, VM_R0, VM_R1)
      else
        WriteInstruction(Ctx.Cond, STRA_CREATE, VM_R0, VM_R1);
      if Ctx.ILCode = icArrayRAlloc then begin
        WriteLDInstruction(Ctx, D, VM_R1);
        WriteInstruction(Ctx.Cond, STR_MOVE, VM_R0, VM_R1, ArrayElementTypeInfo.DataSize);
        WriteInstruction(Ctx.Cond, STR_DECREF, VM_R1, VM_R1);
      end;
    end;
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_CMP(const Ctx: TILTContext; var Args: TIL_CMP_ARGS);
var
  MCode: TVMCode;
  LTI, RTI: PRTTIType;
begin
  with Args do
  begin
    {оптимизация команд: начало}
    if CHECK_CMP_L_I32_APPLY(L, R) then
    begin
      WriteInstruction(Ctx.Cond, CMP_L_I32, L.GetVarOffset, R.GetVarOffset);
      Exit;
    end;
    if CHECK_CMP_L_C32_APPLY(L, R) then
    begin
      WriteInstruction(Ctx.Cond, CMP_L_C32, L.GetVarOffset, R.NUInt);
      Exit;
    end;
    {оптимизация команд: конец}

    LTI := GetTypeInfo(L.TypeInfo);
    RTI := GetTypeInfo(R.TypeInfo);

    if (LTI.DataTypeID in [dtRecord, dtGuid, dtStaticArray, dtSet]) {and (not (LTI.DataSize in [1, 2, 4, 8])))} or
       (RTI.DataTypeID in [dtRecord, dtGuid, dtStaticArray, dtSet]) {and (not (RTI.DataSize in [1, 2, 4, 8])))} then
    begin
      // WriteLDInstruction(Context.Condition, L, VM_R0);
      WriteInstruction(Ctx.Cond, GetLDPTRInstruction(L), VM_R0, VM_R0, L);
      WriteLDInstruction(Ctx, R, VM_R1);

  //                                             все не правильно!!!!!!!!!!!!!!!!!!!!
  //    WriteInstruction(Context.Condition, GetLDPTRInstruction(L), VM_R0, VM_R0, L);
  //    WriteInstruction(Context.Condition, GetLDPTRInstruction(R), VM_R1, VM_R1, R);
      if LTI.DataSize in [1, 2, 4, 8] then
        WriteInstruction(Ctx.Cond, CMP_MEM_VS_REG, VM_R0, VM_R1, Min(LTI.DataSize, RTI.DataSize))
      else
        WriteInstruction(Ctx.Cond, CMP_MEM, VM_R0, VM_R1, Min(LTI.DataSize, RTI.DataSize));
      Exit;
    end;

    WriteLDInstruction(Ctx, L, VM_R0);
    MCode := GetInstruction(icCmp, L.TypeInfo {!!!!!});

    if (R.ArgumentType = atImmConst) and (MCode = CMP_I32) then
    begin
      WriteInstruction(Ctx.Cond, CMP_I32_C, R.NUInt);
    end else begin
      WriteLDInstruction(Ctx, R, VM_R1);
      CheckConvert(Ctx, VM_R1, L.TypeInfo, R.TypeInfo);
      WriteInstruction(Ctx.Cond, MCode, VM_R0, VM_R1);
    end;
  end;
end;

procedure TVMTranslator.Translate_CMPJ(const Ctx: TILTContext; var Args: TIL_CMPJ_ARGS);
begin
  //  todo
end;

procedure TVMTranslator.Translate_CONVERT(const Ctx: TILTContext; var Args: TIL_CONVERT_ARGS);
var
  DTypeInfo: PRTTIType;
  STypeInfo: PRTTIType;
  DstTID, SrcTID: TDataTypeID;
begin
  with Args do
  begin
    DTypeInfo := GetTypeInfo(D);
    STypeInfo := GetTypeInfo(S);
    DstTID := DTypeInfo.DataTypeID;
    SrcTID := STypeInfo.DataTypeID;

    if (DstTID = dtString) and
       (SrcTID = dtAnsiString) then
    begin
      WriteLDInstruction(Ctx, S, VM_R0);
      WriteInstruction(cNone, CNV_ASTR_TO_USTR, VM_R0, VM_R0);
      WriteSTInstruction(Ctx, D, VM_R0);
    end else
    if (DstTID = dtAnsiString) and
       (SrcTID = dtString) then
    begin
      WriteLDInstruction(Ctx, S, VM_R0);
      WriteInstruction(cNone, CNV_USTR_TO_ASTR, VM_R0, VM_R0);
      WriteSTInstruction(Ctx, D, VM_R0);
    end else
    if (DstTID = dtString) and
       (SrcTID = dtChar) then
    begin
      WriteLDInstruction(Ctx, S, VM_R0);
      WriteInstruction(cNone, CNV_UCHR_TO_USTR, VM_R0, VM_R0);
      WriteSTInstruction(Ctx, D, VM_R0);
    end else
    if (DstTID = dtAnsiString) and
       (SrcTID = dtAnsiChar) then
    begin
      WriteLDInstruction(Ctx, S, VM_R0);
      WriteInstruction(cNone, CNV_ACHR_TO_ASTR, VM_R0, VM_R0);
      WriteSTInstruction(Ctx, D, VM_R0);
    end else
    if (DstTID = dtVariant) then
    begin
      WriteLDInstruction(Ctx, D, VM_R0);
      WriteLDInstruction(Ctx, S, VM_R1);
      WriteInstruction(cNone, CNV_VALUE_TO_VAR, VM_R0, VM_R1, Ord(SrcTID));
    end else
    if (SrcTID = dtVariant) then begin
      WriteLDInstruction(Ctx, D, VM_R0);
      WriteLDInstruction(Ctx, S, VM_R1);
      WriteInstruction(cNone, CNV_VAR_TO_VALUE, VM_R0, VM_R1, Ord(DstTID));
      WriteSTInstruction(Ctx, D, VM_R0);
    end else
      AbortWork('unsupported datatypes');
  end;
end;

procedure TVMTranslator.Translate_INC(const Ctx: TILTContext; var Args: TIL_INC_ARGS);
var
  DTID: TDataTypeID;
  Code: TVMCode;
begin
  with Args do
  begin
    DTID := GetTypeInfo(D.TypeInfo).DataTypeID;
    {оптимизация команд: начало}
    if (Ctx.ILCode = icInc) and D.IsLocalVar and (DTID <= dtInt32) then
    begin
      WriteInstruction(Ctx.Cond, INC_L_I32, D.GetVarOffset);
      Exit;
    end;
    {оптимизация команд: конец}
    WriteLDInstruction(Ctx, D);
    case Ctx.ILCode of
      icInc: case DTID of
               dtEnum: Code := ADD_I32_C; // tmp
               dtInt8, dtInt16, dtInt32: Code := ADD_I32_C;
               dtUInt8, dtUInt16, dtUInt32 : Code := ADD_I32_C;
               dtFloat32, dtFloat64: Code := ADD_F64_CI32;
               dtPointer: begin
                 WriteInstruction(cNone, LD_C_I32, VM_R1, 1);
                 WriteInstruction(cNone, ADD_NATIVE, VM_R0, VM_R0, VM_R1);
                 WriteSTInstruction(Ctx, D);
                 Exit;
               end;
               dtInt64, dtUInt64: begin
                 WriteInstruction(cNone, LD_C_I32, VM_R1, 1);
                 WriteInstruction(cNone, ADD_I64, VM_R0, VM_R0, VM_R1);
                 WriteSTInstruction(Ctx, D);
                 Exit;
               end;
             else
               Code := VM_NOPE;
             end;
      icDec: case DTID of
               dtEnum: Code := SUB_I32_C; // tmp
               dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32: Code := SUB_I32_C;
               dtFloat32, dtFloat64: Code := SUB_F64_CI32;
               dtPointer: begin
                 WriteInstruction(cNone, LD_C_I32, VM_R1, 1);
                 WriteInstruction(cNone, SUB_NATIVE, VM_R0, VM_R0, VM_R1);
                 WriteSTInstruction(Ctx, D);
                 Exit;
               end;
               dtInt64, dtUInt64: begin
                 WriteInstruction(cNone, LD_C_I32, VM_R1, 1);
                 WriteInstruction(cNone, SUB_I64, VM_R0, VM_R0, VM_R1);
                 WriteSTInstruction(Ctx, D);
                 Exit;
               end;
             else
               Code := VM_NOPE;
             end;
    else
      Code := VM_NOPE;
    end;
    Assert(Code <> VM_NOPE);
    WriteInstruction(Ctx.Cond, Code, 1);
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_INCREF(const Ctx: TILTContext; var Args: TIL_INCREF_ARGS);
var
  Variable: PILVariable;
  TypeInfo: PRTTIType;
  VMCode: TVMCode;
begin
  with Args do
  begin
    if D.ArgClass = ARG_VAR then
    begin
      Variable := D.AsVariable;
      TypeInfo := GetTypeInfo(Variable.RTTI.DataType);
      WriteLDInstruction(Ctx, D);

      if (TypeInfo.ImportLib <> 0) then // если это Delphi интерфейс
        VMCode := DINF_INCREF
      else
        VMCode := _DestInstructions[vmIncRef, TypeInfo.DataTypeID];

      Assert(VMCode <> VM_NOPE);
      WriteInstruction(cNone, VMCode);
    end;
  end;
end;

function TVMTranslator.GetDstInstruction(VMCodeClass: TVMCodeClass; const Arg: TILArgument): TVMCode;
var
  TI: PRTTIType;
begin
  TI := GetTypeInfo(Arg.TypeInfo);
  Result := _DestInstructions[VMCodeClass, TI.DataTypeID];
end;

procedure TVMTranslator.Translate_DECREF(const Ctx: TILTContext; var Args: TIL_DECREF_ARGS);
var
  TypeInfo: PRTTIType;
  VMCode: TVMCode;
begin
  if Assigned(Args.D.Next) then
    CheckListArg(Ctx, Args.D);


  with Args do
  begin
    TypeInfo := GetTypeInfo(D);

    WriteLDInstruction(Ctx, D);
    if (TypeInfo.ImportLib <> 0) then // если это Delphi интерфейс
      VMCode := DINF_DECREF
    else
      VMCode := _DestInstructions[vmDecRef, TypeInfo.DataTypeID];

    Assert(VMCode <> VM_NOPE);
    if VMCode = OBJ_DECREF then
      WriteInstruction(cNone, VMCode, 0 {empty final proc})
    else
      WriteInstruction(cNone, VMCode);
  end;
end;

procedure TVMTranslator.Translate_DECREFFINAL(const Ctx: TILTContext; var Args: TIL_DECREFFINAL_ARGS);
var
  TypeInfo: PRTTIType;
  VMCode: TVMCode;
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, Dst);
    TypeInfo := GetTypeInfo(Dst);
    if (TypeInfo.ImportLib <> 0) then
      VMCode := DINF_DECREF // если это Delphi интерфейс
    else
      VMCode := _DestInstructions[vmDecRef, TypeInfo.DataTypeID];

    CheckVMCode(VMCode);

    if VMCode = OBJ_DECREF then
      WriteInstruction(cNone, VMCode, ArgGet(FinalProc.AsMethod.Proc))
    else
      WriteInstruction(cNone, VMCode, ArgGet(FinalProc.AsProcedure));
  end;
end;

procedure TVMTranslator.Translate_JMP(const Ctx: TILTContext; var Args: TIL_JMP_ARGS);
begin
  if Args.D.I32 >= Length(Ctx.Proc.IL) then
    AbortWork('Invalid jump offset');
  WriteInstruction(Ctx.Cond, VM_JMP, Args.D.I32);
end;

procedure TVMTranslator.AddFixOffset(Offset: TOffset);
begin
  FFixTable.Add(Pointer(Offset));
end;

procedure TVMTranslator.AfterLoadILGlobalVars(ILUnit: TILUnit);
var
  c: Integer;
begin
  c := Length(ILUnit.Vars);
  ILUnit.VMUnit.VarsCount := c;
  if c > 0 then begin
    ILUnit.VMUnit.Vars := IMG.Position;
    ILUnit.VMVars := IMG.MemoryPosition;
  end else begin
    ILUnit.VMUnit.Vars := 0;
    ILUnit.VMVars := nil;
    Exit;
  end;
  IMG.IncPosition(c*SizeOf(TVMVariable));
end;

procedure TVMTranslator.SaveTargetCode(Stream: TStream);
var
  IMGFormat: TIMGFormat;
  RTTISize: Integer;
begin
  // разрядность образа
  if PTR_SIZE = 4 then
    IMGFormat := IMG_32Bit
  else
    IMGFormat := IMG_64Bit;

  FIMGHeader.IMGFlags := Byte(IMGFormat);
  FIMGHeader.IMGFlags := FIMGHeader.IMGFlags or (Byte(FRTTICharset) shl 2);

  if IncludeRTTI then begin
    FIMGHeader.IMGFlags := FIMGHeader.IMGFlags or IMG_HAS_RTTI;
    FIMGHeader.RTTIOffset := IMG.Position;
  end else
  if Assigned(FStrictRTTI) then
    FIMGHeader.RTTIOffset := IMG.Position;

  if IncludeDebugInfo then
    FIMGHeader.IMGFlags := FIMGHeader.IMGFlags or IMG_HAS_DEBUG_INFO;

  if FFixTable.Count > 0 then
  begin
    if IncludeRTTI then
      FIMGHeader.FixTable := IMG.Position + FRTTI.Position + FRTTIProcs.Position
    else
      FIMGHeader.FixTable := IMG.Position;
  end else
    FIMGHeader.FixTable := 0;

  // размер образа
  FIMGHeader.IMGSize := IMG.Position;
  IMG.Position := 0;
  Stream.CopyFrom(IMG, FIMGHeader.IMGSize);

  {сохраняем RTTI}
  if IncludeRTTI then begin
    {добавляем RTTI типов}
    RTTISize := FRTTI.Position;
    FRTTI.Position := 0;
    Stream.CopyFrom(FRTTI, RTTISize);
    {добавляем RTTI процедур}
    RTTISize := FRTTIProcs.Position;
    FRTTIProcs.Position := 0;
    Stream.CopyFrom(FRTTIProcs, RTTISize);
  end else begin
    RTTISize := FStrictRTTI.Position;
    if RTTISize > 0 then begin
      FStrictRTTI.Position := 0;
      Stream.CopyFrom(FStrictRTTI, RTTISize);
    end;
  end;
  WriteFixTable(Stream);
end;

procedure TVMTranslator.WriteStrictRTTISection;
var
  ui, ti: Integer;
  UN: TILUnit;
  ILType: PILType;
  TypeInfo: PRTTIType;
  ClassInfo: PRTTIClass;
begin
  FStrictRTTI.Size := SizeOf(TRTTIClass)*10;  // default size
  FStrictRTTI.Position := 0;
  for ui := 0 to Length(FUnits) - 1 do
  begin
    UN := FUnits[ui];
    for ti := 0 to Length(UN.Types) - 1 do
    begin
      ILType := addr(UN.Types[ti]);
      TypeInfo := GetTypeInfo(ILType.Offset);
      if TypeInfo.DataTypeID = dtClass then
      begin
        ClassInfo := FStrictRTTI.MemoryPosition;
        ILType.StrictRTTI := FStrictRTTI.Position;
        FStrictRTTI.IncPosition(SizeOf(TRTTIClass));
        Move(TypeInfo^, ClassInfo^, SizeOf(TRTTIClass));
        ClassInfo.Fields := 0;
        ClassInfo.Methods := 0;
      end;
    end;
  end;
end;

procedure TVMTranslator.WriteTypesRTTIArray(ILUnit: TILUnit);
var
  i, Cnt: Integer;
  ILType: PILType;
begin
  Cnt := Length(ILUnit.Types);
  if Cnt = 0 then
  begin
    ILUnit.VMUnit.Types := 0;
    Exit;
  end;
  {заголовок дин. массива RTTI типов модуля}
  FRTTI.WriteNativeInt(-1);   // refcnt
  FRTTI.WriteNativeInt(Cnt);  // length
  ILUnit.VMUnit.Types := FRTTI.Position;
  for i := 0 to Cnt - 1 do
  begin
    ILType := addr(ILUnit.Types[i]);
    FRTTI.WriteNativeInt(ILType.Offset + SizeOf(TVMObjHeader)); // ссылка на обьект TRTTIType
  end;
end;

procedure TVMTranslator.WriteUnitsRTTIArray(UnitsCount: Integer);
var
  i: Integer;
  ObjOffset: TOffset;
begin
  IMG.WriteNativeInt(-1);          // refcnt
  IMG.WriteNativeInt(UnitsCount);  // length
  FIMGHeader.UnitsOffset := IMG.Position;
  ObjOffset := IMG.Position + UnitsCount*PTR_SIZE;
  for i := 0 to UnitsCount - 1 do
  begin
    IMG.WriteNativeInt(ObjOffset + SizeOF(TVMObjHeader));    // ссылка на обьект TRTTIUnit
    Inc(ObjOffset, SizeOf(TRTTIUnit));
  end;
end;

procedure TVMTranslator.SetBreakPoints;
var
  ui, pbi: Integer;
  U: TILUnit;
begin
  for ui := 0 to Length(FUnits) - 1 do
  begin
    U := FUnits[ui];
    for pbi := 0 to Length(U.BreakPoints) - 1 do
    begin
      U.BreakPoints[pbi].Offset := GetVMCodeLineOffset(U.BreakPoints[pbi].SrcTextLine);
      U.BreakPoints[pbi].VMTextLine := GetVMCodeLine(U.BreakPoints[pbi].SrcTextLine);
    end;
  end;
end;

procedure TVMTranslator.Translate_MEMGET(const Ctx: TILTContext; var Args: TIL_DNEW_ARGS);
var
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(Args.D.TypeInfo);
  TypeInfo := GetTypeInfo(PRTTIPointer(TypeInfo).RefTypeInfoOffset);
  WriteInstruction(Ctx.Cond, MEM_ALLOC, TypeInfo.DataSize);
  WriteSTInstruction(Ctx, Args.D);
end;

procedure TVMTranslator.Translate_NOPE(const Ctx: TILTContext);
begin
  WriteInstruction(cNone, VM_NOPE);
end;

procedure TVMTranslator.Translate_NOT(const Ctx: TILTContext; var Args: TIL_NOT_ARGS);
begin
  WriteLDInstruction(Ctx, Args.S, VM_R0);
  CheckConvert(Ctx, VM_R0, Args.D.TypeInfo, Args.S.TypeInfo);
  WriteInstruction(Ctx.Cond, GetInstruction(Ctx.ILCode, Args.D.TypeInfo));
  WriteSTInstruction(Ctx, Args.D);
end;

procedure TVMTranslator.Translate_NOW(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  WriteInstruction(Ctx.Cond, VM_SYSMACRO, VM_R0, Ord(vmsmNow));
  WriteSTInstruction(Ctx, Args.D);
end;

procedure TVMTranslator.Translate_PCALL(const Ctx: TILTContext; var Args: TIL_PCALL_ARGS);
var
  i, StartPos: Integer;
  Params: PRTTIParams;
  ParamsCount: Integer;
  Param: PRTTIParameter;
  ParamsOffset: Integer;
  ResultOffset: Integer;
  ParamInfo: PRTTIType;
  LDCode, STCode: TVMCode;
  ProcResultType: PRTTIType;
  ADDR: NativeUInt;
  CallType: TCallType;
  PrevParam: PRTTIParameter;
  Arg: TILArgument;
  ResultDataSize: Integer;
begin
  //===============================================================
  // порядок размещения параметров в стеке вызываемой процедуры  //
  //===============================================================
  // смещение в стеке VM результата начинается всегда с нуля
  // для методов self-параметр идет следующим после результата (если он есть)
  // смещение в стеке VM первого входного параметра начинается с нуля или после результати и self-параметра
  //===============================================================
  CallType := PrepareCALLInfo(Args.PArg, ParamsCount, Params, ProcResultType, ADDR);
  if Ctx.ILCode = icVirtCall then
    CallType := CallMethodVirtual;

  // LOffset - смещение на стеке для аргументов вызываемой процедуры
  // параметры распалагаются сразу за локальными переменными
  ParamsOffset := Ctx.Proc.GetStackSize();

  // если это НЕ внешний вызов, то резирвируем также 2 указателя
  // куда будет сохранен адрес возврата и указатель стека процедуры
  // для внешнего вызова эти поля не нужны
  if not (CallType in [CallStaticExternal, CallMethodExternal]) then
    ParamsOffset := ParamsOffset + PTR_SIZE*2;

  ResultOffset := ParamsOffset;

  {обработка возвращаемого значения}
  if Assigned(ProcResultType) then
  begin
    {если возвращаемый парамет структурный value-тип, то передаем на него ссылку}
    if ProcResultType.DataTypeID in [dtRecord, dtGuid, dtStaticArray, dtSet] then
    begin
      WriteInstruction(Ctx.Cond, GetLDStrongPTRInstruction(Args.CallArgs[0]), Args.CallArgs[0]);
      WriteInstruction(Ctx.Cond, GetSTPTRInstruction(atLocal), ResultOffset);
      ProcResultType := nil;
      Inc(ParamsOffset, PTR_SIZE);
    end else
    {если возвращаемый парамет ссылочный, зануляем его}
    if ProcResultType.DataTypeID in [dtString, dtAnsiString, dtDynArray, dtInterface] then
    begin
      WriteInstruction(Ctx.Cond, CLR_L_NATIVE, ResultOffset);
      Inc(ParamsOffset, PTR_SIZE);
    end else
      Inc(ParamsOffset, GetParamDataSize(@Params[0]));
    StartPos := 1;
  end else begin
    StartPos := 0;
  end;

  {если ссылка на TMethod - генерируем передачу параметра "self"}
  if CallType = CallMethodIndirect then
  begin
    WriteLDInstruction(Ctx, Args.PArg);
    WriteInstruction(Ctx.Cond, LD_D_NATIVE, VM_R0, PTR_SIZE);
    WriteInstruction(Ctx.Cond, ST_L_NATIVE, ParamsOffset);
    Inc(ParamsOffset, PTR_SIZE);
  end else
  if CallType in [CallMethod, CallMethodVirtual, CallMethodInterface, CallMethodExternal] then
  begin
    {генерируем передачу параметра "self"}
    WriteInstruction(Ctx.Cond, GetLDPTRInstruction(Args.PArg.AsMethod.Self), Args.PArg.AsMethod.Self);
    WriteInstruction(Ctx.Cond, ST_L_NATIVE, ParamsOffset);
    Inc(ParamsOffset, PTR_SIZE);
    if (CallType = CallMethodExternal) and ((Args.PArg.AsMethod.Proc.Flags and ILPROC_CONSTRUCTOR) <> 0) then
    begin
      {генерируем передачу второго параметра "1" для конструктора}
      WriteInstruction(Ctx.Cond, LD_C_ONE);
      WriteInstruction(Ctx.Cond, ST_L_NATIVE, ParamsOffset);
      Inc(ParamsOffset, SizeOf(Int32));
    end;
  end;

  {обработка входных аргументов}
  PrevParam := nil;
  for i := StartPos to ParamsCount - 1 do begin
    Param := @(Params[i]);
    Arg := Args.CallArgs[i];
    ParamInfo := GetTypeInfo(Param.DataType);
    {если параметр передается по ссылке}
    if PassByRef(Param) then begin
      if Arg.IsReference then
        LDCode := LD_L_NATIVE
      else
      case Arg.ArgScope of
        ARG_SCOPE_LOCAL: LDCode := LD_L_PTR;
        ARG_SCOPE_GLOBAL: LDCode := LD_G_PTR;
        else begin
          AbortWork('Scope type %d is not supported', [Integer(Arg.ArgScope)]);
          LDCode := VM_NOPE;
        end;
      end;
      STCode := ST_L_NATIVE;
    end else
    {если параметр передается по значению}
    begin
      if Arg.ArgClass = ARG_PROC  then
      begin
        LDCode := LD_G_PROC;
        STCode := ST_L_NATIVE;
      end else
      // временно для openarray
      if ParamInfo.DataTypeID = dtOpenArray then
      begin
        // есил это нулевая константа
        if Arg.U32 = 0 then
          LDCode := LD_C_ZERO
        else
          LDCode := GetLDPTRInstruction(Arg);
        STCode := ST_L_NATIVE;
      end else
      {if ParamInfo.DataSize > PTR_SIZE then
      begin
        //CheckManagedSrc(Context, Arg^);
        LDCode := GetLDPTRInstruction(Arg^);
        STCode := ST_L_NATIVE;
        //PrevParam := Param;
        //Continue;
      end else}
      if (ParamInfo.DataTypeID = dtProcType) and (ParamInfo.DataSize = PTR_SIZE*2) then
      begin
        WriteLDInstruction(Ctx, Arg, VM_R1);
        WriteInstruction(Ctx.Cond, LD_L_PTR, NativeUInt(ParamsOffset));
        WriteInstruction(Ctx.Cond, MOVE_MEM_C, VM_R0, VM_R1, ParamInfo.DataSize);
        Inc(ParamsOffset, GetParamDataSize(Param));
        PrevParam := Param;
        Continue;
      end else
      if (ParamInfo.DataTypeID in [dtStaticArray, dtRecord, dtGuid, dtSet]) and not (ParamInfo.DataSize in [1, 2, 4, 8]) then
      begin
        if ParamInfo.DataSize > 8 then
        begin
          case Arg.ArgScope of
            ARG_SCOPE_LOCAL: LDCode := LD_L_PTR;
            ARG_SCOPE_GLOBAL: LDCode := LD_G_PTR;
            else begin
              AbortWork('Scope type %d is not supported', [Integer(Arg.ArgScope)]);
              LDCode := VM_NOPE;
            end;
          end;
          STCode := ST_L_NATIVE;
        end else begin
          //CheckManagedSrc(Context, Arg^);
          WriteLDInstruction(Ctx, Arg, VM_R1);
          WriteInstruction(Ctx.Cond, LD_L_PTR, NativeUInt(ParamsOffset));
          WriteInstruction(Ctx.Cond, MOVE_MEM_C, VM_R0, VM_R1, ParamInfo.DataSize);
          PrevParam := Param;
          Continue;
        end;
      end else begin
        //====================================================================================
        // так как в делфи, для открытых массивов скрытый параметр равен Length - 1 то
        // уменьшаем значение скрытого параметра на 1
        if Assigned(PrevParam) and (CallType in [CallStaticExternal, CallMethodExternal]) and
          (GetTypeInfo(PrevParam.DataType).DataTypeID = dtOpenArray) then
          Arg.U32 := Arg.U32 - 1;
        //====================================================================================
        LDCode := GetLDInstruction(Arg);
        STCode := GetSTInstruction(Param.DataType, atLocal);
      end;
    end;
    {оптимизация команд: начало}
    if CHECK_ST_C_I32_APPLY(Arg, ParamInfo, Param.IsReference) then
      WriteInstruction(Ctx.Cond, ST_C_I32, ParamsOffset, Arg.NUInt)
    else
    if CHECK_MOVE_L_I32_APPLY(Arg, ParamInfo, Param.IsReference) then
      WriteInstruction(Ctx.Cond, MOVE_L_I32, ParamsOffset, Arg.GetVarOffset)
    else
    {оптимизация команд: конец}
    begin
      WriteInstruction(Ctx.Cond, LDCode, Arg);
      CheckConvert(Ctx, VM_R0, Param.DataType, Arg.TypeInfo);
      WriteInstruction(Ctx.Cond, STCode, NativeUint(ParamsOffset));
    end;
    Inc(ParamsOffset, GetParamDataSize(Param));
    PrevParam := Param;
  end;

  {генерация инструкци вызова}
  case CallType of
    CallStatic, CallMethod: WriteInternalCALLInstruction(Ctx, ADDR);
    CallStaticExternal: WriteExternalCALLInstruction(Ctx, TILProc(ADDR));
    CallMethodExternal: WriteExternalCALLInstruction(Ctx, Args.PArg.AsMethod.Proc);
    CallIndirect, CallMethodIndirect: WriteInderectInternalCALLInstruction(Ctx, Args.PArg);
    CallMethodVirtual: WriteVirtualCALLInstruction(Ctx, @Args.PArg.AsMethod);
    CallMethodInterface: WriteInterfaceCALLInstruction(Ctx, @Args.PArg.AsMethod);
  else
    raise Exception.Create('Invalid call type');
  end;

  {генерация инструкции сохранения результата}
  if Assigned(ProcResultType) then
  begin
    ParamInfo := GetTypeInfo(Params[0].DataType);
    Arg := Args.CallArgs[0];
    ResultDataSize := GetResultParamDataSize(ParamInfo);
    {оптимизация команд: начало}
    if Arg.IsLocalVar and (ResultDataSize <= 4) then
      WriteInstruction(Ctx.Cond, MOVE_L_I32, Arg.GetVarOffset, ResultOffset)
    else
    {оптимизация команд: конец}
    begin
      LDCode := GetLDInstruction(Params[0].DataType, atLocal);
      WriteInstruction(Ctx.Cond, LDCode, NativeUInt(ResultOffset));
      if ResultDataSize in [1, 2, 4, 8] then
        WriteSTInstruction(Ctx, Arg)
      else begin
        WriteLDInstruction(Ctx, Arg, VM_R1);
        WriteInstruction(Ctx.Cond, MOVE_MEM_C, VM_R1, VM_R0, ParamInfo.DataSize);
      end;
    end;
  end;
end;

procedure TVMTranslator.Translate_QUERYTYPE(const Ctx: TILTContext; var Args: TIL_QUERYTYPE_ARGS);
var
  DTypeInfo: PRTTIType;
  RTypeInfo: PRTTIType;
  MacroID: TVMMacroID;
begin
  with Args do
  begin
    DTypeInfo := GetTypeInfo(D.TypeInfo);
    RTypeInfo := GetTypeInfo(R.TypeInfo);

    case DTypeInfo.DataTypeID of
      dtBoolean: begin
        if RTypeInfo.DataTypeID = dtClass then
          MacroID := vmsmCheckClass
        else
          MacroID := vmsmCheckIntf;
      end;
      dtClass: MacroID := vmsmQueryClass;
      dtInterface: MacroID := vmsmQueryIntf;
    else
      raise Exception.CreateFmt('QUERYTYPE: The type "%s" is not supported', [GetDataTypeName(DTypeInfo.DataTypeID)]);
    end;

    WriteLDInstruction(Ctx, L, VM_R0);

    if RTypeInfo.DataTypeID = dtClass then
      WriteLDInstruction(Ctx, R, VM_R1)
    else
      WriteLDConstant(Ctx.Cond, PRTTIInterface(RTypeInfo).InterfaceID, VM_R1);

    WriteInstruction(Ctx.Cond, VM_SYSMACRO, VM_R0, VM_R1, NativeUInt(MacroID));
    WriteSTInstruction(Ctx, D, VM_R0);
  end;
end;

procedure TVMTranslator.Translate_REFCNT(const Ctx: TILTContext; var Args: TIL_DS_ARGS);
var
  SrcTypeInfo: PRTTIType;
  MacroID: TVMMacroID;
begin
  with Args do
  begin
    SrcTypeInfo := GetTypeInfo(S);
    WriteLDInstruction(Ctx, S);
    case SrcTypeInfo.DataTypeID of
      dtString, dtAnsiString: MacroID := vmsmStrRefCount;
      dtDynArray: MacroID := vmsmArrRefCount;
      dtClass, dtInterface: MacroID := vmsmObjRefCount;
    else
      AbortWork('unsuported type');
      MacroID := vmsmCheckClass;
    end;
    WriteInstruction(Ctx.Cond, VM_SYSMACRO, NativeUInt(MacroID));
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_RET(const Ctx: TILTContext);
begin
  WriteInstruction(Ctx.Cond, PROC_RET);
end;

procedure TVMTranslator.Translate_GETBIT(const Ctx: TILTContext; var Args: TIL_GETBIT_ARGS);
var
  DTypeInfo: PRTTIType;
  MaskValue: Int64;
begin
  with Args do
  begin
    DTypeInfo := GetTypeInfo(BitArray);
    //===============================================================
    // если индекс бита - константа
    //===============================================================
    if BitIndex.ArgumentType = atImmConst then
    begin
      // загружаем битовый набор
      WriteLDInstruction(Ctx, BitArray, VM_R0);
      MaskValue := UInt64(1) shl BitIndex.U32;
      // загружаем маску

      if DTypeInfo.DataSize <= 4 then
      begin
        WriteLDConstant(cNone, MaskValue, VM_R1);
        WriteInstruction(cNone, BIN_AND32, VM_R1, VM_R0, VM_R1);
        WriteInstruction(cNone, CMP_TEST32, VM_R0, VM_R1, VM_R1);
      end else begin
        WriteLDConstant64(cNone, MaskValue, VM_R1);
        WriteInstruction(cNone, BIN_AND64, VM_R1, VM_R0, VM_R1);
        WriteInstruction(cNone, CMP_TEST64, VM_R0, VM_R1, VM_R1);
      end;

      WriteInstruction(cNonZero, LD_C_ONE, VM_R1, VM_R0);
      WriteInstruction(cZero, LD_C_ZERO, VM_R1, VM_R0);
      // сохраняем результат
      WriteSTInstruction(Ctx, Dst, VM_R1);
    end else
    //===============================================================
    // если индекс бита - переменная
    //===============================================================
    begin
      // загружаем битовый набор
      WriteLDInstruction(Ctx, BitArray, VM_R0);

      // вычесляем маску
      WriteLDInstruction(Ctx, BitIndex, VM_R1);
      WriteLDConstant(cNone, 1, VM_R2);
      WriteInstruction(Ctx.Cond, BIN_SHL64, VM_R1, VM_R2, VM_R1);

      if DTypeInfo.DataSize <= 4 then
      begin
        WriteInstruction(cNone, BIN_AND32, VM_R1, VM_R0, VM_R1);
        WriteInstruction(cNone, CMP_TEST32, VM_R0, VM_R1, VM_R1)
      end else begin
        WriteInstruction(cNone, BIN_AND64, VM_R1, VM_R0, VM_R1);
        WriteInstruction(cNone, CMP_TEST64, VM_R0, VM_R1, VM_R1);
      end;

      WriteInstruction(cNonZero, LD_C_ONE, VM_R1, VM_R0);
      WriteInstruction(cZero, LD_C_ZERO, VM_R1, VM_R0);
      // сохраняем результат
      WriteSTInstruction(Ctx, Dst, VM_R1);
    end;
  end;
end;

procedure TVMTranslator.Translate_SETBIT(const Ctx: TILTContext; var Args: TIL_SETBIT_ARGS);
var
  DTypeInfo: PRTTIType;
  MaskValue: Int64;
begin
  with Args do
  begin
    DTypeInfo := GetTypeInfo(Dst);
    //===============================================================
    // если индекс бита - константа
    //===============================================================
    if BitIndex.ArgumentType = atImmConst then
    begin
      MaskValue := UInt64(1) shl BitIndex.U32;
      // если значение бита - константа
      if BitValue.ArgumentType = atImmConst then
      begin
        // загружаем битовый набор
        WriteLDInstruction(Ctx, Dst, VM_R0);
        // установить бит
        if BitValue.U32 = 1 then
        begin
          if DTypeInfo.DataSize <= 4 then
          begin
            WriteLDConstant(cNone, MaskValue, VM_R1);
            WriteInstruction(cNone, BIN_OR32, VM_R0, VM_R0, VM_R1)
          end else begin
            WriteLDConstant64(cNone, MaskValue, VM_R1);
            WriteInstruction(cNone, BIN_OR64, VM_R0, VM_R0, VM_R1)
          end;
        end else
        // снять бит
        begin
          MaskValue := not MaskValue;
          if DTypeInfo.DataSize <= 4 then
          begin
            WriteLDConstant(cNone, MaskValue, VM_R1);
            WriteInstruction(cNone, BIN_AND32, VM_R0, VM_R0, VM_R1)
          end else begin
            WriteLDConstant64(cNone, MaskValue, VM_R1);
            WriteInstruction(cNone, BIN_AND64, VM_R0, VM_R0, VM_R1)
          end;
        end;
        // сохраняем битовый набор
        WriteSTInstruction(Ctx, Dst, VM_R0);
      end else
      // если значение бита - переменная
      begin
        // загружаем битовый набор
        WriteLDInstruction(Ctx, Dst, VM_R0);
        WriteLDInstruction(Ctx, BitValue, VM_R1);
        WriteInstruction(Ctx.Cond, CMP_TEST32, VM_R0, VM_R1, VM_R1);
        if DTypeInfo.DataSize <= 4 then
        begin
          WriteLDConstant(cNone, MaskValue, VM_R2);
          // инвертируем маску если нужно снять бит
          WriteInstruction(cZero, BIN_NOT32, VM_R2, VM_R2);
          WriteInstruction(cNonZero, BIN_OR32, VM_R0, VM_R0, VM_R2);
          WriteInstruction(cZero, BIN_AND32, VM_R0, VM_R0, VM_R2);
        end else begin
          WriteLDConstant64(cNone, MaskValue, VM_R2);
          // инвертируем маску если нужно снять бит
          WriteInstruction(cZero, BIN_NOT64, VM_R2, VM_R2);
          WriteInstruction(cNonZero, BIN_OR64, VM_R0, VM_R0, VM_R2);
          WriteInstruction(cZero, BIN_AND64, VM_R0, VM_R0, VM_R2);
        end;
        // сохраняем битовый набор
        WriteSTInstruction(Ctx, Dst, VM_R0);
      end;
    end else
    //===============================================================
    // если индекс бита - переменная
    //===============================================================
    begin
      // расчет маски в VM_R2
      WriteLDInstruction(Ctx, BitIndex, VM_R1);
      WriteLDConstant(cNone, 1, VM_R2);
      WriteInstruction(Ctx.Cond, BIN_SHL64, VM_R2, VM_R2, VM_R1);
      // загружаем битовый набор
      WriteLDInstruction(Ctx, Dst, VM_R0);
      // если значение бита - константа
      if (BitValue.ArgumentType = atImmConst) then
      begin
        // установить бит
        if BitValue.I32 = 1 then
        begin
          if DTypeInfo.DataSize <= 4 then
            WriteInstruction(cNone, BIN_OR32, VM_R0, VM_R0, VM_R2)
          else
            WriteInstruction(cNone, BIN_OR64, VM_R0, VM_R0, VM_R2);
        end else
        // снять бит
        begin
          WriteInstruction(Ctx.Cond, BIN_NOT64, VM_R2, VM_R2);
          if DTypeInfo.DataSize <= 4 then
            WriteInstruction(cNone, BIN_AND32, VM_R0, VM_R0, VM_R2)
          else
            WriteInstruction(cNone, BIN_AND64, VM_R0, VM_R0, VM_R2);
        end;
      end else
      // если значение бита - переменная
      begin
        WriteLDInstruction(Ctx, BitValue, VM_R1);
        WriteInstruction(Ctx.Cond, CMP_TEST32, VM_R0, VM_R1, VM_R1);
        if DTypeInfo.DataSize <= 4 then
        begin
          // инвертируем маску если нужно снять бит
          WriteInstruction(cZero, BIN_NOT32, VM_R2, VM_R2);
          WriteInstruction(cNonZero, BIN_OR32, VM_R0, VM_R0, VM_R2);
          WriteInstruction(cZero, BIN_AND32, VM_R0, VM_R0, VM_R2);
        end else begin
          // инвертируем маску если нужно снять бит
          WriteInstruction(cZero, BIN_NOT64, VM_R2, VM_R2);
          WriteInstruction(cNonZero, BIN_OR64, VM_R0, VM_R0, VM_R2);
          WriteInstruction(cZero, BIN_AND64, VM_R0, VM_R0, VM_R2);
        end;
      end;
      // сохраняем битовый набор
      WriteSTInstruction(Ctx, Dst, VM_R0);
    end;
  end;
end;

procedure TVMTranslator.Translate_SETBOOL(const Ctx: TILTContext; var Args: TIL_SETBOOL_ARGS);
begin
  WriteInstruction(Ctx.Cond, LD_C_ONE);
  WriteInstruction(InverseCondition(Ctx.Cond), LD_C_ZERO);
  WriteSTInstruction(Ctx, cNone, Args.D);
end;

procedure TVMTranslator.Translate_INIT(const Ctx: TILTContext; var Args: TIL_INIT_ARGS);
var
  TypeInfo: PRTTIType;
  VMCode: TVMCode;
begin
  with Args do begin
    if D.ArgumentType = atField then
      WriteLDSelfInstruction(Ctx, Ctx.Proc.SelfArg);

    TypeInfo := GetTypeInfo(D);
    VMCode := _ClearInstructions[TypeInfo.DataTypeID, D.ArgumentType];
    CheckVMCode(VMCode);
    WriteInstruction(Ctx.Cond, VMCode, D);
  end;
end;

procedure TVMTranslator.Translate_CHKBND(const Ctx: TILTContext; var Args: TIL_CHCKBND_ARGS);
var
  AInfo: PRTTIArray;
  Dimentions: PRTTIDimensions;
  Dim: PRTTIOrdinal;
begin
  with Args do
  begin
    AInfo := PRTTIArray(GetTypeInfo(AArray.TypeInfo));
    if AInfo.DataTypeID = dtPointer then
      AInfo := PRTTIArray(GetTypeInfo(PRTTIPointer(AInfo).RefTypeInfoOffset));

    if AInfo.DimensionsCount > 1 then
      raise Exception.Create('More then one dimensions is not supported');

    case AInfo.DataTypeID of
      dtStaticArray, dtSet: begin
        // пока только для одного измерения
        Dimentions := PRTTIDimensions(GetTypeInfo(AInfo.Dimensions));
        Dim := PRTTIOrdinal(GetTypeInfo(Dimentions[0]));
        WriteLDInstruction(Ctx, AIndex, VM_R0);
        WriteInstruction(cNone, SCHECKB, Dim.LoBound, Dim.HiBound);
      end;
      dtOpenArray: begin
        WriteInstruction(cNone, LD_L_I32, VM_R1, VM_R0, AArray.AsVariable.RTTI.Offset + PTR_SIZE);
        WriteLDInstruction(Ctx, AIndex, VM_R0);
        WriteInstruction(cNone, DCHECKB, VM_R0, VM_R1);
      end;
      dtDynArray, dtString, dtAnsiString: begin
        WriteLDInstruction(Ctx, AArray, VM_R0);
        WriteLDInstruction(Ctx, AIndex, VM_R1);
        WriteInstruction(cNone, ACHECKB, VM_R0, VM_R1);
      end;
    else
      raise Exception.Create('Type is not supported');
    end;
  end;
end;

procedure TVMTranslator.Translate_DNEWOBJ(const Ctx: TILTContext; var Args: TIL_DNEWOBJ_ARGS);
var
  ClassInfo: PRTTIType;
  ILType: PILType;
begin
  ClassInfo := GetTypeInfo(Args.S.TypeInfo);
  ILType := addr(FUnits[ClassInfo.UnitID].Types[ClassInfo.Index]);
  WriteInstruction(Ctx.Cond, OBJ_CREATE, ArgGet(ILType)); // аругмент - указтель на TILType
  WriteSTInstruction(Ctx, Args.D);
end;

procedure TVMTranslator.Translate_RDREF(const Ctx: TILTContext; var Args: TIL_RDREF_ARGS);
var
  PtrRTTI: PRTTIPointer;
  Code: TVMCode;
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, S, VM_R1);
    PtrRTTI := PRTTIPointer(GetTypeInfo(S.TypeInfo));
    Code := GetInstruction(icReadDRef, PtrRTTI.RefTypeInfoOffset);
    WriteInstruction(Ctx.Cond, Code, VM_R0, VM_R1);
    WriteSTInstruction(Ctx, D, VM_R0);
  end;
end;

procedure TVMTranslator.Translate_WDREF(const Ctx: TILTContext; var Args: TIL_WDREF_ARGS);
var
  PtrRTTI: PRTTIPointer;
  Code: TVMCode;
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, D, VM_R0);
    WriteLDInstruction(Ctx, S, VM_R1);
    PtrRTTI := PRTTIPointer(GetTypeInfo(D.TypeInfo));
    Code := GetInstruction(icWriteDRef, PtrRTTI.RefTypeInfoOffset);
    WriteInstruction(Ctx.Cond, Code, VM_R0, VM_R1);
  end;
end;

procedure TVMTranslator.Translate_ETHROW(const Ctx: TILTContext; var Args: TIL_ETHROW_ARGS);
begin
  WriteLDInstruction(Ctx, Args.D);
  WriteInstruction(Ctx.Cond, ETHROW);
end;

procedure TVMTranslator.Translate_FMACRO(const Ctx: TILTContext; var Args: TIL_FMACRO_ARGS);
var
  ILMacroID: TILMacroID;
  VMMacroID: TVMMacroID;
begin
  with Args do
  begin
    ILMacroID := M.U32;
    case ILMacroID of
      IL_MACROID_GETCALLSTACK:
      begin
        WriteInstruction(cNone, LD_G_PROC, VM_R0, NativeUInt(Ctx.Proc));
        VMMacroID := vmsmGetCallStack;
      end;
      IL_MACROID_GETCURUNIT: begin
        WriteInstruction(cNone, LD_C_I32, VM_R0, Ctx.PUnit.Index);
        VMMacroID := vmsmGetCurUnit;
      end;
      IL_MACROID_GETUNITSLIST: VMMacroID := vmsmGetUnitsList;
    else
      AbortWork('Unknown IL macro: %s(%d)', [GetILMacroName(ILMacroID), ILMacroID]);
      Exit;
    end;
    WriteInstruction(cNone, VM_SYSMACRO, VM_R0, VM_R0, VM_R0, Ord(VMMacroID));
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_MEMFREE(const Ctx: TILTContext; var Args: TIL_DFREE_ARGS);
begin
  WriteLDInstruction(Ctx, Args.D);
  WriteInstruction(Ctx.Cond, MEM_FREE);
end;

procedure TVMTranslator.Translate_NEARCALL(const Ctx: TILTContext; var Args: TIL_NEARCALL_ARGS);
begin
  if Args.D.I32 >= Length(Ctx.Proc.IL) then
    AbortWork('Invalid call addr');
  WriteInstruction(Ctx.Cond, CALL_NEAR, Args.D.U32);
end;

procedure TVMTranslator.Translate_TRYBEGIN(const Ctx: TILTContext);
begin
end;

procedure TVMTranslator.Translate_TRYEND(const Ctx: TILTContext);
begin
end;

procedure TVMTranslator.Translate_TYPEINFO(const Ctx: TILTContext; var Args: TIL_TYPEINFO_ARGS);
begin
  with Args do
  begin
    case S.ArgClass of
      ARG_PROC: WriteInstruction(cNone, LD_C_U32, VM_R1, GetArgProcRtti(S.AsProcedure));
    else
      WriteInstruction(cNone, LD_C_I32, VM_R1, S.TypeInfo);
    end;
    WriteInstruction(cNone, VM_SYSMACRO, VM_R0, VM_R1, Ord(vmsmQTypeInfo));
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_UNIQUE(const Ctx: TILTContext; var Args: TIL_D_Args);
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, D);
    WriteInstruction(Ctx.Cond, GetInstruction(icUnique, D.TypeInfo));
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_WEAKREF(const Ctx: TILTContext; var Args: TIL_WEAKREF_ARGS);
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, S);
    WriteInstruction(cNone, OBJ_WEAKREF);
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.Translate_STRONGREF(const Ctx: TILTContext; var Args: TIL_STRONGREF_ARGS);
begin
  with Args do
  begin
    WriteLDInstruction(Ctx, S);
    WriteInstruction(cNone, OBJ_STRONGREF);
    WriteSTInstruction(Ctx, D);
  end;
end;

procedure TVMTranslator.WriteProcEpilog(Proc: TILProc; LastILCode: TILCode);
begin
  // добавляем финальный _RET
  if LastILCode <> icRet then
    WriteInstruction(cNone, PROC_RET);
end;

procedure TVMTranslator.WriteProcProlog(Proc: TILProc);
begin
  WriteInstruction(cNone, VM_STACK, Proc.GetStackSize);
end;

procedure TVMTranslator.WriteProcsRTTI(ILUnit: TILUnit);
var
  i, Cnt: Integer;
  Proc: TILProc;
begin
  Cnt := Length(ILUnit.Procs);
  // заголовок массива всех процедур модуля
  FRTTIProcs.WriteNativeInt(1);        // refcnt
  FRTTIProcs.WriteNativeInt(Cnt + 2);  // length
  // новое смещение указывает на дин. массив RTTI процедур
  ILUnit.VMUnit.Procs := FRTTIProcs.Position;
  for i := 0 to Cnt - 1 do
  begin
    Proc := ILUnit.Procs[i];
    FRTTIProcs.WriteNativeUInt(Proc.ProcInfo);
  end;
  FRTTIProcs.WriteNativeUInt(ILUnit.InitProc.ProcInfo);
  FRTTIProcs.WriteNativeUInt(ILUnit.FinalProc.ProcInfo);
  // пишем информацию о локальных переменных
  if IncludeDebugInfo then
  begin
    for i := 0 to Cnt - 1 do
    begin
      Proc := ILUnit.Procs[i];
      WriteProcLocalVarDebugInfo(Proc);
    end;
    WriteProcLocalVarDebugInfo(ILUnit.InitProc);
    WriteProcLocalVarDebugInfo(ILUnit.FinalProc);
  end;
end;

procedure TVMTranslator.WriteExportProcParams(Proc: TILProc; EProc: PVMExportProc);
var
  i, c: Integer;
  Param: PRTTIParameter;
begin
  c := Length(Proc.Params);
  FRTTI.WriteNativeInt(-1);  // refcnt
  FRTTI.WriteUInt32(c);      // len
  EProc.Params := PVMParams(FRTTI.Position);
  for i := 0 to c - 1 do
  begin
    Param := @(Proc.Params[i]);
    FRTTI.WriteNativeUInt(Param.Name);
    FRTTI.WriteNativeUInt(Param.DataType);
    FRTTI.WriteNativeUInt(Param.Offset);
    FRTTI.WriteBoolean(Param.IsReference);
  end;
end;

procedure TVMTranslator.WriteVMExportProcs(ILUnit: TILUnit);
  procedure WriteProcInfo(VMProcs: PVMExportProcs; Proc: TILProc);
  var
    EProc: PVMExportProc;
    ProcRTTI: PRTTIProcedure;
  begin
    // если процедура экспортируемая, то добовляем в список экспорта
    if Proc.ExportIndex > -1 then
    begin
      ProcRTTI := GetProcInfo(Proc);
      EProc := @(VMProcs^[Proc.ExportIndex]);
      EProc.Name := ProcRTTI.Name;
      EProc.Offset := Proc.Offset;
      EProc.ProcType := Proc.ProcType;
      //EProc.StackSize := Proc.StackSize;
      EProc.Struct := ProcRTTI.StructInfo;
      WriteExportProcParams(Proc, EProc);
    end;
  end;
var
  ti, i, cnt: Integer;
  T: ^TIMGType;
  P: TILProc;
  EProcs: PVMExportProcs;
begin
  // экспорт
  cnt := ILUnit.ExportProcsCount;

  FRTTI.WriteNativeInt(-1);  // refcnt
  FRTTI.WriteNativeInt(cnt); // len

  if cnt = 0 then
  begin
    ILUnit.VMUnit.ExportProcs := 0;
    Exit;
  end;
  ILUnit.VMUnit.ExportProcs := FRTTI.Position;

  EProcs := PVMExportProcs(FRTTI.MemoryPosition);
  FRTTI.IncPosition(SizeOf(TVMExportProc)*cnt);

  for i := 0 to Length(ILUnit.Procs) - 1 do
  begin
    P := ILUnit.Procs[i];
    WriteProcInfo(EProcs, P);
  end;

  for ti := 0 to Length(ILUnit.Types) - 1 do
  begin
    T := addr(ILUnit.Types[ti]);
    for i := 0 to Length(T.Methods) - 1 do
    begin
      P := T.Methods[i];
      WriteProcInfo(EProcs, P);
    end;
  end;
end;

function TVMTranslator.GetInvokeAdapterID(Proc: TILProc): Integer;
  function ResultDataTypeToInvokeDataType(DataType: PRTTIType; out InvokeParam: TInvokeParam): Boolean;
  begin
    Result := True;
    case DataType.DataTypeID of
      dtInt8,
      dtInt16,
      dtInt32:   InvokeParam := _I32;
      dtInt64:   InvokeParam := _I64;
      dtUInt8,
      dtUInt16,
      dtUInt32:  InvokeParam := _I32;
      dtUInt64:  InvokeParam := _I64;
      dtFloat32: InvokeParam := _F32;
      dtFloat64: InvokeParam := _F64;

      dtBoolean,
      dtAnsiChar,
      dtChar:    InvokeParam := _I32;
      dtAnsiString,
      dtString:  InvokeParam := _STR;

      dtVariant,
      dtPointer,
      dtRange,
      dtEnum,
      dtProcType,
      dtClass: InvokeParam := _NINT;
      dtInterface: InvokeParam := _INF;
      dtRecord: begin
        if DataType.DataSize > 8 then
          InvokeParam := _NINT
        else
          Exit(False) // нельзя, т.к структура менее 8 байт возвращается по значению через стек
      end;
    else
      Exit(False);
    end;
  end;
  function DataTypeToInvokeDataType(DataType: PRTTIType; out InvokParam: TInvokeParam): Boolean;
  begin
    Result := True;
    case DataType.DataTypeID of
      dtInt8,
      dtInt16,
      dtInt32:   InvokParam := _I32;
      dtInt64:   InvokParam := _I64;
      dtUInt8,
      dtUInt16,
      dtUInt32:  InvokParam := _I32;
      dtUInt64:  InvokParam := _I64;
      dtFloat32: InvokParam := _F32;
      dtFloat64: InvokParam := _F64;

      dtBoolean,
      dtAnsiChar,
      dtChar: InvokParam := _I32;
      dtAnsiString,
      dtString: InvokParam := _NINT;
      dtSet: if DataType.DataSize in [1, 2, 4] then
               InvokParam := _NINT
             else
               Exit(False);
      dtVariant,
      dtGeneric,
      dtPointer,
      dtRange,
      dtEnum,

      dtStaticArray,
      dtDynArray,
      dtOpenArray,
      dtProcType,
      dtRecord,
      dtGuid,
      dtClass,
      dtInterface: InvokParam := _NINT;
    else
      Exit(False);
    end;
  end;
var
  Params: TInvokeParams;
  procedure AddParam(Param: TInvokeParam);
  var
    c: Integer;
  begin
    c := Length(Params);
    SetLength(Params, c + 1);
    Params[c] := Param;
  end;
var
  i, StartIdx, pc: Integer;
  DataType: PRTTIType;
  InvokeParamDT: TInvokeParam;
  IsFunction: Boolean;
  Param: PRTTIParameter;
  FastCallAply: Boolean;
begin
  FastCallAply := True;
  if (Proc.Flags and ILPROC_HASSELFPTR) <> 0 then // если это метод
  begin
    AddParam(_NINT);
    if (Proc.Flags and ILPROC_CONSTRUCTOR) <> 0 then // если это констструктор
      AddParam(_I32);
  end;
  pc := Length(Proc.Params);

  IsFunction := (Proc.ProcType = ptFunction);
  if (Proc.ProcType = ptFunction) then
  begin
    DataType := GetTypeInfo(Proc.Params[0].DataType);
    if DataType.DataTypeID = dtRecord then
      IsFunction := False;

    if ResultDataTypeToInvokeDataType(DataType, InvokeParamDT) then
    begin
      AddParam(InvokeParamDT);
      StartIdx := 1;
    end else begin
      FastCallAply := False;
      StartIdx := 0;
    end;
  end else
    StartIdx := 0;

  if FastCallAply then
  for i := StartIdx to pc - 1 do begin
    Param := addr(Proc.Params[i]);
    if Param.IsReference then
      InvokeParamDT := _NINT
    else begin
      DataType := GetTypeInfo(Param.DataType);
      if not DataTypeToInvokeDataType(DataType, InvokeParamDT) then
      begin
        FastCallAply := False;
        Break;
      end;
    end;
    AddParam(InvokeParamDT);
  end;

  if FastCallAply then
    Result := FindInvokeAdapterID(Proc.CallConvention, IsFunction, Params)
  else
    Result := -1;

  {$IFDEF USE_ONLY_FAST_INVOKE}
  if Result = -1 then begin
    if Proc.ProcType = ptProcedure then
      ErrorStr := '_proc'
    else
      ErrorStr := '_func';
    for i := 0 to Length(Params) - 1 do
      ErrorStr := ErrorStr + TypInfo.GetEnumName(TypeInfo(TInvokeParam), Int32(Params[i]));
    ErrorStr := TypInfo.GetEnumName(TypeInfo(TCallConv), Integer(ccReg)) + ErrorStr;
    raise Exception.CreateFmt('Map import proc "%s.%s" ERROR: Unknown invoke type: %s', [GetNameString(Proc.ILUnit.Name), GetNameString(Proc.RTTI.Name), ErrorStr]);
  end;
  {$ENDIF}
end;

procedure TVMTranslator.MapImportMethod(aType: PRTTIType; Proc: TILProc);
var
  LibName, TypeName, ProcName: UnicodeString;
  RegType: TTypeRegInfo;
  ProcPtr: Pointer;
begin
  if not Proc.IsIntfMethod then
  begin
    LibName := GetString(aType.ImportLib);
    TypeName := GetString(aType.ImportName);
    RegType := FindType(LibName, TypeName);
    if not Assigned(RegType) then
      raise Exception.CreateFmt('Import type "%s" in library "%s" is not found', [TypeName, LibName]);
    ProcName := GetString(Proc.ImportName);
    ProcPtr := RegType.FindMethod(ProcName);
    if not Assigned(ProcPtr) then
      raise Exception.CreateFmt('Import method "%s" in type %s in library "%s" is not found', [ProcName, TypeName, LibName]);
    Proc.Offset := NativeUInt(ProcPtr);
  end else
    Proc.Offset := Proc.VirtualIndex;
  Proc.InvokeAdapterID := GetInvokeAdapterID(Proc);
  Proc.ImportIndex := FImportTableCount - 1;
end;

procedure TVMTranslator.MapImportProc(Proc: TILProc);
var
  ProcPtr: Pointer;
  LibName, ProcName: UnicodeString;
begin
  LibName := GetString(Proc.ImportLib);
  ProcName := GetString(Proc.ImportName);
  ProcPtr := FindProc(LibName, ProcName);
  if not Assigned(ProcPtr) then
    raise Exception.CreateFmt('Import function "%s" in library "%s" is not found', [ProcName, LibName]);
  Proc.Offset := NativeUInt(ProcPtr);
  Proc.InvokeAdapterID := GetInvokeAdapterID(Proc);
  Proc.ImportIndex := FImportTableCount - 1;
end;

procedure RegConvertInstruction(RegNum: Integer; Destanation, Source: TDataTypeID; MachineCode: TVMCode);
begin
  {$IFDEF DEBUG}
  if _ConvertInstructions[RegNum, Destanation, Source] <> VM_NOPE then
    AbortWork('Register convert instruction Error');
  {$ENDIF}
  _ConvertInstructions[RegNum, Destanation, Source] := MachineCode;
end;

function TVMTranslator.GetLDInstruction(const Source: TILArgument): TVMCode;
begin
  Result := GetLDInstruction(Source.TypeInfo, Source.ArgumentType);
end;

function TVMTranslator.GetLDInstruction(TypeInfoOffset: TOffset; ArgType: TILArgumentType): TVMCode;
var
  EDataType: TDataTypeID;
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(TypeInfoOffset);

  if TypeInfo.DataTypeID in [dtRange, dtStaticArray, dtEnum, dtSet, dtRecord] then
  begin
    case TypeInfo.DataSize of
      1: EDataType := dtUInt8;
      2: EDataType := dtUInt16;
      4: EDataType := dtUInt32;
      8: EDataType := dtUInt64;
    else EDataType := TypeInfo.DataTypeID;
    end;
  end else
    EDataType := TypeInfo.DataTypeID;

  if (TypeInfo.DataTypeID = dtProcType) and (TypeInfo.DataSize = PTR_SIZE*2) then
  begin
    case ArgType of
      atLocal: Exit(LD_L_PTR);
      atReference: Exit(LD_L_NATIVE);
      atGlobal: Exit(LD_G_PTR);
    end;
  end;

  if EDataType = dtPointer then
    EDataType := {$IFDEF CPUX64} dtUInt64 {$ELSE} dtUInt32 {$ENDIF};


  Result := _LoadInstructions[0, EDataType, ArgType];

  {$IFDEF DEBUG}
  if Result = VM_NOPE then
    AbortWork('Invalid LOAD instruction [Reg number: %d; Data type: %s; Argument type: %d]',
              [0, GetDataTypeName(EDataType), Integer(ArgType)]);
  {$ENDIF}
end;

function TVMTranslator.GetLDPTRInstruction(const Source: TILArgument): TVMCode;
var
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(Source.TypeInfo);
  // есил это ссылочный тип, загружаем его просто как значение
  if TypeInfo.DataTypeID in [dtPointer, dtString, dtAnsiString, dtDynArray,
                             dtOpenArray, dtClass, dtInterface] then
  begin
    Result := GetLDInstruction(Source);
    Exit;
  end;
  case Source.ArgumentType of
    atLocal: Result:= LD_L_PTR;
    atReference: Result := LD_L_NATIVE;    // если параметр передается по ссылке - загружаем указатель как есть
    atGlobal: Result:= LD_G_PTR;
    atImmConst: Result := LD_G_PTR;
  else
    raise Exception.Create('Unsupported argument type');
  end;
end;

function TVMTranslator.GetLDStrongPTRInstruction(const Source: TILArgument): TVMCode;
begin
  case Source.ArgumentType of
    atLocal: Result:= LD_L_PTR;
    atReference: Result := LD_L_NATIVE;    // если параметр передается по ссылке - загружаем указатель как есть
    atGlobal: Result:= LD_G_PTR;
  else
    raise Exception.Create('Unsupported argument type');
  end;
end;

function TVMTranslator.GetSTInstruction(TypeInfoOffset: TOffset; ArgType: TILArgumentType): TVMCode;
var
  EDataType: TDataTypeID;
  TypeInfo: PRTTIType;
begin
  TypeInfo := GetTypeInfo(TypeInfoOffset);

  if TypeInfo.DataTypeID in [dtRange, dtStaticArray, dtEnum, dtSet, dtRecord] then
  begin
    case TypeInfo.DataSize of
      1: EDataType := dtUInt8;
      2: EDataType := dtUInt16;
      4: EDataType := dtUInt32;
      8: EDataType := dtUInt64;
    else
      EDataType := TypeInfo.DataTypeID;
    end;
  end else
    EDataType := TypeInfo.DataTypeID;

  if EDataType = dtPointer then
    EDataType := {$IFDEF CPUX64} dtUInt64 {$ELSE} dtUInt32 {$ENDIF};

  Result := _StoreInstructions[EDataType, ArgType];
  {$IFDEF DEBUG}
  if Result = VM_NOPE then
    AbortWork('Invalid STORE instruction [Data type: %s; Argument type: %d]', [GetDataTypeName(EDataType), Integer(ArgType)]);
  {$ENDIF}
end;

function TVMTranslator.GetSTPTRInstruction(ArgType: TILArgumentType): TVMCode;
begin
  case ArgType of
    atLocal: Result := ST_L_NATIVE;
    atReference: Result := ST_R_NATIVE;
    atGlobal: Result := ST_G_NATIVE;
    else begin
      AbortWork('Invalid SETPTR instruction [Arg type: %d]', [Integer(ArgType)]);
      Result := VM_NOPE;
    end;
  end;
end;

function TVMTranslator.GetSTInstruction(const Arg: TILArgument): TVMCode;
begin
  Result := GetSTInstruction(Arg.TypeInfo, Arg.ArgumentType);
end;

function TVMTranslator.GetInstruction(ILCode: TILCode; TypeInfoOffset: TOffset): TVMCode;
var
  TypeInfo: PRTTIType;
  EDataType: TDataTypeID;
begin
  TypeInfo := GetTypeInfo(TypeInfoOffset);
  EDataType := TypeInfo.DataTypeID;
  if EDataType in [dtSet, dtRange] then begin
    case TypeInfo.DataSize of
      1: EDataType := dtUInt8;
      2: EDataType := dtUInt16;
      4: EDataType := dtUInt32;
      8: EDataType := dtUInt64;
    else
      raise Exception.CreateFmt('Data type size %d bytes is not supported', [TypeInfo.DataSize]);
    end;
  end;
  Result := _RegInstructions[ILCode, EDataType];
  {$IFDEF DEBUG}
  if Result = VM_NOPE then
    AbortWork('Invalid instruction "%s" [Data type: %s]', [UpperCase(GetILCodeName(ILCode)), GetDataTypeName(EDataType)]);
  {$ENDIF}
end;

procedure RegisterInstructions;
  procedure RegLoadInstruction(RegNum: Integer; const DataTypes: array of TDataTypeID; ArgumentType: TILArgumentType; MachineCode: TVMCode); overload;
  var
    i: Integer;
  begin
    {$IFDEF DEBUG}
    for i := 0 to Length(DataTypes) - 1 do
      if _LoadInstructions[RegNum, DataTypes[i], ArgumentType] <> VM_NOPE then
        AbortWork('Register instruction Error');
    {$ENDIF}
    for i := 0 to Length(DataTypes) - 1 do
      _LoadInstructions[RegNum, DataTypes[i], ArgumentType] := MachineCode;
  end;
  procedure RegStoreInstruction(const DataTypes: array of TDataTypeID; ArgumentType: TILArgumentType; MachineCode: TVMCode);
  var
    i: Integer;
  begin
    {$IFDEF DEBUG}
    for i := 0 to Length(DataTypes) - 1 do
      if _StoreInstructions[DataTypes[i], ArgumentType] <> VM_NOPE then
        AbortWork('Register instruction Error');
    {$ENDIF}
    for i := 0 to Length(DataTypes) - 1 do
      _StoreInstructions[DataTypes[i], ArgumentType] := MachineCode;
  end;
  procedure RegClearInstruction(const DataTypes: array of TDataTypeID; ArgumentType: TILArgumentType; MachineCode: TVMCode);
  var
    i: Integer;
  begin
    {$IFDEF DEBUG}
    for i := 0 to Length(DataTypes) - 1 do
      if _ClearInstructions[DataTypes[i], ArgumentType] <> VM_NOPE then
        AbortWork('Register instruction Error');
    {$ENDIF}
    for i := 0 to Length(DataTypes) - 1 do
      _ClearInstructions[DataTypes[i], ArgumentType] := MachineCode;
  end;

  procedure RegDestInstruction(const VMCodeClass: TVMCodeClass; const DataTypes: array of TDataTypeID; MachineCode: TVMCode);
  var
    i: Integer;
  begin
    {$IFDEF DEBUG}
    for i := 0 to Length(DataTypes) - 1 do
      if _DestInstructions[VMCodeClass, DataTypes[i]] <> VM_NOPE then
        AbortWork('Register instruction Error');
    {$ENDIF}
    for i := 0 to Length(DataTypes) - 1 do
      _DestInstructions[VMCodeClass, DataTypes[i]] := MachineCode;
  end;

  procedure RegInstruction(ILCode: TILCode; const DataTypes: array of TDataTypeID; MachineCode: TVMCode);
  var
    i: Integer;
  begin
    {$IFDEF DEBUG}
    for i := 0 to Length(DataTypes) - 1 do
      if _RegInstructions[ILCode, DataTypes[i]] <> VM_NOPE then
        AbortWork('Register instruction Error');
    {$ENDIF}
    for i := 0 to Length(DataTypes) - 1 do
      _RegInstructions[ILCode, DataTypes[i]] := MachineCode;
  end;
begin
  FillChar(_RegInstructions, SizeOf(_RegInstructions), char(VM_NOPE));
  FillChar(_LoadInstructions, SizeOf(_LoadInstructions), char(VM_NOPE));
  FillChar(_StoreInstructions, SizeOf(_StoreInstructions), char(VM_NOPE));
  FillChar(_ClearInstructions, SizeOf(_ClearInstructions), char(VM_NOPE));
  FillChar(_ConvertInstructions, SizeOf(_ConvertInstructions), char(VM_NOPE));
  FillChar(_DestInstructions, SizeOf(_DestInstructions), ord(VM_NOPE));
  FillChar(_LDS, SizeOf(_LDS), char(VM_NOPE));
  ////////////////////////////////////////////////////////////////
  // LAOD
  ////////////////////////////////////////////////////////////////
  //== загрузка констант ================================================================================================
  RegLoadInstruction(0, [dtUInt8, dtUInt16, dtUInt32, dtBoolean, dtAnsiChar, dtChar], atImmConst, LD_C_U32);
  RegLoadInstruction(0, [dtInt8, dtInt16, dtInt32], atImmConst, LD_C_I32);
  RegLoadInstruction(0, [dtInt64, dtUInt64], atImmConst, LD_C_I64);
  RegLoadInstruction(0, [dtFloat32], atImmConst, LD_C_F32);
  RegLoadInstruction(0, [dtFloat64], atImmConst, LD_C_F64);
  RegLoadInstruction(0, [dtString, dtStaticArray, dtDynArray, dtOpenArray, dtRecord, dtGuid], atImmConst, LD_G_PTR);
  RegLoadInstruction(0, [dtNativeInt, dtNativeUInt, dtClass, dtInterface, dtClassOf], atImmConst, LD_C_NATIVE);
  //== загрузка локальных переменных ====================================================================================
  RegLoadInstruction(0, [dtInt8], atLocal, LD_L_I8);
  RegLoadInstruction(0, [dtUInt8, dtAnsiChar, dtBoolean], atLocal, LD_L_U8);
  RegLoadInstruction(0, [dtInt16], atLocal, LD_L_I16);
  RegLoadInstruction(0, [dtUInt16, dtChar], atLocal, LD_L_U16);
  RegLoadInstruction(0, [dtInt32, dtUInt32], atLocal, LD_L_I32);
  RegLoadInstruction(0, [dtInt64, dtUInt64], atLocal, LD_L_I64);
  RegLoadInstruction(0, [dtFloat32], atLocal, LD_L_F32);
  RegLoadInstruction(0, [dtFloat64], atLocal, LD_L_F64);
  RegLoadInstruction(0, [dtString, dtAnsiString], atLocal, LD_L_NATIVE);
  RegLoadInstruction(0, [dtVariant, dtStaticArray, dtRecord, dtGuid, dtSet], atLocal, LD_L_PTR);
  RegLoadInstruction(0, [dtNativeInt, dtNativeUInt, dtDynArray, dtOpenArray, dtProcType, dtInterface, dtClass, dtClassOf, dtPointer, dtWeakRef], atLocal, LD_L_NATIVE);
  //== загрузка локальных ссылок  =======================================================================================
  RegLoadInstruction(0, [dtInt8], atReference, LD_R_I8);
  RegLoadInstruction(0, [dtUInt8, dtAnsiChar, dtBoolean], atReference, LD_R_U8);
  RegLoadInstruction(0, [dtInt16], atReference, LD_R_I16);
  RegLoadInstruction(0, [dtUInt16, dtChar], atReference, LD_R_U16);
  RegLoadInstruction(0, [dtInt32, dtUInt32], atReference, LD_R_I32);
  RegLoadInstruction(0, [dtInt64, dtUInt64], atReference, LD_R_I64);
  RegLoadInstruction(0, [dtFloat32], atReference, LD_R_F32);
  RegLoadInstruction(0, [dtFloat64], atReference, LD_R_I64);
  RegLoadInstruction(0, [dtString, dtAnsiString], atReference, LD_R_NATIVE);
  RegLoadInstruction(0, [dtNativeInt, dtNativeUInt, dtStaticArray, dtDynArray, dtOpenArray, dtRecord, dtGuid, dtSet, dtInterface, dtClass, dtClassOf, dtPointer, dtWeakRef], atReference, LD_R_NATIVE);
  RegLoadInstruction(0, [dtVariant], atReference, LD_L_NATIVE);
  //== загрузка глобальных значений  ====================================================================================
  RegLoadInstruction(0, [dtInt8], atGlobal, LD_G_I8);
  RegLoadInstruction(0, [dtUInt8, dtAnsiChar, dtBoolean], atGlobal, LD_G_U8);
  RegLoadInstruction(0, [dtInt16], atGlobal, LD_G_I16);
  RegLoadInstruction(0, [dtUInt16, dtChar], atGlobal, LD_G_U16);
  RegLoadInstruction(0, [dtInt32, dtUInt32], atGlobal, LD_G_I32);
  RegLoadInstruction(0, [dtInt64, dtUInt64], atGlobal, LD_G_I64);
  RegLoadInstruction(0, [dtFloat32], atGlobal, LD_G_F32);
  RegLoadInstruction(0, [dtFloat64], atGlobal, LD_G_F64);
  RegLoadInstruction(0, [dtString, dtAnsiString], atGlobal, LD_G_NATIVE);
  RegLoadInstruction(0, [dtVariant, dtStaticArray, dtRecord, dtGuid, dtSet], atGlobal, LD_G_PTR);
  RegLoadInstruction(0, [dtNativeInt, dtNativeUInt, dtDynArray, dtProcType, dtInterface, dtClass, dtClassOf, dtPointer, dtWeakRef], atGlobal, LD_G_NATIVE);
  //== загрузка значений с разименованием  ==============================================================================
  RegLoadInstruction(0, [dtInt8], atField, LD_D_I8);
  RegLoadInstruction(0, [dtUInt8, dtAnsiChar, dtBoolean], atField, LD_D_U8);
  RegLoadInstruction(0, [dtInt16], atField, LD_D_I16);
  RegLoadInstruction(0, [dtUInt16, dtChar], atField, LD_D_U16);
  RegLoadInstruction(0, [dtInt32, dtUInt32], atField, LD_D_I32);
  RegLoadInstruction(0, [dtInt64, dtUInt64], atField, LD_D_I64);
  RegLoadInstruction(0, [dtFloat32], atField, LD_D_F32);
  RegLoadInstruction(0, [dtFloat64], atField, LD_D_I64);
  RegLoadInstruction(0, [dtString, dtAnsiString], atField, LD_D_NATIVE);
  RegLoadInstruction(0, [dtNativeInt, dtNativeUInt, dtVariant, dtStaticArray, dtDynArray, dtOpenArray, dtRecord, dtGuid, dtSet, dtInterface, dtClass, dtClassOf, dtPointer, dtWeakRef], atField, LD_D_NATIVE);
  ////////////////////////////////////////////////////////////////
  // CONVERT
  ////////////////////////////////////////////////////////////////
  RegConvertInstruction(0, dtFloat32, dtInt8, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat32, dtInt16, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat32, dtInt32, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat32, dtInt64, CNV_F64_S64);

  RegConvertInstruction(0, dtFloat32, dtUInt8, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat32, dtUInt16, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat32, dtUInt32, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat32, dtUInt64, CNV_F64_U64);

  RegConvertInstruction(0, dtFloat64, dtInt8, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat64, dtInt16, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat64, dtInt32, CNV_F64_S32);
  RegConvertInstruction(0, dtFloat64, dtInt64, CNV_F64_S64);

  RegConvertInstruction(0, dtFloat64, dtUInt8, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat64, dtUInt16, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat64, dtUInt32, CNV_F64_U32);
  RegConvertInstruction(0, dtFloat64, dtUInt64, CNV_F64_U64);
  ////////////////////////////////////////////////////////////////
  // STORE
  ////////////////////////////////////////////////////////////////
  //== сохранение локальных значений  ==============================================================================================
  RegStoreInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atLocal, ST_L_I8);
  RegStoreInstruction([dtInt16, dtUInt16, dtChar], atLocal, ST_L_I16);
  RegStoreInstruction([dtInt32, dtUInt32], atLocal, ST_L_I32);
  RegStoreInstruction([dtInt64, dtUInt64], atLocal, ST_L_I64);
  RegStoreInstruction([dtFloat32], atLocal, ST_L_F32);
  RegStoreInstruction([dtFloat64], atLocal, ST_L_I64);
  RegStoreInstruction([dtString, dtAnsiString], atLocal, ST_L_NATIVE);
  RegStoreInstruction([dtNativeInt, dtNativeUInt, dtDynArray, dtOpenArray, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atLocal, ST_L_NATIVE);
  RegStoreInstruction([dtVariant], atLocal, ST_L_VAR);
  //== сохранение локальных значений по ссылке =====================================================================================
  RegStoreInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atReference, ST_R_I8);
  RegStoreInstruction([dtInt16, dtUInt16, dtChar], atReference, ST_R_I16);
  RegStoreInstruction([dtInt32, dtUInt32], atReference, ST_R_I32);
  RegStoreInstruction([dtInt64, dtUInt64], atReference, ST_R_I64);
  RegStoreInstruction([dtFloat32], atReference, ST_R_F32);
  RegStoreInstruction([dtFloat64], atReference, ST_R_I64);
  RegStoreInstruction([dtString, dtAnsiString], atReference, ST_R_NATIVE);
  RegStoreInstruction([dtNativeInt, dtNativeUInt, dtStaticArray, dtDynArray, dtRecord, dtGuid, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atReference, ST_R_NATIVE);
  //== сохранение глобальных значений  =============================================================================================
  RegStoreInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atGlobal, ST_G_I8);
  RegStoreInstruction([dtInt16, dtUInt16, dtChar], atGlobal, ST_G_I16);
  RegStoreInstruction([dtInt32, dtUInt32], atGlobal, ST_G_I32);
  RegStoreInstruction([dtInt64, dtUInt64], atGlobal, ST_G_I64);
  RegStoreInstruction([dtFloat32], atGlobal, ST_G_F32);
  RegStoreInstruction([dtFloat64], atGlobal, ST_G_I64);
  RegStoreInstruction([dtString, dtAnsiString], atGlobal, ST_G_NATIVE);
  RegStoreInstruction([dtNativeInt, dtNativeUInt, dtDynArray, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atGlobal, ST_G_NATIVE);
  RegStoreInstruction([dtVariant], atGlobal, ST_G_VAR);
  //== сохранение локальных значений по ссылке =====================================================================================
  RegStoreInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atField, ST_D_I8);
  RegStoreInstruction([dtInt16, dtUInt16, dtChar], atField, ST_D_I16);
  RegStoreInstruction([dtInt32, dtUInt32], atField, ST_D_I32);
  RegStoreInstruction([dtInt64, dtUInt64], atField, ST_D_I64);
  RegStoreInstruction([dtFloat32], atField, ST_D_F32);
  RegStoreInstruction([dtFloat64], atField, ST_D_I64);
  RegStoreInstruction([dtString, dtAnsiString], atField, ST_D_NATIVE);
  RegStoreInstruction([dtNativeInt, dtNativeUInt, dtStaticArray, dtDynArray, dtRecord, dtGuid, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atField, ST_D_NATIVE);
  ////////////////////////////////////////////////////////////////
  // CLEAR
  ////////////////////////////////////////////////////////////////
  //== очистка локальных переменных  ==============================================================================================
  RegClearInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atLocal, CLR_L_I8);
  RegClearInstruction([dtInt16, dtUInt16, dtChar], atLocal, CLR_L_I16);
  RegClearInstruction([dtInt32, dtUInt32], atLocal, CLR_L_I32);
  RegClearInstruction([dtInt64, dtUInt64], atLocal, CLR_L_I64);
  RegClearInstruction([dtFloat32], atLocal, CLR_L_F32);
  RegClearInstruction([dtFloat64], atLocal, CLR_L_F64);
  RegClearInstruction([dtString, dtAnsiString], atLocal, CLR_L_NATIVE);
  RegClearInstruction([dtNativeInt, dtNativeUInt, dtDynArray, dtOpenArray, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atLocal, CLR_L_NATIVE);
  RegClearInstruction([dtVariant], atLocal, CLR_L_NATIVE);
  //== очистка локальных значений по ссылке =====================================================================================
  RegClearInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atReference, CLR_R_I8);
  RegClearInstruction([dtInt16, dtUInt16, dtChar], atReference, CLR_R_I16);
  RegClearInstruction([dtInt32, dtUInt32], atReference, CLR_R_I32);
  RegClearInstruction([dtInt64, dtUInt64], atReference, CLR_R_I64);
  RegClearInstruction([dtFloat32], atReference, CLR_R_F32);
  RegClearInstruction([dtFloat64], atReference, CLR_R_F64);
  RegClearInstruction([dtString, dtAnsiString], atReference, CLR_R_NATIVE);
  RegClearInstruction([dtNativeInt, dtNativeUInt, dtStaticArray, dtDynArray, dtRecord, dtGuid, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atReference, CLR_R_NATIVE);
  RegClearInstruction([dtVariant], atReference, CLR_R_NATIVE);
  //== очистка глобальных переменных  ==============================================================================================
  RegClearInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atGlobal, CLR_G_I8);
  RegClearInstruction([dtInt16, dtUInt16, dtChar], atGlobal, CLR_G_I16);
  RegClearInstruction([dtInt32, dtUInt32], atGlobal, CLR_G_I32);
  RegClearInstruction([dtInt64, dtUInt64], atGlobal, CLR_G_I64);
  RegClearInstruction([dtFloat32], atGlobal, CLR_G_F32);
  RegClearInstruction([dtFloat64], atGlobal, CLR_G_F64);
  RegClearInstruction([dtString, dtAnsiString], atGlobal, CLR_G_NATIVE);
  RegClearInstruction([dtNativeInt, dtNativeUInt, dtDynArray, dtOpenArray, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atGlobal, CLR_G_NATIVE);
  RegClearInstruction([dtVariant], atGlobal, CLR_G_NATIVE);
  //== очистка полей структуры =====================================================================================
  RegClearInstruction([dtInt8, dtUInt8, dtAnsiChar, dtBoolean], atField, CLR_D_I8);
  RegClearInstruction([dtInt16, dtUInt16, dtChar], atField, CLR_D_I16);
  RegClearInstruction([dtInt32, dtUInt32], atField, CLR_D_I32);
  RegClearInstruction([dtInt64, dtUInt64], atField, CLR_D_I64);
  RegClearInstruction([dtFloat32], atField, CLR_D_F32);
  RegClearInstruction([dtFloat64], atField, CLR_D_F64);
  RegClearInstruction([dtString, dtAnsiString], atField, CLR_D_I32);
  RegClearInstruction([dtNativeInt, dtNativeUInt, dtStaticArray, dtDynArray, dtRecord, dtGuid, dtProcType, dtClass, dtInterface, dtClassOf, dtPointer, dtWeakRef], atField, CLR_D_I32);
  RegClearInstruction([dtVariant], atField, CLR_D_I32);
  ////////////////////////////////////////////////////////////////
  // ADD
  ////////////////////////////////////////////////////////////////
  RegInstruction(icAdd, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtNativeInt, dtChar, dtAnsiChar], ADD_I32);
  RegInstruction(icAdd, [dtInt64, dtUInt64], ADD_I64);
  RegInstruction(icAdd, [dtFloat32], ADD_F64);
  RegInstruction(icAdd, [dtFloat64], ADD_F64);
  RegInstruction(icAdd, [dtString], ADD_USTR);
  RegInstruction(icAdd, [dtAnsiString], ADD_ASTR);
  RegInstruction(icAdd, [dtPointer], ADD_NATIVE);
  ////////////////////////////////////////////////////////////////
  // SUB
  ////////////////////////////////////////////////////////////////
  RegInstruction(icSub, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtNativeInt, dtChar, dtAnsiChar], SUB_I32);
  RegInstruction(icSub, [dtInt64, dtUInt64], SUB_I64);
  RegInstruction(icSub, [dtFloat32], SUB_F64);
  RegInstruction(icSub, [dtFloat64], SUB_F64);
  RegInstruction(icSub, [dtPointer], SUB_NATIVE);
  ////////////////////////////////////////////////////////////////
  // MUL
  ////////////////////////////////////////////////////////////////
  RegInstruction(icMul, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], MUL_I32);
  RegInstruction(icMul, [dtInt64, dtUInt64], MUL_I64);
  RegInstruction(icMul, [dtFloat32], MUL_F64);
  RegInstruction(icMul, [dtFloat64], MUL_F64);
  ////////////////////////////////////////////////////////////////
  // IDIV
  ////////////////////////////////////////////////////////////////
  RegInstruction(icIntDiv, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], IDIV_I32);
  RegInstruction(icIntDiv, [dtInt64, dtUInt64], IDIV_I64);
  ////////////////////////////////////////////////////////////////
  // IMOD
  ////////////////////////////////////////////////////////////////
  RegInstruction(icModDiv, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], IMOD_I32);
  RegInstruction(icModDiv, [dtInt64, dtUInt64], IMOD_I64);
  ////////////////////////////////////////////////////////////////
  // DIV
  ////////////////////////////////////////////////////////////////
  RegInstruction(icDiv, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], DIV_I32);
  RegInstruction(icDiv, [dtInt64, dtUInt64], DIV_I64);
  RegInstruction(icDiv, [dtFloat32], DIV_F64);
  RegInstruction(icDiv, [dtFloat64], DIV_F64);
  ////////////////////////////////////////////////////////////////
  // CMP
  ////////////////////////////////////////////////////////////////
  RegInstruction(icCmp, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtAnsiChar, dtChar, dtBoolean, dtEnum], CMP_I32);
  RegInstruction(icCmp, [dtInt64, dtUInt64], CMP_I64);
  RegInstruction(icCmp, [dtNativeInt, dtNativeUInt, dtPointer, dtClass, dtInterface, dtWeakRef], CMP_NATIVE);
  RegInstruction(icCmp, [dtFloat32], CMP_F64);
  RegInstruction(icCmp, [dtFloat64], CMP_F64);
  RegInstruction(icCmp, [dtString], CMP_USTR);
  RegInstruction(icCmp, [dtAnsiString], CMP_ASTR);
  RegInstruction(icCmp, [dtVariant], CMP_VAR);
  RegInstruction(icTest, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtAnsiChar, dtChar, dtBoolean], CMP_TEST32);
  RegInstruction(icTest, [dtInt64, dtUInt64], CMP_TEST64);
  ////////////////////////////////////////////////////////////////
  // NEG
  ////////////////////////////////////////////////////////////////
  RegInstruction(icNeg, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], NEG_I32);
  RegInstruction(icNeg, [dtInt64, dtUInt64], NEG_I64);
  RegInstruction(icNeg, [dtFloat32], NEG_F64);
  RegInstruction(icNeg, [dtFloat64], NEG_F64);
  ////////////////////////////////////////////////////////////////
  // BIN_AND
  ////////////////////////////////////////////////////////////////
  RegInstruction(icAnd, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], BIN_AND32);
  RegInstruction(icAnd, [dtInt64, dtUInt64], BIN_AND64);
  ////////////////////////////////////////////////////////////////
  // BIN_OR
  ////////////////////////////////////////////////////////////////
  RegInstruction(icOr, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], BIN_OR32);
  RegInstruction(icOr, [dtInt64, dtUInt64], BIN_OR64);
  ////////////////////////////////////////////////////////////////
  // BIN_XOR
  ////////////////////////////////////////////////////////////////
  RegInstruction(icXor, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtBoolean], BIN_XOR32);
  RegInstruction(icXor, [dtInt64, dtUInt64], BIN_XOR64);
  ////////////////////////////////////////////////////////////////
  // BIN_NOT
  ////////////////////////////////////////////////////////////////
  RegInstruction(icNot, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32, dtBoolean], BIN_NOT32);
  RegInstruction(icNot, [dtInt64, dtUInt64], BIN_NOT64);
  ////////////////////////////////////////////////////////////////
  // BIN_SHL
  ////////////////////////////////////////////////////////////////
  RegInstruction(icShl, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], BIN_SHL32);
  RegInstruction(icShl, [dtInt64, dtUInt64], BIN_SHL64);
  ////////////////////////////////////////////////////////////////
  // BIN_SHR
  ////////////////////////////////////////////////////////////////
  RegInstruction(icShr, [dtInt8, dtUInt8, dtInt16, dtUInt16, dtInt32, dtUInt32], BIN_SHR32);
  RegInstruction(icShr, [dtInt64, dtUInt64], BIN_SHR64);
  ////////////////////////////////////////////////////////////////
  // UNIQUE
  ////////////////////////////////////////////////////////////////
  RegInstruction(icUnique, [dtString], UNIQUE_USTR);
  RegInstruction(icUnique, [dtAnsiString], UNIQUE_ASTR);
  ////////////////////////////////////////////////////////////////
  // RDREF
  ////////////////////////////////////////////////////////////////
  RegInstruction(icReadDRef, [dtInt8, dtUInt8, dtBoolean, dtAnsiChar], LD_D_I8);
  RegInstruction(icReadDRef, [dtInt16, dtUInt16, dtChar], LD_D_I16);
  RegInstruction(icReadDRef, [dtInt32, dtUInt32], LD_D_I32);
  RegInstruction(icReadDRef, [dtFloat32], LD_D_F32);
  RegInstruction(icReadDRef, [dtInt64, dtUInt64, dtFloat64], LD_D_I64);
  RegInstruction(icReadDRef, [dtDynArray, dtString, dtAnsiString, dtClass, dtClassOf, dtInterface], LD_D_NATIVE);
  ////////////////////////////////////////////////////////////////
  // WDREF
  ////////////////////////////////////////////////////////////////
  RegInstruction(icWriteDRef, [dtInt8, dtUInt8, dtBoolean, dtAnsiChar], ST_D_I8);
  RegInstruction(icWriteDRef, [dtInt16, dtUInt16, dtChar], ST_D_I16);
  RegInstruction(icWriteDRef, [dtInt32, dtUInt32], ST_D_I32);
  RegInstruction(icWriteDRef, [dtFloat32], ST_D_F32);
  RegInstruction(icWriteDRef, [dtInt64, dtUInt64, dtFloat64], ST_D_I64);
  RegInstruction(icWriteDRef, [dtDynArray, dtString, dtAnsiString, dtClass, dtClassOf, dtInterface], ST_D_NATIVE);
  ////////////////////////////////////////////////////////////////
  // ARRAY LENGTH
  ////////////////////////////////////////////////////////////////
  RegInstruction(icArrayLength, [dtString, dtAnsiString], STR_LENGTH);
  RegInstruction(icArrayLength, [dtDynArray], ARRAY_LENGTH);
  /////////////////////////////////////////////////////////////////
  //===================================================================================================
  RegDestInstruction(vmIncRef, [dtString, dtAnsiString], STR_INCREF);
  RegDestInstruction(vmIncRef, [dtDynArray], ARRAY_INCREF);
  RegDestInstruction(vmIncRef, [dtClass], OBJ_INCREF);
  RegDestInstruction(vmIncRef, [dtWeakRef], WEAK_INCREF);
  RegDestInstruction(vmIncRef, [dtInterface], OBJ_INCREF);
  //===================================================================================================
  RegDestInstruction(vmDecRef, [dtClass], OBJ_DECREF);
  RegDestInstruction(vmDecRef, [dtWeakRef], WEAK_DECREF);
  RegDestInstruction(vmDecRef, [dtInterface], OBJ_DECREF);
  RegDestInstruction(vmDecRef, [dtDynArray], ARRAY_DECREF);
  RegDestInstruction(vmDecRef, [dtString, dtAnsiString], STR_DECREF);
  RegDestInstruction(vmDecRef, [dtVariant], VAR_RELEASE);
  //===================================================================================================
  _LDS[LD_L_I8] := ST_L_I8;
  _LDS[LD_L_I16] := ST_L_I16;
  _LDS[LD_L_I32] := ST_L_I32;
  _LDS[LD_L_I64] := ST_L_I64;
  _LDS[LD_L_F32] := ST_L_F32;

  _LDS[LD_R_I8] := ST_R_I8;
  _LDS[LD_R_I16] := ST_R_I16;
  _LDS[LD_R_I32] := ST_R_I32;
  _LDS[LD_R_I64] := ST_R_I64;
  _LDS[LD_R_F32] := ST_R_F32;

  _LDS[LD_G_I8] := ST_G_I8;
  _LDS[LD_G_I16] := ST_G_I16;
  _LDS[LD_G_I32] := ST_G_I32;
  _LDS[LD_G_I64] := ST_G_I64;
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; const Arg: TVMCodeArg);
begin
  WriteInstruction(CND, ILCode, VM_R0, VM_R0, VM_R0, Arg);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst: TVMRegister; const Arg: TVMCodeArg);
begin
  WriteInstruction(CND, ILCode, Dst, VM_R0, VM_R0, Arg);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src: TVMRegister; const Arg: TVMCodeArg);
begin
  WriteInstruction(CND, ILCode, Dst, Src, VM_R0, Arg);
end;

procedure TVMTranslator.WriteInstruction(CND: TILCondition; ILCode: TVMCode; Dst, Src1, Src2: TVMRegister; const Arg: TVMCodeArg);
var
  VMCode: NativeUInt;
  DataCnt: Integer;
begin
  DataCnt := Arg.GetDataCnt(Self);
  VMCode := MakeInstruction(ILCode, Dst, Src1, Src2, CND, DataCnt, VMARG_VAR);

  IMG.WriteNativeUInt(VMCode);

  IMG.WriteNativeUInt(NativeUInt(Arg));
  if DataCnt = 2 then
    IMG.WriteNativeUInt(0);
end;

initialization
  RegisterInstructions;

end.
