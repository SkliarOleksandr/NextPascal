unit WASM.Writer;

interface

uses System.SysUtils, System.Classes, NPCompiler.Utils, Generics.Collections;

type
  TWasmOpCode =
  (
    op_unreachable = $00,
    op_nop = $01,
    op_block = $02,
    op_loop = $03,
    op_if = $04,
    op_else = $05,
    op_end = $0B,
    op_br = $0C,
    op_br_if = $0D,
    op_br_table = $0E,
    op_return = $0F,
    op_call = $10,
    op_call_indirect = $11,
    op_Drop = $1A,
    op_Select = $1B,
    op_Get_local = $20,
    op_Set_local = $21,
    op_Tee_local = $22,
    op_Get_global = $23,
    op_Set_global = $24,
    op_i32_load = $28,
    op_i64_load = $29,
    op_f32_load = $2A,
    op_f64_load = $2B,
    op_i32_load8_s = $2C,
    op_i32_load8_u = $2D,
    op_i32_load16_s = $2E,
    op_i32_load16_u = $2F,
    op_i64_load8_s = $30,
    op_i64_load8_u = $31,
    op_i64_load16_s = $32,
    op_i64_load16_u = $33,
    op_i64_load32_s = $34,
    op_i64_load32_u = $35,
    op_i32_store = $36,
    op_i64_store = $37,
    op_f32_store = $38,
    op_f64_store = $39,
    op_i32_store8 = $3A,
    op_i32_store16 = $3B,
    op_i64_store8 = $3C,
    op_i64_store16 = $3D,
    op_i64_store32 = $3E,
    op_current_memory = $3F,
    op_grow_memory = $40,
    op_i32_const = $41,
    op_i64_const = $42,
    op_f32_const = $43,
    op_f64_const = $44,
    op_i32_eqz = $45,
    op_i32_eq = $46,
    op_i32_ne = $47,
    op_i32_lt_s = $48,
    op_i32_lt_u = $49,
    op_i32_gt_s = $4A,
    op_i32_gt_u = $4B,
    op_i32_le_s = $4C,
    op_i32_le_u = $4D,
    op_i32_ge_s = $4E,
    op_i32_ge_u = $4F,
    op_i64_eqz = $50,
    op_i64_eq = $51,
    op_i64_ne = $52,
    op_i64_lt_s = $53,
    op_i64_lt_u = $54,
    op_i64_gt_s = $55,
    op_i64_gt_u = $56,
    op_i64_le_s = $57,
    op_i64_le_u = $58,
    op_i64_ge_s = $59,
    op_i64_ge_u = $5A,
    op_f32_eq = $5B,
    op_f32_ne = $5C,
    op_f32_lt = $5D,
    op_f32_gt = $5E,
    op_f32_le = $5F,
    op_f32_ge = $60,
    op_f64_eq = $61,
    op_f64_ne = $62,
    op_f64_lt = $63,
    op_f64_gt = $64,
    op_f64_le = $65,
    op_f64_ge = $66,
    op_i32_clz	= $67,
    op_i32_ctz	= $68,
    op_i32_popcnt	= $69,
    op_i32_add	= $6a,
    op_i32_sub	= $6b,
    op_i32_mul	= $6c,
    op_i32_div_s	= $6d,
    op_i32_div_u	= $6e,
    op_i32_rem_s	= $6f,
    op_i32_rem_u	= $70,
    op_i32_and	= $71,
    op_i32_or	= $72,
    op_i32_xor	= $73,
    op_i32_shl	= $74,
    op_i32_shr_s	= $75,
    op_i32_shr_u	= $76,
    op_i32_rotl	= $77,
    op_i32_rotr	= $78,
    op_i64_clz	= $79,
    op_i64_ctz	= $7a,
    op_i64_popcnt	= $7b,
    op_i64_add	= $7c,
    op_i64_sub	= $7d,
    op_i64_mul	= $7e,
    op_i64_div_s	= $7f,
    op_i64_div_u	= $80,
    op_i64_rem_s	= $81,
    op_i64_rem_u	= $82,
    op_i64_and	= $83,
    op_i64_or	= $84,
    op_i64_xor	= $85,
    op_i64_shl	= $86,
    op_i64_shr_s	= $87,
    op_i64_shr_u	= $88,
    op_i64_rotl	= $89,
    op_i64_rotr	= $8a,
    op_f32_abs	= $8b,
    op_f32_neg	= $8c,
    op_f32_ceil	= $8d,
    op_f32_floor	= $8e,
    op_f32_trunc	= $8f,
    op_f32_nearest	= $90,
    op_f32_sqrt	= $91,
    op_f32_add	= $92,
    op_f32_sub	= $93,
    op_f32_mul	= $94,
    op_f32_div	= $95,
    op_f32_min	= $96,
    op_f32_max	= $97,
    op_f32_copysign	= $98,
    op_f64_abs	= $99,
    op_f64_neg	= $9a,
    op_f64_ceil	= $9b,
    op_f64_floor	= $9c,
    op_f64_trunc	= $9d,
    op_f64_nearest	= $9e,
    op_f64_sqrt	= $9f,
    op_f64_add	= $a0,
    op_f64_sub	= $a1,
    op_f64_mul	= $a2,
    op_f64_div	= $a3,
    op_f64_min	= $a4,
    op_f64_max	= $a5,
    op_f64_copysign	= $a6,
    _wasm_op_future
  );

  TWASMDataType =
  (
    dt_i32 = $7F,
    dt_i64 = $7E,
    dt_f32 = $7D,
    dt_f64 = $7C
  );

  TWASMExternalKind =
  (
    exFunction = 0, // indicating a Function import or definition
    exTable    = 1, // indicating a Table import or definition
    exMemory   = 2, // indicating a Memory import or definition
    exGlobal   = 3  // indicating a Global import or definition
  );


  TWASMDataTypes = array of TWASMDataType;

const
  wasm_body_end = $0B;

type
  IWASMTypeSection = interface;
  IWASMImportSection = interface;
  IWASMFunctionSection = interface;
  IWASMTableSection = interface;
  IWASMMemorySection = interface;
  IWASMGlobalSection = interface;
  IWASMExportSection = interface;
  IWASMStartSection = interface;
  IWASMElementSection = interface;
  IWASMCodeSection = interface;
  IWASMDataSection = interface;
  IWASMCodeWriter = interface;

  TWASMIndex = Integer;

  IWASMWriter = interface
    ['{6E53EFE3-7CD7-4519-A87A-8FB3D95DC25C}']
    procedure SaveToStream(Stream: TStream);
    // sections
    function TypesSection: IWASMTypeSection;
    function ImportSection: IWASMImportSection;
    function FunctionSection: IWASMFunctionSection; // The function section declares the signatures of all functions in the module (their definitions appear in the code section)
    function TableSection: IWASMTableSection;
    function MemorySection: IWASMMemorySection;
    function GlobalSection: IWASMGlobalSection;
    function ExportSection: IWASMExportSection;
    function StartSection: IWASMStartSection;
    function ElementSection: IWASMElementSection;
    function CodeSection: IWASMCodeSection;
    function DataSection: IWASMDataSection;
  end;

  IWASMTypeSection = interface
    function AddSignature(const Params: TWASMDataTypes): TWASMIndex; overload;
    function AddSignature(const Params: TWASMDataTypes; ResultType: TWASMDataType): TWASMIndex; overload;
    function GetTypeSectionEntriesCount: Integer;
    property Count: Integer read GetTypeSectionEntriesCount;
  end;

  IWASMImportSection = interface
    function AddImportFunction(const LibName, FuncName: string; SignatureIdx: Integer): TWASMIndex;
    function GetImportSectionEntriesCount: Integer;
    property Count: Integer read GetImportSectionEntriesCount;
  end;

  IWASMFunctionSection = interface
    procedure AddFunctionSignatureIndex(SignIndex: TWASMIndex);
    function GetFunctionSectionEntriesCount: Integer;
    property Count: Integer read GetFunctionSectionEntriesCount;
  end;

  IWASMTableSection = interface
    procedure AddTable;
    function GetTableSectionEntriesCount: Integer;
    property Count: Integer read GetTableSectionEntriesCount;
  end;

  IWASMMemorySection = interface
    procedure AddMemory;
    function GetMemorySectionEntriesCount: Integer;
    property Count: Integer read GetMemorySectionEntriesCount;
  end;

  IWASMGlobalSection = interface
  end;

  IWASMExportSection = interface
    function AddExportFunction(const Name: string; SignatureIdx: Integer; ExternalKind: Byte): TWASMIndex;
    function GetExportSectionEntriesCount: Integer;
    property Count: Integer read GetExportSectionEntriesCount;
  end;

  IWASMStartSection = interface
  end;

  IWASMElementSection = interface
  end;

  IWASMCodeSection = interface
    function AddFunctionBody: IWASMCodeWriter;
  end;

  IWASMDataSection = interface
  end;

  IWASMCodeWriter = interface
    procedure Write_I32_CONST(const Data: Int32);
    procedure Write_I64_CONST(const Data: Int64);
    procedure Write_F32_CONST(const Data: Int32);
    procedure Write_F64_CONST(const Data: Int64);
    procedure Write_CALL(Index: UInt32);
    procedure Write_GET_LOCAL(Index: UInt32);
    procedure Write_GET_GLOBAL(Index: UInt32);
    procedure WriteOpCode(OpCode: TWasmOpCode);
    procedure Write_VarUInt32(Data: UInt32);
    procedure WriteEnd;
    function Data: TStream;
  end;

  TWASMFuncBody = class(TInterfacedObject, IWASMCodeWriter)
  private
    FStream: TMemoryStream;
  protected
    function Data: TStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Write_CALL(Index: UInt32);
    procedure Write_I32_CONST(const Data: Int32);
    procedure Write_I64_CONST(const Data: Int64);
    procedure Write_F32_CONST(const Data: Int32);
    procedure Write_F64_CONST(const Data: Int64);
    procedure Write_GET_LOCAL(Index: UInt32);
    procedure Write_GET_GLOBAL(Index: UInt32);
    procedure WriteOpCode(OpCode: TWasmOpCode);
    procedure Write_VarUInt32(Data: UInt32);
    procedure WriteEnd;
  end;

  TWASMWriter = class(TInterfacedObject, IWASMWriter,
                                         IWASMTypeSection,
                                         IWASMImportSection,
                                         IWASMFunctionSection,
                                         IWASMTableSection,
                                         IWASMMemorySection,
                                         IWASMGlobalSection,
                                         IWASMExportSection,
                                         IWASMStartSection,
                                         IWASMElementSection,
                                         IWASMCodeSection,
                                         IWASMDataSection)
  private
    FTypeSectionECount: Integer;
    FTypeSection: TMemoryStream;
    FImportSectionECount: Integer;
    FImportSection: TMemoryStream;
    FFunctionSectionECount: Integer;
    FFunctionSection: TMemoryStream;
    FTableSectionECount: Integer;
    FTableSection: TMemoryStream;
    FMemorySectionECount: Integer;
    FMemorySection: TMemoryStream;
    FGlobalSectionECount: Integer;
    FGlobalSection: TMemoryStream;
    FExportSectionECount: Integer;
    FExportSection: TMemoryStream;
    FStartSectionECount: Integer;
    FStartSection: TMemoryStream;
    FElementSectionECount: Integer;
    FElementSection: TMemoryStream;
    FDataSectionECount: Integer;
    FDataSection: TMemoryStream;
    FFuncBodies: TList<IWASMCodeWriter>;
  protected
    procedure WriteSection(Stream: TStream; SectionID: Byte; EntriesCount: Integer; Section: TStream);
    procedure WriteCodeSection(Stream: TStream);
    procedure WriteParamTypes(const Params: TWASMDataTypes);
    function AddSignature(const Params: TWASMDataTypes): TWASMIndex; overload;
    function AddSignature(const Params: TWASMDataTypes; ResultType: TWASMDataType): TWASMIndex; overload;
    procedure AddFunctionSignatureIndex(SignIndex: TWASMIndex);
    procedure AddTable;
    procedure AddMemory;
    function AddExportFunction(const Name: string; SignatureIdx: Integer; ExternalKind: Byte): TWASMIndex;
    function AddFunctionBody: IWASMCodeWriter;
    function AddImportFunction(const LibName, FuncName: string; SignatureIdx: Integer): TWASMIndex;
    function GetTypeSectionEntriesCount: Integer;
    function GetImportSectionEntriesCount: Integer;
    function GetFunctionSectionEntriesCount: Integer;
    function GetTableSectionEntriesCount: Integer;
    function GetMemorySectionEntriesCount: Integer;
    function GetExportSectionEntriesCount: Integer;
  public
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    // sections
    function TypesSection: IWASMTypeSection;
    function ImportSection: IWASMImportSection;
    function FunctionSection: IWASMFunctionSection;
    function TableSection: IWASMTableSection;
    function MemorySection: IWASMMemorySection;
    function GlobalSection: IWASMGlobalSection;
    function ExportSection: IWASMExportSection;
    function StartSection: IWASMStartSection;
    function ElementSection: IWASMElementSection;
    function CodeSection: IWASMCodeSection;
    function DataSection: IWASMDataSection;
  end;

implementation

function GetVarUInt32Size(Value: UInt32): Integer;
begin
  if Value < (1 shl 7) then
    Result := 1
  else
  if Value < (1 shl 14) then
    Result := 2
  else
  if Value < (1 shl 21) then
    Result := 3
  else
  if Value < (1 shl 28) then
    Result := 4
  else begin
    Assert(False, 'Value must be lower then ' + IntToStr(1 shl 28));
    Result := 0;
  end;
end;

{ TWASMWriter }

procedure TWASMWriter.WriteParamTypes(const Params: TWASMDataTypes);
var
  i, pc: Integer;
begin
  pc := Length(Params);
  FTypeSection.WriteStretchUInt(pc);
  for i := 0 to pc - 1 do
    FTypeSection.WriteUInt8(Ord(Params[i]));
end;

function TWASMWriter.AddSignature(const Params: TWASMDataTypes): TWASMIndex;
begin
  FTypeSection.WriteUInt8($60);  // functional type
  WriteParamTypes(Params);       // write params
  FTypeSection.WriteUInt8(0);    // result count
  Result := FTypeSectionECount;
  Inc(FTypeSectionECount);
end;

procedure TWASMWriter.AddFunctionSignatureIndex(SignIndex: TWASMIndex);
begin
  Inc(FFunctionSectionECount);
  FFunctionSection.WriteStretchUInt(SignIndex);
end;

function TWASMWriter.AddSignature(const Params: TWASMDataTypes; ResultType: TWASMDataType): TWASMIndex;
begin
  FTypeSection.WriteUInt8($60);             // functional type
  WriteParamTypes(Params);                  // write params
  FTypeSection.WriteUInt8(1);               // result count
  FTypeSection.WriteUInt8(Ord(ResultType)); // result type
  Result := FTypeSectionECount;
  Inc(FTypeSectionECount);
end;

procedure TWASMWriter.AddTable;
begin
  FTableSection.WriteUInt8($70);      // element_type, always = $60
  FTableSection.WriteUInt8(0);        // flags
  FTableSection.WriteStretchUInt(0);  // initial
  //FTableSection.WriteStretchUInt(0); todo // maximum
  Inc(FTableSectionECount);
end;

function TWASMWriter.AddExportFunction(const Name: string; SignatureIdx: Integer; ExternalKind: Byte): TWASMIndex;
begin
  FExportSection.WriteUTF8String(Name);     // name
  FExportSection.WriteUInt8(ExternalKind);  // external_kind
  FExportSection.WriteUInt8(SignatureIdx);  // index
  Result := FExportSectionECount;
  Inc(FExportSectionECount);
end;

function TWASMWriter.AddFunctionBody: IWASMCodeWriter;
begin
  Result := TWASMFuncBody.Create;
  FFuncBodies.Add(Result);
end;

function TWASMWriter.AddImportFunction(const LibName, FuncName: string; SignatureIdx: Integer): TWASMIndex;
begin
  FImportSection.WriteUTF8String(LibName);       // lib name
  FImportSection.WriteUTF8String(FuncName);      // func name
  FImportSection.WriteUInt8(ord(exFunction));    // kind
  FImportSection.WriteStretchUInt(SignatureIdx); // signature index
  Result := FImportSectionECount;
  Inc(FImportSectionECount);
end;

procedure TWASMWriter.AddMemory;
begin
  FMemorySection.WriteUInt8(0);           // flags
  FMemorySection.WriteStretchUInt(1024);  // initial
  //FTableSection.WriteStretchUInt(0); todo // maximum
  Inc(FMemorySectionECount);
end;

destructor TWASMWriter.Destroy;
begin
  FTypeSection.Free;
  FImportSection.Free;
  FFunctionSection.Free;
  FTableSection.Free;
  FMemorySection.Free;
  FGlobalSection.Free;
  FExportSection.Free;
  FStartSection.Free;
  FElementSection.Free;
  FDataSection.Free;
  FFuncBodies.Free;
  inherited;
end;

procedure TWASMWriter.SaveToStream(Stream: TStream);
begin
  Stream.Position := 0;
  {signature}
  Stream.WriteUInt32($6D736100);
  {version}
  Stream.WriteUInt32(1);
  {Types}
  WriteSection(Stream, 1, FTypeSectionECount, FTypeSection);
  {Import}
  WriteSection(Stream, 2, FImportSectionECount, FImportSection);
  {Function}
  WriteSection(Stream, 3, FFunctionSectionECount, FFunctionSection);
  {Table}
  WriteSection(Stream, 4, FTableSectionECount, FTableSection);
  {Memory}
  WriteSection(Stream, 5, FMemorySectionECount, FMemorySection);
  {Global}
  WriteSection(Stream, 6, FGlobalSectionECount, FGlobalSection);
  {Export}
  WriteSection(Stream, 7, FExportSectionECount, FExportSection);
  {Start}
  //WriteSection(Stream, 8, FStartSectionECount, FStartSection);
  {Element}
  //WriteSection(Stream, 9, FElementSectionECount, FElementSection);
  {Code}
  WriteCodeSection(Stream);
  {Data}
  //WriteSection(Stream, 11, FDataSectionECount, FDataSection);
end;

function TWASMWriter.CodeSection: IWASMCodeSection;
begin
  if not Assigned(FFuncBodies) then
    FFuncBodies := TList<IWASMCodeWriter>.Create;
  Result := Self;
end;

function TWASMWriter.DataSection: IWASMDataSection;
begin
  if not Assigned(FDataSection) then
    FDataSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.ElementSection: IWASMElementSection;
begin
  if not Assigned(FElementSection) then
    FElementSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.ExportSection: IWASMExportSection;
begin
  if not Assigned(FExportSection) then
    FExportSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.FunctionSection: IWASMFunctionSection;
begin
  if not Assigned(FFunctionSection) then
    FFunctionSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.GetExportSectionEntriesCount: Integer;
begin
  Result := FExportSectionECount;
end;

function TWASMWriter.GetFunctionSectionEntriesCount: Integer;
begin
  Result := FFunctionSectionECount;
end;

function TWASMWriter.GetImportSectionEntriesCount: Integer;
begin
  Result := FImportSectionECount;
end;

function TWASMWriter.GetMemorySectionEntriesCount: Integer;
begin
  Result := FMemorySectionECount;
end;

function TWASMWriter.GetTableSectionEntriesCount: Integer;
begin
  Result := FTableSectionECount;
end;

function TWASMWriter.GetTypeSectionEntriesCount: Integer;
begin
  Result := FTypeSectionECount;
end;

function TWASMWriter.GlobalSection: IWASMGlobalSection;
begin
  if not Assigned(FGlobalSection) then
    FGlobalSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.ImportSection: IWASMImportSection;
begin
  if not Assigned(FImportSection) then
    FImportSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.MemorySection: IWASMMemorySection;
begin
  if not Assigned(FMemorySection) then
    FMemorySection := TMemoryStream.Create;
  Result := Self;
end;

procedure TWASMWriter.WriteSection(Stream: TStream; SectionID: Byte; EntriesCount: Integer; Section: TStream);
var
  SectionSize: UInt32;
begin
  if EntriesCount > 0 then
  begin
    Section.Position := 0;
    SectionSize := Section.Size + 1;
  end else
    SectionSize := 1;

  Assert(EntriesCount< 128); // tmp !!!
  Assert(SectionSize < 128); // tmp !!!
  SectionSize := SectionSize or $80808080; // tmp !!!
  Stream.WriteUInt8(SectionID);            // section id
  Stream.WriteUInt32(SectionSize);         // section content size, bytes
  Stream.WriteInt8(0);                     // section name = ''
  Stream.WriteStretchUInt(EntriesCount);   // section entries count
  if EntriesCount > 0 then
    Stream.CopyFrom(Section, Section.Size);// section content
end;

procedure TWASMWriter.WriteCodeSection(Stream: TStream);
var
  i, ec: Integer;
  SectionSize: UInt32;
  BodyData: TStream;
  BodySize: UInt32;
begin
  SectionSize := 0;
  ec := FFuncBodies.Count;
  for i := 0 to ec - 1 do
    SectionSize := SectionSize + FFuncBodies[i].Data.Size + 2 + 4{BodySize: int32};

  Assert(ec < 128); // tmp !!!
  SectionSize := SectionSize + 1;

  Assert(SectionSize < 128); // tmp !!!
  SectionSize := SectionSize or $80808080; // tmp !!!
  Stream.WriteUInt8(10);             // section id
  Stream.WriteUInt32(SectionSize);   // section content size, bytes
  Stream.WriteInt8(0);               // section name = ''
  Stream.WriteInt8(ec);              // func bodies count
  for i := 0 to ec - 1 do
  begin
    BodyData := FFuncBodies[i].Data;
    BodyData.Position := 0;
    BodySize := BodyData.Size + 2 - 1;
    BodySize := BodySize or $80808080; // tmp !!!
    Stream.WriteUInt32(BodySize);      // body size, bytes
    Stream.WriteInt8(0); // number of local entries
    Stream.WriteInt8(0); // local_count
    Stream.CopyFrom(BodyData, BodyData.Size);
  end;
end;

function TWASMWriter.StartSection: IWASMStartSection;
begin
  if not Assigned(FStartSection) then
    FStartSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.TableSection: IWASMTableSection;
begin
  if not Assigned(FTableSection) then
    FTableSection := TMemoryStream.Create;
  Result := Self;
end;

function TWASMWriter.TypesSection: IWASMTypeSection;
begin
  if not Assigned(FTypeSection) then
    FTypeSection := TMemoryStream.Create;
  Result := Self;
end;

{ TWASMFuncBody }

procedure TWASMFuncBody.WriteOpCode(OpCode: TWasmOpCode);
begin
  FStream.WriteStretchUInt(Ord(OpCode));
end;

constructor TWASMFuncBody.Create;
begin
  FStream := TMemoryStream.Create;
end;

function TWASMFuncBody.Data: TStream;
begin
  Result := FStream;
end;

destructor TWASMFuncBody.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TWASMFuncBody.Write_CALL(Index: UInt32);
begin
  WriteOpCode(op_call);
  FStream.WriteStretchUInt(Index);
end;

procedure TWASMFuncBody.Write_F32_CONST(const Data: Int32);
begin
  WriteOpCode(op_f32_const);
  FStream.WriteInt32(Data);
end;

procedure TWASMFuncBody.Write_F64_CONST(const Data: Int64);
begin
  WriteOpCode(op_f64_const);
  FStream.WriteInt64(Data);
end;

procedure TWASMFuncBody.Write_GET_GLOBAL(Index: UInt32);
begin
  WriteOpCode(op_get_global);
  FStream.WriteStretchUInt(Index);
end;

procedure TWASMFuncBody.Write_GET_LOCAL(Index: UInt32);
begin
  WriteOpCode(op_get_local);
  FStream.WriteStretchUInt(Index);
end;

procedure TWASMFuncBody.Write_I32_CONST(const Data: Int32);
begin
  WriteOpCode(op_i32_const);
  FStream.WriteStretchUInt(Data);
end;

procedure TWASMFuncBody.Write_I64_CONST(const Data: Int64);
begin
  WriteOpCode(op_i64_const);
  FStream.WriteInt64(Data);
end;

procedure TWASMFuncBody.Write_VarUInt32(Data: UInt32);
begin
  FStream.WriteStretchUInt(Data);
end;

procedure TWASMFuncBody.WriteEnd;
begin
  FStream.WriteUInt8(wasm_body_end);
end;

end.

(*

int aaa () {
  return 11;
}


(module
 (table 0 anyfunc)
 (memory $0 1)
 (export "memory" (memory $0))
 (export "aaa" (func $aaa))
 (func $aaa (; 0 ;) (result i32)
  (i32.const 11)
 )
)


0, $61, $73, $6D, // sign
1, 0, 0, 0,       // version
1, // section: Function signature declarations (Type)
  $85, $80, $80, $80, // section size = 5
  0, // name length = 0
  1, // count of entries
  $60, // functional type
  0, // param_count
  1, // return_count (the number of results from the function)
  $7F, // type int32

3, // section: Function declarations (Function)
  $82, $80, $80, $80, // section size = 2
  0, // name length = 0
  1, // count of entries
  0, // idx in section=1

4, // section: Indirect function table and other tables (Table)
  $84, $80, $80, $80, // size = 4
  0, // name length = 0
  1, // count of entries
    $70, // anyfunc type
    0,   // flags = 0
    0,   // initial = 0

5, // section: Memory attributes (Memory)
  $83, $80, $80, $80, // size = 3
  0, // name length = 0
  1, // count of entries
  0, // flags = 0
  1, // initial = 1

6, // section: Global declarations (Global)
  $81, $80, $80, $80, // size = 1
  0, // name length = 0
  0, // count of entries

7, // section: Exports (Export)
  $90, $80, $80, $80, // size = 16
  0, // name length = 0
  2, // count of entries
    6, $6D, $65, $6D, $6F, $72, $79, // func_name 'memory'
    2, // external_kind (indicating a Memory)
    0, // index
    3, $61, $61, $61,  // func_name 'aaa'
    0, // external_kind (indicating a Function)
    0, // index

$A, // section: Function bodies (code)
  $8A, $80, $80, $80, // size = 10
  0, // name length = 0
  1, // count
    $84, $80, $80, $80, // body_size = 4
    0, // number of local entries
    0, // local_count
    $41, $B, // i32.const 11
    $B // end_proc

*)
