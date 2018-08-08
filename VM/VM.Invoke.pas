unit VM.Invoke;

interface

{$I compilers.inc}

uses SysUtils, Classes, {$IFNDEF FPC}Rtti, {$ENDIF}TypInfo, IL.TypeInfo, VM.Types, NPCompiler.DataTypes, NPCompiler.Utils,
     Generics.Collections; // system

type

  TInvokeParam = (
    _STR,    // unicode string
    _I32,
    _I64,
    _F32,
    _F64,
    _INF
  );

const _NINT: TInvokeParam = {$IFDEF PTR_SIZE4}_I32 {$ELSE}_I64{$ENDIF};

type
  TInvokeParams = array of TInvokeParam;

  TProcRegInfo = record
    Proc: Pointer;
    Adapter: Pointer;
    IsIntfMethod: Boolean;
  end;
  PProcRegInfo = ^TProcRegInfo;

  TPropRegInfo = record
    Name: string;
    GetProc: Pointer;
    SetProc: Pointer;
  end;

  TProperyList = TDictionary<string, TPropRegInfo>;
  TMethodsList = TDictionary<string, TprocRegInfo>;

  TTypeRegInfo = class
    Lib: string;
    Name: string;
    Data: Pointer;
    Parent: TTypeRegInfo;
    Methods: TMethodsList;
    Properties: TProperyList;
    procedure RegisterMethod(const Name: string; Proc: Pointer; Adapter: Pointer = nil);
    procedure RegisterIntfMethod(const Name: string);
    procedure RegisterDummyMethod;
    procedure RegisterVirtMethod(const Name: string; Proc: Pointer);
    procedure RegisterPublishedProperty(const Name: string);
    procedure RegisterProperty(const Name: string; GetProc, SetProc: Pointer);
    function FindMethod(const Name: string): Pointer;
    function FindPropertyGetter(const Name: string): Pointer;
    function FindPropertySetter(const Name: string): Pointer;
  public
    destructor Destroy; override;
  end;

procedure InvokeExternalStatic(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte); overload;
procedure InvokeExternalVirtual(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
procedure InvokeExternalInterfaceMethod(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
procedure InvokeExternalInterfaceFunc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
procedure InvokeExternalCommonProc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
procedure InvokeExternalCommonFunc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);

function FindProc(const Lib, Name: string): Pointer;
function FindType(const Lib, Name: string): TTypeRegInfo;

procedure RegisterProc(const Lib, Name: string; Proc: Pointer; Adapter: Pointer = nil);

function RegisterType(const Lib, Name: string; aClass: pointer; Parent: TTypeRegInfo = nil): TTypeRegInfo;

function FindInvokeAdapterID(CallConv: TCallConvention; HasResult: Boolean; const ParamSizes: TInvokeParams): Integer;

procedure AddInvokeAdapter(CallConv: TCallConvention; HasResult: Boolean; const Params: array of TInvokeParam; AdapterProc: pointer);

function GetInvokeAdapterPtr(AdapterID: Integer): Pointer;

implementation

uses VM.Core, NativeCalls;

var
  RegProcs: TStringList = nil;
  RegTypes: TStringList = nil;
  InvokeAdapters: array of Pointer;

function GetInvokeAdapterPtr(AdapterID: Integer): Pointer;
begin
  if (AdapterID >= 0) and (AdapterID < Length(InvokeAdapters)) then
    Result := InvokeAdapters[AdapterID]
  else
    Result := nil;
end;

function GetVirtualMethodVMTOffset(FClass: TClass; MethodPtr: Pointer): Integer;
  procedure CheckPackagePtr(var P: PByte);
  begin
    {$IFDEF WIN32}
    // сигнатура реджампинга между пакетами
    if (PWord(p)^ = $25FF) and (PWord(p + 6)^ = $C08B) then
      p := PPointer(PPointer(p + 2)^)^;
    {$ENDIF}
    {$IFDEF WIN64}
    if PWord(p)^ = $25FF then
    begin
      p := PPointer(NativeUInt(@P[0]) + Cardinal((@p[2])^) + 6{Instruction Size})^
    end;
    {$ENDIF}
  end;
type
  TPtrArr = array[0..999] of Pointer;
  PPtrArr = ^TPtrArr;

{$WARNINGS OFF}
const
  VMT_DESTROY_OFFSET = {$IFNDEF FPC} vmtDestroy {$ELSE} vmtMethodStart {$ENDIF};
{$WARNINGS ON}

var
  p: PPtrArr;
  i: Integer;
begin
  CheckPackagePtr(PByte(MethodPtr));
  p := PPtrArr(PByte(FClass) + VMT_DESTROY_OFFSET);
  i := 0;
  while (p[i] <> nil) and (i < 10000) do
  begin
    if p[i] = MethodPtr then
    begin
      Result := VMT_DESTROY_OFFSET + i*sizeof(pointer);
      Exit;
    end;
    Inc(i);
  end;
  Result := -1;
end;

function FindProc(const Lib, Name: string): Pointer;
var
  FullName: string;
  i: Integer;
  RegInfo: PProcRegInfo;
begin
  FullName := LowerCase(Lib + '.' + Name);
  i := RegProcs.IndexOf(FullName);
  if i = -1 then
    Exit(nil);
  RegInfo := PProcRegInfo(RegProcs.Objects[i]);
  Result := RegInfo.Proc;
end;

function FindType(const Lib, Name: string): TTypeRegInfo;
var
  FullName: string;
  i: Integer;
begin
  FullName := LowerCase(Lib + '.' + Name);
  i := RegTypes.IndexOf(FullName);
  if i = -1 then
    Exit(nil);
  Result := RegTypes.Objects[i] as TTypeRegInfo;
end;

procedure RegisterProc(const Lib, Name: string; Proc: Pointer; Adapter: Pointer = nil);
var
  FullName: string;
  RegInfo: PProcRegInfo;
begin
  FullName := LowerCase(Lib + '.' + Name);

  if not Assigned(RegProcs) then
    RegProcs := TStringList.Create;

  if RegProcs.IndexOf(FullName) > -1 then
    raise Exception.CreateFmt('Procedure "%s" in library "%s" already registered', [Name, Lib]);
  New(RegInfo);
  RegInfo.Proc := Proc;
  RegProcs.AddObject(FullName, TObject(RegInfo));
end;

function RegisterType(const Lib, Name: string; aClass: pointer; Parent: TTypeRegInfo = nil): TTypeRegInfo;
var
  FullName: string;
begin
  FullName := LowerCase(Lib + '.' + Name);
  if not Assigned(RegTypes) then
    RegTypes := TStringList.Create;
  if RegTypes.IndexOf(FullName) > -1 then
    raise Exception.CreateFmt('Type "%s" in library "%s" already registered', [Name, Lib]);

  Result := TTypeRegInfo.Create;

  Result.Lib := LowerCase(Lib);
  Result.Name := LowerCase(Name);
  Result.Data := aClass;
  Result.Parent := Parent;
  Result.Methods := TMethodsList.Create;
  Result.Properties := TProperyList.Create;
  RegTypes.AddObject(FullName, TObject(Result));
end;


type
  TInvokeData = record
    Params: TInvokeParams;
    AdapterID: Integer;
  end;
  PInvokeData = ^TInvokeData;
  TInvokeProcs = array of TInvokeData;

var
   // 1 сегмент - нотация вызова, 2 - есть ли возвращяемое значение
  InvokeProcs: array[TCallConvention, boolean] of TInvokeProcs;

type
  _TInt32Array = array [0..65536] of Int32;
  _TInt64Array = array [0..65536] of Int64;
  _NativeIntArray = array [0..65536] of NativeInt;
  //_TFlt64Array = array [0..65536] of Double;
  PInt32Array = ^_TInt32Array;
  PInt64Array = ^_TInt64Array;
  PNativeIntArray = ^_NativeIntArray;
  PIInterface = ^IInterface;

  //PFlt64Array = ^_TFlt64Array;

function FindInvokeAdapterID(CallConv: TCallConvention; HasResult: Boolean; const ParamSizes: TInvokeParams): Integer;
var
  i, pi, pc: Integer;
  Founded: Boolean;
  Item: PInvokeData;
  Items: TInvokeProcs;
begin
  Items := InvokeProcs[CallConv][HasResult];
  for i := 0 to Length(Items) - 1 do
  begin
    Item := @Items[i];
    pc := Length(Item.Params);
    if pc <> Length(ParamSizes) then
      Continue;
    Founded := True;
    for pi := 0 to pc - 1 do
    begin
      if Item.Params[pi] <> ParamSizes[pi] then
      begin
        Founded := False;
        Break;
      end;
    end;
    if Founded then
      Exit(Item.AdapterID);
  end;
  Result := -1;
end;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Invoke routines adapters
//////////////////////////////////////////////////////////////////////////////////////////////////

procedure _ccReg_proc_0(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure;
begin
  TProc(Proc)();
end;

procedure _ccReg_proc_I32(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0: Int32);
begin
  TProc(Proc)(Params[0]);
end;

procedure _ccReg_proc_I64(Proc: Pointer; Params: PInt64Array);
type
  TProc = procedure(P0: Int64);
begin
  TProc(Proc)(Params[0]);
end;

procedure _ccReg_proc_I32_I32(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0, P1: Int32);
begin
  TProc(Proc)(Params[0], Params[1]);
end;

procedure _ccReg_proc_I32_I64(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0: Int32; P1: Int64);
begin
  TProc(Proc)(Params[0], PInt64(PByte(Params) + 4)^);
end;

procedure _ccReg_proc_I64_I64(Proc: Pointer; Params: PInt64Array);
type
  TProc = procedure(P0, P1: Int64);
begin
  TProc(Proc)(Params[0], Params[1]);
end;

procedure _ccReg_proc_I32_I32_I32(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0, P1, P2: Int32);
begin
  TProc(Proc)(Params[0], Params[1], Params[2]);
end;

procedure _ccReg_proc_I32_I32_I32_I32(Proc: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0, P1, P2, P3: Int32);
begin
  TProc(Proc)(Params[0], Params[1], Params[2], Params[3]);
end;

procedure _ccReg_proc_I32_F32(Proc: Pointer; Params: PByte);
type
  TProc = procedure(P0: Int32; P1: Float32);
begin
  TProc(Proc)(PInt32(Params)^, PFlt32(Params + 4)^);
end;

procedure _ccReg_proc_I32_F64(Proc: Pointer; Params: PByte);
type
  TProc = procedure(P0: Int32; P1: Float64);
begin
  TProc(Proc)(PInt32(Params)^, PFlt64(Params + 4)^);
end;

procedure _ccReg_proc_I64_F32(Proc: Pointer; Params: PByte);
type
  TProc = procedure(P0: Int32; P1: Float32);
begin
  TProc(Proc)(PInt32(Params)^, PFlt32(Params + 8)^);
end;

procedure _ccReg_proc_I64_F64(Proc: Pointer; Params: PByte);
type
  TProc = procedure(P0: Int32; P1: Float64);
begin
  TProc(Proc)(PInt32(Params)^, PFlt64(Params + 8)^);
end;

procedure _ccReg_func_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function: Int32;
begin
  Params[0] := TFunc(Func)();
end;

procedure _ccReg_func_I64(Func: Pointer; Params: PInt64Array);
type
  TFunc = function: Int64;
begin
  Params[0] := TFunc(Func)();
end;

procedure _ccReg_func_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function(P0: Int32): Int32;
begin
  Params[0] := TFunc(Func)(Params[1]);
end;

procedure _ccReg_func_I64_I64(Func: Pointer; Params: PInt64Array);
type
  TFunc = function(P0: Int64): Int64;
begin
  Params[0] := TFunc(Func)(Params[1]);
end;

procedure _ccReg_func_I32_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function(P0, P1: Int32): Int32;
begin
  Params[0] := TFunc(Func)(Params[1], Params[2]);
end;

procedure _ccReg_func_I32_F64_F64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1: double): Int32;
begin
  PInt32(Params)^ := TFunc(Func)(PDouble(Params + 4)^, PDouble(Params + 12)^);
end;

procedure _ccReg_func_I64_F64_F64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1: double): Int64;
begin
  PInt64(Params)^ := TFunc(Func)(PDouble(Params + 8)^, PDouble(Params + 8*2)^);
end;

procedure _ccReg_func_F32(Func: Pointer; Params: PFlt32);
type
  TFunc = function: Single;
begin
  Params^ := TFunc(Func)();
end;

procedure _ccReg_func_F64(Func: Pointer; Params: PFlt64);
type
  TFunc = function: Double;
begin
  Params^ := TFunc(Func)();
end;

procedure _ccReg_func_F64_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int32): Double;
begin
  PDouble(Params)^ := TFunc(Func)(PInt32(Params + 8)^);
end;

procedure _ccReg_func_F64_I64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int64): Double;
begin
  PDouble(Params)^ := TFunc(Func)(PInt64(Params + 8)^);
end;

procedure _ccReg_func_F64_F64_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: double; P1: Int32): Double;
begin
  PDouble(Params)^ := TFunc(Func)(PDouble(Params + 8)^, PInt32(Params + 8*2)^);
end;

procedure _ccReg_func_F64_F64_I64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: double; P1: Int64): Double;
begin
  PDouble(Params)^ := TFunc(Func)(PDouble(Params + 8)^, PInt64(Params + 8*2)^);
end;

procedure _ccReg_func_I32_I32_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function(P0, P1, P2: Int32): Int32;
begin
  Params[0] := TFunc(Func)(Params[1], Params[2], Params[3]);
end;

procedure _ccReg_func_STR_0(Func: Pointer; Params: PNativeUInt);
type
  TFunc = function: string;
begin
  string(Params^) := TFunc(Func)();
end;

procedure _ccReg_func_STR_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int32): string;
begin
  PString(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^);
end;

procedure _ccReg_func_STR_F32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Float32): string;
begin
  PString(Params)^ := TFunc(Func)(PFlt32(Params + PTR_SIZE)^);
end;

procedure _ccReg_func_STR_F64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Float64): string;
begin
  string(Params) := TFunc(Func)(PFlt64(Params + PTR_SIZE)^);
end;

procedure _ccReg_func_STR_I32_F64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int32; P1: Double): string;
begin
  PString(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PDouble(Params + PTR_SIZE*2)^);
end;

procedure _ccReg_func_STR_I32_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1: Int32): string;
begin
  PString(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PInt32(Params + 2*PTR_SIZE)^);
end;

procedure _ccReg_func_STR_I32_I32_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1, P2: Int32): string;
{$IFDEF FPC}
var
  s: string;
begin
  s := TFunc(Func)(Params[1], Params[2], Params[3]);
  Params[0] := NativeInt(s);
{$ELSE}
begin
  PString(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PInt32(Params + 2*PTR_SIZE)^, PInt32(Params + 3*PTR_SIZE)^);
{$ENDIF}
end;

procedure _ccReg_func_STR_I64(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int64): string;
begin
  PString(Params)^ := TFunc(Func)(PInt64(Params + PTR_SIZE)^);
end;

procedure _ccReg_func_INTF_0(Func: Pointer; Params: PByte);
type
  TFunc = function(): IInterface;
begin
  PIInterface(Params)^ := TFunc(Func)();
end;

procedure _ccReg_func_INTF_INT32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0: Int32): IInterface;
begin
  PIInterface(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^);
end;

procedure _ccReg_func_INTF_I32_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1: Int32): IInterface;
begin
  PIInterface(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PInt32(Params + 2*PTR_SIZE)^);
end;

procedure _ccReg_func_INTF_I32_I32_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1, P2: Int32): IInterface;
begin
  PIInterface(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PInt32(Params + 2*PTR_SIZE)^, PInt32(Params + 3*PTR_SIZE)^);
end;

procedure _ccReg_func_INTF_I32_I32_I32_I32(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1, P2, P3: Int32): IInterface;
begin
  PIInterface(Params)^ := TFunc(Func)(PInt32(Params + PTR_SIZE)^, PInt32(Params + 2*PTR_SIZE)^,
                                      PInt32(Params + 3*PTR_SIZE)^, PInt32(Params + 4*PTR_SIZE)^);
end;

/////////////////////////////////////////////////////////////////////////////////////
// STDCALL
/////////////////////////////////////////////////////////////////////////////////////


procedure _ccStdCall_proc_I32_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TProc = procedure(P0, P2, P3: Int32); stdcall;
begin
  TProc(Func)(Params[0], Params[1], Params[2]);
end;


procedure _ccStdCall_func_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function: Int32; stdcall;
begin
  Params[0] := TFunc(Func)();
end;

procedure _ccStdCall_func_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function(P0: Int32): Int32; stdcall;
begin
  Params[0] := TFunc(Func)(Params[1]);
end;

procedure _ccStdCall_func_I32_I32_I32_I32(Func: Pointer; Params: PInt32Array);
type
  TFunc = function(P0, P2, P3: Int32): Int32; stdcall;
begin
  Params[0] := TFunc(Func)(Params[1], Params[2], Params[3]);
end;

procedure _ccStdCall_func_I64(Func: Pointer; Params: PInt64Array);
type
  TFunc = function: Int64; stdcall;
begin
  Params[0] := TFunc(Func)();
end;

procedure _ccStdCall_func_I64_I64(Func: Pointer; Params: PInt64Array);
type
  TFunc = function(P0: Int64): Int64; stdcall;
begin
  Params[0] := TFunc(Func)(Params[1]);
end;

procedure _ccStdCall_func_I32_NINT_NINT(Func: Pointer; Params: PByte);
type
  TFunc = function(P0, P1: NativeInt): Int32; stdcall;
begin
  PInt32(Params)^ := TFunc(Func)(PNativeInt(Params + 4)^, PNativeInt(Params + 4 + PTR_SIZE)^);
end;

procedure InvokeExternalStatic(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
type
  TAdapterProc = procedure(Func: Pointer; Params: Pointer);
var
  ImportIndex: Integer;
  Adapter: Pointer;
  ImportProc: PImportEntry;
begin
  ImportIndex := PM^;
  ImportProc := addr(PImportTable(Header.ImportTable)[ImportIndex]);

  Adapter := Pointer(ImportProc.Adapter);
  Inc(PM);

  StackPtr := StackPtr + PM^; // stack size
  TAdapterProc(Adapter)(Pointer(ImportProc.ADDR), StackPtr);
end;

procedure InvokeExternalCommonProc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
var
  ImportIndex: Integer;
  ImportProc: PImportEntry;
  StackSize: Integer;
  ProcPtr: Pointer;
  CallConv: TCallConvention;
begin
  CallConv := TCallConvention(PM^ shr 24);
  if CallConv <> ConvNative then; // hint remove
  ImportIndex := (PM^ and (not(255 shl 24)));
  ImportProc := @PImportTable(Header.ImportTable)[ImportIndex];
  Inc(PM);

  StackSize := PM^;
  StackPtr := StackPtr + StackSize; // stack size
  Inc(PM);

  //ArgsCount := PM^; // skip the args count field
  Inc(PM);

  if ImportProc.IsIntfMethod then
  begin
    ProcPtr := Pointer(PPointer(PPointer(StackPtr)^)^); // получаем IMT
    ProcPtr := PPointer(NativeUInt(ProcPtr) + PTR_SIZE*ImportProc.ADDR)^; // складывая базу VMT и индекс*PTR_SIZE, получаем адрес метода
  end else
    ProcPtr := Pointer(ImportProc.ADDR);

  NativeCall(ProcPtr, TArgs.TArgList(PM), PCardinal(StackPtr));
end;

procedure InvokeExternalCommonFunc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
var
  ImportIndex: Integer;
  ImportProc: PImportEntry;
  StackSize: Integer;
  ResultPtr: PCardinal;
  ResultTID: TArgTypeID;
  ProcPtr: Pointer;
  CallConv: TCallConvention;
begin
  CallConv := TCallConvention(PM^ shr 24);
  if CallConv <> ConvNative then; // hint remove
  ImportIndex := (PM^ and (not(255 shl 24)));
  ImportProc := @PImportTable(Header.ImportTable)[ImportIndex];
  Inc(PM);

  StackSize := PM^;
  StackPtr := StackPtr + StackSize; // stack size
  Inc(PM);

  ResultTID := TArgTypeID(PM^); // Result TID
  Inc(PM);

  //ArgsCount := PM^; // skip the args count field
  Inc(PM);

  ResultPtr := PCardinal(StackPtr);

  {если передается указатель на структуру/сет/стат. массив то нам надо передать его напрямую}
  if ResultTID in [atSet1, atSet2, atSet3, atSet4, atSet5, atSet6, atSet7, atSet8, atSetN,
                   atRecord1, atRecord2, atRecord3, atRecord4, atRecord5, atRecord6, atRecord7, atRecord8, atRecordN,
                   atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4, atStaticArray5, atStaticArray6, atStaticArray7, atStaticArray8, atStaticArrayN] then
    ResultPtr := PCardinal(ResultPtr^);

  {инкриментируем указатeль на стек на размер результата}
  {$IF Defined(CPUX86) or Defined(CPUARM)}
  if ResultTID in [atInt64, atFloat64] then
    StackPtr := StackPtr + 8
  else
    StackPtr := StackPtr + 4;
  {$ELSE}
    if ResultTID in [atInt8, atInt16, atInt32, atFloat32] then
      StackPtr := StackPtr + 4
    else
      StackPtr := StackPtr + 8;
  {$ENDIF}

  if ImportProc.IsIntfMethod then
  begin
    ProcPtr := Pointer(PPointer(PPointer(StackPtr)^)^); // получаем IMT
    ProcPtr := PPointer(NativeUInt(ProcPtr) + PTR_SIZE*ImportProc.ADDR)^; // складывая базу VMT и индекс*PTR_SIZE, получаем адрес метода
  end else
    ProcPtr := Pointer(ImportProc.ADDR);
  NativeCall(ProcPtr, TArgs.TArgList(PM), PCardinal(StackPtr), ResultPtr, ResultTID);
end;

procedure InvokeExternalVirtual(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
type
  TAdapterProc = procedure(Func: Pointer; Params: Pointer);
var
  ImportIndex: Integer;
  Adapter: Pointer;
  ImportProc: PImportEntry;
  ProcPtr: Pointer;
begin
  ImportIndex := PM^;
  Inc(PM);
  ImportProc := addr(PImportTable(Header.ImportTable)[ImportIndex]);

  StackPtr := StackPtr + PM^; // stack size
  Inc(PM);

  ProcPtr := Pointer(PPointer(PPointer(StackPtr)^)^); // получаем VMT
  ProcPtr := PPointer(NativeUInt(ProcPtr) + PM^)^;    // складывая базу VMT и индекс*PTR_SIZE, получаем адрес метода  !!! ????

  Adapter := Pointer(ImportProc.Adapter);
  TAdapterProc(Adapter)(ProcPtr, StackPtr);
end;

procedure InvokeExternalInterfaceMethod(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
type
  TAdapterProc = procedure(Func: Pointer; Params: Pointer);
var
  ImportIndex: Integer;
  Adapter: Pointer;
  ImportProc: PImportEntry;
  ProcPtr: Pointer;
begin
  ImportIndex := PM^;
  Inc(PM);
  ImportProc := @PImportTable(Header.ImportTable)[ImportIndex];

  StackPtr := StackPtr + PM^; // stack size

  ProcPtr := Pointer(PPointer(PPointer(StackPtr)^)^); // получаем IMT
  ProcPtr := PPointer(NativeUInt(ProcPtr) + PTR_SIZE*ImportProc.ADDR)^; // складывая базу VMT и индекс*PTR_SIZE, получаем адрес метода

  Adapter := Pointer(ImportProc.Adapter);
  TAdapterProc(Adapter)(ProcPtr, StackPtr);
end;

procedure InvokeExternalInterfaceFunc(const Header: PIMGHeader; PM: PNativeUInt; StackPtr: PByte);
type
  TAdapterProc = procedure(Func: Pointer; Params: Pointer);
var
  ImportIndex: Integer;
  Adapter: Pointer;
  ImportProc: PImportEntry;
  ProcPtr: Pointer;
begin
  ImportIndex := PM^;
  Inc(PM);
  ImportProc := @PImportTable(Header.ImportTable)[ImportIndex];

  StackPtr := StackPtr + PM^; // stack size

  ProcPtr := Pointer(PPointer(PPointer(StackPtr + PTR_SIZE)^)^); // получаем IMT
  ProcPtr := PPointer(NativeUInt(ProcPtr) + PTR_SIZE*ImportProc.ADDR)^; // складывая базу VMT и индекс*PTR_SIZE, получаем адрес метода

  Adapter := Pointer(ImportProc.Adapter);
  TAdapterProc(Adapter)(ProcPtr, StackPtr);
end;

procedure AddInvokeAdapter(CallConv: TCallConvention; HasResult: Boolean; const Params: array of TInvokeParam; AdapterProc: pointer);
var
  Item: PInvokeData;
  Items: TInvokeProcs;
  i, c, ItemIndex: Integer;
  InvokeParams: TInvokeParams;
begin
  Items := InvokeProcs[CallConv][HasResult];

  SetLength(InvokeParams, Length(Params));
  for i := 0 to Length(Params) - 1 do
    InvokeParams[i] := Params[i];

  // поиск существующей декларации
  if FindInvokeAdapterID(CallConv, HasResult, InvokeParams) <> -1 then
    raise Exception.Create('Invoke type already registered');

  ItemIndex := Length(Items);
  SetLength(Items, ItemIndex + 1);
  InvokeProcs[CallConv][HasResult] := Items;
  Item := @Items[ItemIndex];
  Item.Params := InvokeParams;

  c := Length(InvokeAdapters);
  SetLength(InvokeAdapters, c + 1);
  InvokeAdapters[c] := AdapterProc;
  Item.AdapterID := c;
end;

{ TTypeRegInfo }

destructor TTypeRegInfo.Destroy;
begin
  FreeAndNil(Methods);
  FreeAndNil(Properties);
  inherited;
end;

function TTypeRegInfo.FindMethod(const Name: string): Pointer;
var
  RI: TProcRegInfo;
  LCName: string;
begin
  LCName := LowerCase(Name);
  if Methods.TryGetValue(LCName, RI) then
    Result := RI.Proc
  else
    Result := nil;
end;

function TTypeRegInfo.FindPropertyGetter(const Name: string): Pointer;
var
  info: TPropRegInfo;
begin
  if Properties.TryGetValue(LowerCase(Name), info) then
    Result := info.GetProc
  else
    Result := nil;
end;

function TTypeRegInfo.FindPropertySetter(const Name: string): Pointer;
var
  info: TPropRegInfo;
begin
  if Properties.TryGetValue(LowerCase(Name), info) then
    Result := info.SetProc
  else
    Result := nil;
end;

procedure TTypeRegInfo.RegisterDummyMethod;
var
  Idx: Integer;
  PR: TTypeRegInfo;
  RI: TProcRegInfo;
begin
  Idx := Methods.Count;

  PR := Parent;
  while Assigned(PR) do
  begin
    Inc(Idx, PR.Methods.Count);
    PR := PR.Parent;
  end;

  Methods.Add('$Dummy' + IntToStr(Idx), RI);
end;

procedure TTypeRegInfo.RegisterIntfMethod(const Name: string);
var
  RI: TProcRegInfo;
  Idx: Integer;
  PR: TTypeRegInfo;
begin
  Idx := Methods.Count;

  PR := Parent;
  while Assigned(PR) do
  begin
    Inc(Idx, PR.Methods.Count);
    PR := PR.Parent;
  end;

  RI.Proc := Pointer(Idx);
  RI.Adapter := nil;
  RI.IsIntfMethod := True;
  Methods.Add(LowerCase(Name), RI);
end;

procedure TTypeRegInfo.RegisterMethod(const Name: string; Proc: Pointer; Adapter: Pointer = nil);
var
  RI: TProcRegInfo;
begin
  RI.Proc := Proc;
  RI.Adapter := Adapter;
  RI.IsIntfMethod := (Self.Data = nil);
  Methods.Add(LowerCase(Name), RI);
end;

procedure TTypeRegInfo.RegisterProperty(const Name: string; GetProc, SetProc: Pointer);
var
  Info: TPropRegInfo;
begin
  Info.GetProc := GetProc;
  Info.SetProc := SetProc;
  Properties.Add(LowerCase(Name), Info);
end;

procedure TTypeRegInfo.RegisterPublishedProperty(const Name: string);
var
  PropInfo: PPropInfo;
  Info: TPropRegInfo;
begin
  PropInfo := GetPropInfo(TClass(Self.Data), Name);
  if not Assigned(PropInfo) then
    raise Exception.CreateFmt('Property "%s" is not found in "%s"', [Name, Self.Name]);
  Info.GetProc := PropInfo.GetProc;
  Info.SetProc := PropInfo.SetProc;
  Properties.Add(LowerCase(Name), Info);
end;

procedure TTypeRegInfo.RegisterVirtMethod(const Name: string; Proc: Pointer);
var
  VmtOffset: Integer;
  RI: TProcRegInfo;
begin
  VmtOffset := GetVirtualMethodVMTOffset(TClass(Self.Data), Proc);
  if VmtOffset = -1 then
    raise Exception.CreateFmt('Method "%s" is not virtual or abstract', [name]);
  RI.Proc := Pointer(VmtOffset);
  Methods.Add(LowerCase(Name), RI);
end;

procedure AddStandartInvokeAdapters;
begin
  AddInvokeAdapter(ConvNative, False, [], @_ccReg_proc_0);                                             // procedure
  AddInvokeAdapter(ConvNative, False, [_I32], @_ccReg_proc_I32);                                       // procedure(int32)
  AddInvokeAdapter(ConvNative, False, [_I64], @_ccReg_proc_I64);                                       // procedure(int64)
  AddInvokeAdapter(ConvNative, False, [_I32, _I32], @_ccReg_proc_I32_I32);                             // procedure(int32, int32)
  AddInvokeAdapter(ConvNative, False, [_I32, _I64], @_ccReg_proc_I32_I64);                             // procedure(int32, int64)
  AddInvokeAdapter(ConvNative, False, [_I64, _I64], @_ccReg_proc_I64_I64);                             // procedure(int64, int64)
  AddInvokeAdapter(ConvNative, False, [_I32, _I32, _I32], @_ccReg_proc_I32_I32_I32);                   // procedure(int32, int32, int32)
  AddInvokeAdapter(ConvNative, False, [_I32, _I32, _I32, _I32], @_ccReg_proc_I32_I32_I32_I32);         // procedure(int32, int32, int32, int32)

  AddInvokeAdapter(ConvNative, False, [_I32, _F32], @_ccReg_proc_I32_F32);                             // procedure(int32, flt32)
  AddInvokeAdapter(ConvNative, False, [_I32, _F64], @_ccReg_proc_I32_F64);                             // procedure(int32, flt64)
  AddInvokeAdapter(ConvNative, False, [_I64, _F32], @_ccReg_proc_I64_F32);                             // procedure(int64, flt32)
  AddInvokeAdapter(ConvNative, False, [_I64, _F64], @_ccReg_proc_I64_F64);                             // procedure(int64, flt64)

  AddInvokeAdapter(ConvNative, True, [_I32], @_ccReg_func_I32);                                        // function: int32
  AddInvokeAdapter(ConvNative, True, [_I64], @_ccReg_func_I64);                                        // function: int64
  AddInvokeAdapter(ConvNative, True, [_I32, _I32], @_ccReg_func_I32_I32);                              // function(int32): int32
  AddInvokeAdapter(ConvNative, True, [_I64, _I64], @_ccReg_func_I64_I64);                              // function(int64): int64
  AddInvokeAdapter(ConvNative, True, [_I32, _I32, _I32], @_ccReg_func_I32_I32_I32);                    // function(int32, int32): int32
  AddInvokeAdapter(ConvNative, True, [_I32, _I32, _I32, _I32], @_ccReg_func_I32_I32_I32_I32);          // function(int32, int32, int32): int32
  AddInvokeAdapter(ConvNative, True, [_I32, _F64, _F64], @_ccReg_func_I32_F64_F64);                    // function(flt64, flt64): int32
  AddInvokeAdapter(ConvNative, True, [_I64, _F64, _F64], @_ccReg_func_I64_F64_F64);                    // function(flt64, flt64): int64

  AddInvokeAdapter(ConvNative, True, [_F32], @_ccReg_func_F32);                                        // function: flt32
  AddInvokeAdapter(ConvNative, True, [_F64], @_ccReg_func_F64);                                        // function: flt64
  AddInvokeAdapter(ConvNative, True, [_F64, _I32], @_ccReg_func_F64_I32);                              // function(int32): flt64
  AddInvokeAdapter(ConvNative, True, [_F64, _I64], @_ccReg_func_F64_I64);                              // function(int64): flt64
  AddInvokeAdapter(ConvNative, True, [_F64, _F64, _I32], @_ccReg_func_F64_F64_I32);                    // function(flt64, int32): flt64
  AddInvokeAdapter(ConvNative, True, [_F64, _F64, _I64], @_ccReg_func_F64_F64_I64);                    // function(flt64, int64): flt64

  AddInvokeAdapter(ConvNative, True, [_STR], @_ccReg_func_STR_0);                                      // function: string
  AddInvokeAdapter(ConvNative, True, [_STR, _I32], @_ccReg_func_STR_I32);                              // function(int32): string
  AddInvokeAdapter(ConvNative, True, [_STR, _I64], @_ccReg_func_STR_I64);                              // function(int64): string
  AddInvokeAdapter(ConvNative, True, [_STR, _F32], @_ccReg_func_STR_F32);                              // function(flt32): string
  AddInvokeAdapter(ConvNative, True, [_STR, _F64], @_ccReg_func_STR_F64);                              // function(flt64): string

  AddInvokeAdapter(ConvNative, True, [_STR, _I32, _I32], @_ccReg_func_STR_I32_I32);                    // function(int32, int32): string
  AddInvokeAdapter(ConvNative, True, [_STR, _I32, _F64], @_ccReg_func_STR_I32_F64);                    // function(int32, flt64): string
  AddInvokeAdapter(ConvNative, True, [_STR, _I32, _I32, _I32], @_ccReg_func_STR_I32_I32_I32);          // function(int32, int32, int32): string

  AddInvokeAdapter(ConvNative, True, [_INF], @_ccReg_func_INTF_0);                                     // function: interface
  AddInvokeAdapter(ConvNative, True, [_INF, _I32], @_ccReg_func_INTF_INT32);                           // function(int32): interface
  AddInvokeAdapter(ConvNative, True, [_INF, _I32, _I32], @_ccReg_func_INTF_I32_I32);                   // function(int32, int32): interface
  AddInvokeAdapter(ConvNative, True, [_INF, _I32, _I32, _I32], @_ccReg_func_INTF_I32_I32_I32);         // function(int32, int32, int32): interface
  AddInvokeAdapter(ConvNative, True, [_INF, _I32, _I32, _I32, _I32], @_ccReg_func_INTF_I32_I32_I32_I32);  // function(int32, int32, int32, int32): interface


  AddInvokeAdapter(ConvStdCall, False, [_I32, _I32, _I32], @_ccStdCall_proc_I32_I32_I32);


  AddInvokeAdapter(ConvStdCall, True, [_I32], @_ccStdCall_func_I32);
  AddInvokeAdapter(ConvStdCall, True, [_I32, _I32], @_ccStdCall_func_I32_I32);
  AddInvokeAdapter(ConvStdCall, True, [_I32, _I32, _I32, _I32], @_ccStdCall_func_I32_I32_I32_I32);
  AddInvokeAdapter(ConvStdCall, True, [_I64, _I64], @_ccStdCall_func_I64_I64);
  AddInvokeAdapter(ConvStdCall, True, [_I32, _NINT, _NINT], @_ccStdCall_func_I32_NINT_NINT);
end;

procedure ClearFullRegisterInfo;
var
  i: Integer;
  ProcRegInfo: PProcRegInfo;
  TypeRegInfo: TTypeRegInfo;
begin
  if Assigned(RegProcs) then
  for i := 0 to RegProcs.Count - 1 do
  begin
    ProcRegInfo := PProcRegInfo(RegProcs.Objects[i]);
    Dispose(ProcRegInfo);
  end;
  RegProcs.Free;

  if Assigned(RegTypes) then
  for i := 0 to RegTypes.Count - 1 do
  begin
    TypeRegInfo := RegTypes.Objects[i] as TTypeRegInfo;
    TypeRegInfo.Free;
  end;
  RegTypes.Free;
end;

initialization
  AddStandartInvokeAdapters;
  if Assigned(RegTypes) then
    RegTypes.Sort;


finalization
  ClearFullRegisterInfo;

end.
