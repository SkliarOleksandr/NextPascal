{ implementation of x86         abi }
unit NativeCallX86;

interface

{$i compilers.inc}

uses SysUtils, {$IFNDEF FPC}AnsiStrings, {$ENDIF} Classes, NativeCalls;

procedure X86Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal); overload;
procedure X86Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal; const ResultData: PCardinal; const ResultInfo: TArgTypeID); overload;

implementation

const PTR_SIZE = SizeOf(Pointer);

{$ifdef FPC}
{$define PS_ARRAY_ON_STACK}
{$endif}
function RealFloatCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;
                 
function RealFloatCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;

function RealFloatCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
    @@5:
    mov ecx, stackdatalen
    jecxz @@2
    @@6:
    pop edx
    dec ecx
    or ecx, ecx
    jnz @@6
  end;
  Result := E;
end;

function RealCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Cardinal; Stdcall; // make sure all things are on stack
var
  r: Cardinal;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

type
  PInt8 = ^ShortInt;
  PInt16 = ^SmallInt;
  PInt32 = ^Integer;

type
  TCallContext = record
    EAX, EDX, ECX: UInt32;
    Data: PCardinal;
    Stack: AnsiString;
    RegUsage: Integer;
    procedure Init(const Data: PCardinal); inline;
    procedure AddStack4(const AData: NativeUInt);
    procedure AddStack8(const AData: PInt64);
    procedure AddGen(const AData: NativeUInt);
    procedure AddArg(const Arg: NativeUInt);
  end;

procedure ASMCall_RegisterProc(p: Pointer;
  _EAX, _EDX, _ECX: UInt32;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ); stdcall; // make sure all things are on stack
begin
  asm
    mov ecx, StackDataLen
    jecxz @@2
    mov eax, StackData
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax, _EAX
    mov edx, _EDX
    mov ecx, _ECX
    call p
  end;
end;

function RealCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, stackdatalen
    jecxz @@7
    @@6:
    pop eax
    dec ecx
    or ecx, ecx
    jnz @@6
    mov ecx, resedx
    jecxz @@7
    mov [ecx], edx
    @@7:
  end;
  Result := r;
end;

procedure TCallContext.AddStack4(const AData: NativeUInt);
var
  Str: AnsiString;
begin
  Str := StringOfChar(AnsiChar(#0), 4);
  PNativeUInt(@Str[1])^ := AData;
  Stack := Str + Stack;
end;

procedure TCallContext.AddStack8(const AData: PInt64);
var
  Str: AnsiString;
begin
  Str := StringOfChar(AnsiChar(#0), 8);
  PInt64(@Str[1])^ := AData^;
  Stack := Str + Stack;
end;

procedure TCallContext.Init(const Data: PCardinal);
begin
  RegUsage := 0;
  EAX := 0;
  EDX := 0;
  ECX := 0;
  Self.Data := Data;
end;

procedure TCallContext.AddGen(const AData: NativeUInt);
begin
  case RegUsage of
    0: begin EAX := AData; Inc(RegUsage); end;
    1: begin EDX := AData; Inc(RegUsage); end;
    2: begin ECX := AData; Inc(RegUsage); end;
  else
    AddStack4(AData);
  end;
end;

procedure TCallContext.AddArg(const Arg: NativeUInt);
begin
  case TArgTypeID(Arg) of
    atInt8,
    atInt16,
    atInt32, atNativeInt, atDynArray, atString, atInterface: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atFloat32: begin
      AddStack4(Data^);
      Inc(Data, 1);
    end;
    atInt64, atFloat64: begin
      AddStack8(PInt64(Data));      // аргумент int64/float64 передан по значению
      Inc(Data, 2);
    end;
    atVariant: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atSet1, atSet2, atSet3, atSet4, atSetN: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atSet5, atSet6, atSet7, atSet8: begin
      AddGen(NativeInt(Data));
      Inc(Data, 2);
    end;
    {$IFDEF FPC}
    atRecord1, atRecord2, atRecord3, atRecord4,
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4:
    begin
      AddStack4(Data^);
      Inc(Data, 1);
    end;
    atRecord5, atRecord6, atRecord7, atRecord8,
    atStaticArray5, atStaticArray6, atStaticArray7, atStaticArray8:
    begin
      AddGen(NativeUInt(Data));
      Inc(Data, 1);
    end;
    atRecordN,
    atStaticArrayN:
    begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    {$ELSE}
    atRecord1, atRecord2, atRecord4,
    atStaticArray1, atStaticArray2, atStaticArray4: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atRecord3, atStaticArray3: begin
      AddStack4(Data^);
      Inc(Data, 1);
    end;
    atRecord5, atRecord6, atRecord7, atRecord8,
    atStaticArray5, atStaticArray6, atStaticArray7, atStaticArray8: begin
      AddGen(NativeUInt(Data));
      Inc(Data, 2);
    end;
    atRecordN, atStaticArrayN: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    {$ENDIF}
  else
    raise Exception.Create('Invalid type');
  end;
end;

procedure X86Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal); overload;
var
  Context: TCallContext;
  i, ac, StackLength: Integer;
  StackPtr: Pointer;
begin
  Context.Init(Data);
  ac := Length(ArgsInfo);
  for i := 0 to ac - 1 do
   Context.AddArg(ArgsInfo[i]);

  StackLength := Length(Context.Stack);
  if StackLength > 0 then
    StackPtr := addr(Context.Stack[StackLength - 3])
  else
    StackPtr := nil;

  ASMCall_RegisterProc(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4);
end;

type
  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;

procedure X86Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal; const ResultData: PCardinal; const ResultInfo: TArgTypeID); overload;
var
  Context: TCallContext;
  i, ac, StackLength, EAX, EDX: Integer;
  StackPtr: Pointer;
  FloatResult: Extended;
begin
  Context.Init(Data);
  ac := Length(ArgsInfo);
  for i := 0 to ac - 1 do
   Context.AddArg(ArgsInfo[i]);

  case ResultInfo of
    {$IFDEF FPC}
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4, atStaticArray5,
    atStaticArray6, atStaticArray7, atStaticArray8, atStaticArrayN: Context.AddGen(NativeUInt(ResultData));
    {$ELSE}
    atStaticArray3, atStaticArray5, atStaticArray6,
    atStaticArray7, atStaticArray8, atStaticArrayN: Context.AddGen(NativeUInt(ResultData));
    {$ENDIF}
    atDynArray, atString, atInterface, atVariant: Context.AddGen(NativeUInt(ResultData));
    atRecord3, atRecord5, atRecord6, atRecord7, atRecord8, atRecordN: Context.AddGen(NativeUInt(ResultData));
    atSet5, atSet6, atSet7, atSet8, atSetN: Context.AddGen(NativeUInt(ResultData));
  end;

  StackLength := Length(Context.Stack);
  if StackLength > 0 then
    StackPtr := addr(Context.Stack[StackLength - 3])
  else
    StackPtr := nil;

  case ResultInfo of
    atInt8, atSet1, atRecord1
    {$IFNDEF FPC}, atStaticArray1{$ENDIF}: PUInt8(ResultData)^ := RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 1, nil);
    atInt16, atSet2, atRecord2
    {$IFNDEF FPC}, atStaticArray2{$ENDIF}: PUInt16(ResultData)^ := RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 2, nil);
    atInt32, atNativeInt, atRecord4
    {$IFNDEF FPC}, atStaticArray4{$ENDIF}: PUInt32(ResultData)^ := RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 4, nil);
    atInt64: begin
      EAX := RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 4, @EDX);
      PInt64(ResultData)^ := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
    end;
    atFloat32: begin
      FloatResult := RealFloatCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4);
      PSingle(ResultData)^ := FloatResult;
    end;
    atFloat64: begin
      FloatResult := RealFloatCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4);
      PDouble(ResultData)^ := FloatResult;
    end;
    atSet3, atSet4: PUInt32(ResultData)^ := RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 4, nil);
    atRecord3{$IFNDEF FPC}, atStaticArray3{$ENDIF}:
    begin
      RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 0, nil);
      Move(PUInt32(StackPtr)^, PUInt32(ResultData)^, 3);
    end;
  else
    RealCall_Register(ProcAddr, Context.EAX, Context.EDX, Context.ECX, StackPtr, StackLength div 4, 0, nil);
  end;
end;

end.
