unit NativeCallARM32;

{$i compilers.inc}

{ implementation of the arm procedure call standard for function calls in pascal script
  Copyright (c) 2008 by Henry Vermaak (henry.vermaak@gmail.com)

  Support for EABI baseline and EABI hardfloat (c) 2015
    modulo7 ( https://github.com/modulo7 )
    Peter Michael Green ( plugwash@p10link.net )

  todo: add wince support

  notes:

  most arm cpus don't allow unaligned access.  by default (?) the linux kernel
  is set up to try and correct unaligned access, which can lead to strange behaviour.
  to turn this off, try (as root):

  echo 4 > /proc/cpu/alignment

  if you have an alignment problem, you will now get a crash with a backtrace like this:
  (make sure you compile with -O- -gl)

  An unhandled exception occurred at $0006C014 :
  EBusError : Bus error or misaligned data access
    $0006C014  PROCESSREPEAT,  line 9670 of upscompiler.pas
    $00068AAC  TPSPASCALCOMPILER__PROCESSSUB,  line 10459 of upscompiler.pas
    $0007D0B4  TPSPASCALCOMPILER__COMPILE,  line 11704 of upscompiler.pas

  you can fix this by using the "unaligned" keyword around the pointer operation.
  search for occurances of "unaligned" to see how this is done,
  (use $ifdef FPC_REQUIRES_PROPER_ALIGNMENT).

  for more information, visit:

  http://www.aleph1.co.uk/oldsite/armlinux/book/afaq.html
}   

interface

uses SysUtils, NativeCalls;

type
  TArgList = array of NativeInt;

procedure ARM32Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgList; const Data: PNativeUInt); overload;
procedure ARM32Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgList; const Data: PNativeUInt; const ResultData: PNativeUInt; const ResultInfo: TArgTypeID); overload;

implementation

const
  rtINT = 0;
  rtFLOAT = 2;

type
  TIntRegisters = array[1..4] of NativeUInt;
{$IFDEF FPC_abi_eabihf}
  Trfloat = record case byte of
          1:(s:array[0..15] of single);
          2:(d:array[0..7] of double);
          end;
{$ELSE}
  Trfloat = array[1..4] of double;
{$ENDIF}

{ define labels }
label
  stack_loop,
  load_regs,
  asmcall_end;

{ call a function from a pointer }
{ resulttype: 0 = int, 1 = int64, 2 = float }
function armasmcall(constref rint: TIntRegisters; constref rfloat: Trfloat; proc, stack: pointer; stacksize, resulttype: integer): int64; assembler; nostackframe;
asm
	mov	r12, r13
	stmfd	r13!, {r4, r5, r6, r7, r8, r9, r10, r11, r12, r14, r15}
	sub	r11, r12, #4
	mov	r4, #80			(* space for preserved registers and parameters *)
	ldr	r5, [r11, #4]		(* stacksize we need for subroutine *)
	add	r4, r4, r5
	sub	r13, r13, r4		(* create stack space *)

{$ifdef FPC_abi_eabi}
	(* EABI requires 8 byte aligned stack pointer for procedure calls, ensure alignment. *)
	bic	r13, r13, #7
{$endif}

	(* store parameters on stack *)
	str	r0, [r11, #-44]		(* rint *)
	str	r1, [r11, #-48]		(* rfloat *)
	str	r2, [r11, #-52]		(* proc *)
	str	r3, [r11, #-56] 	(* stack *)
	ldr	r0, [r11, #4]
	str	r0, [r11, #-60]		(* stacksize *)
	ldr	r0, [r11, #8]		
	str	r0, [r11, #-64]		(* resulttype *)

	(* store params for sub-routine that don't fit into r0-r3 at start of stack *)
	ldr	r0, [r11, #-60]		(* stacksize *)
	cmp	r0, #0
	beq	load_regs		(* skip if no stack *)
	mov	r1, r13			(* this points to the bottom now *)
	ldr	r2, [r11, #-56]		(* stack pointer *)
stack_loop:
	ldmia	r2!, {r4}		(* get stack + update pos *)
	stmia	r1!, {r4}		(* store stack + update pos *)
	subs	r0, r0, #4
	bne	stack_loop

load_regs:
	(* load general regs *)
	ldr	r4, [r11, #-44]	(* rint *)
	ldr	r0, [r4]
	ldr	r1, [r4, #4]
	ldr	r2, [r4, #8]
	ldr	r3, [r4, #12]

{$ifdef FPUFPA}
	(* load float regs *)
	ldr	r4, [r11, #-48]	(* rfloat *)
	ldfd	f0, [r4]
	ldfd	f1, [r4, #8]
	ldfd	f2, [r4, #16]
	ldfd	f3, [r4, #24]
{$endif}
{$ifdef FPC_abi_eabihf}
	(* load float regs *)
	ldr	r4, [r11, #-48]	(* rfloat *)
	fldmiad r4, {d0,d1,d2,d3,d4,d5,d6,d7}
{$endif}

	(* branch to the proc pointer *)
	ldr	r4, [r11, #-52]

{$ifdef FPC_abi_eabi}
	blx	r4
{$else}
	mov	r14, r15
	mov	r15, r4
{$endif}

	ldr	r4, [r11, #-64]		(* get resulttype *)
	cmp	r4, #1

	ble     asmcall_end
{$ifdef FPUFPA}
	stfd	f0, [r11, #-72]
	ldr r0, [r11, #-72]
	ldr r1, [r11, #-68]
{$endif}
{$IFDEF FPC_abi_eabihf}
	fmrrd    r0, r1, d0
{$endif}


asmcall_end:
	ldmea	r11,{r4,r5,r6,r7,r8,r9,r10,r11,r13,r15}
end;

type
  PInt8 = ^ShortInt;
  PInt16 = ^SmallInt;
  PInt32 = ^Integer;

type

  { TCallContext }

  TCallContext = record
    Regs: TIntRegisters;		// registers r0 to r3
    FloatRegs: Trfloat;		        // registers f0 to f3
    Stack: packed array of Byte;	// stack
    RIndex, FIndex, StIndex: Integer;
    Data: PNativeUInt;                   // args data ptr
    procedure Init(const Data: PNativeUInt); inline;
    procedure AddStack4(const AData: NativeUInt);
    procedure AddStack8(const AData: PInt64);
    procedure AddGen(const AData: NativeUInt);
    procedure AddArg(const Arg: NativeUInt);
  private
    procedure AddGen8(const AData: PInt64);
  end;

procedure TCallContext.AddStack4(const AData: NativeUInt);
begin
  SetLength(Stack, StIndex + 4);
  PNativeUInt(@Stack[stindex])^ := AData;
  inc(StIndex, 4);
end;

procedure TCallContext.AddStack8(const AData: PInt64);
begin
   if (StIndex and 4) <> 0 then inc(StIndex, 4);
   setlength(Stack, StIndex + 8);
   PInt64(@Stack[StIndex])^ := AData^;
   inc(StIndex, 8);
end;

procedure TCallContext.Init(const Data: PNativeUInt);
begin
  Self.Data := Data;
  RIndex := 1;
  FIndex := 1;
  StIndex := 0;
end;

procedure TCallContext.AddGen(const AData: NativeUInt);
begin
  if rindex <= 4 then begin
    Regs[rindex] := AData;
    inc(rindex);
  end else
    AddStack4(AData);
end;

{ add to the general registers or overflow to stack }
procedure TCallContext.AddGen8(const AData: PInt64);
begin
  if (rindex and 1) = 0 then
    inc(rindex);
  if rindex <= 4 then begin
    Regs[rindex] := PNativeUInt(AData)^;
    inc(rindex);
    Regs[rindex] := PNativeUInt(PByte(AData) + 4)^;
    inc(rindex);
  end else
    AddStack8(AData);
end;

procedure TCallContext.AddArg(const Arg: NativeUInt);
var
  V32: Int32;
  Size: Integer;
  Ptr: PNativeUInt;
begin
  Size := (Arg shr 8);
  //writeln('AddArg: ', TArgTypeID(Arg));
  //writeln('Data^: ', Data^);
  //writeln('Size: ', Size);
  case TArgTypeID(Arg) of
    atInt8, atInt16, atInt32,
    atFloat32, atNativeInt, atDynArray,
    atString, atInterface: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atInt64, atFloat64: begin
      AddGen8(PInt64(Data));      // аргумент int64/float64 передан по значению
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
      AddGen(NativeUInt(Data));
      Inc(Data, 2);
    end;
    atRecord1, atRecord2, atRecord3, atRecord4:
    begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atRecord5: begin
      AddGen(Data^);
      Inc(Data, 1);
      AddGen(PByte(Data)^);
      Inc(Data, 1);
    end;
    atRecord6: begin
      AddGen(Data^);
      Inc(Data, 1);
      AddGen(PWord(Data)^);
      Inc(Data, 1);
    end;
    atRecord7: begin
      AddGen(Data^);
      Inc(Data, 1);
      Move(Data^, V32, 3);
      AddGen(V32);
      Inc(Data, 1);
    end;
    atRecord8: begin
      addgen(Data^);
      Inc(Data, 1);
      addgen(Data^);
      Inc(Data, 1);
    end;
    {Структуры рамзером больше 8 байта всегда предается по указателю}
    atRecordN: begin
      Ptr := PNativeUInt(Data^);
      for V32 := 0 to (Size div 4) - 1 do
      begin
        AddGen(Ptr^);
        Inc(Ptr, 1);
      end;
      case (Size mod 4) of
        1: AddGen(PUInt8(Ptr)^);
        2: AddGen(PUInt16(Ptr)^);
        3: AddGen(PUInt32(Ptr)^ and $FFFFFF);
      end;
      Inc(Data, 1);
    end;
    {StaticArray всегда предается по указателю}
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4, atStaticArray5, atStaticArray6, atStaticArray7, atStaticArray8:
    begin
      AddGen(NativeUInt(Data));
      Inc(Data, 1);
    end;
    atStaticArrayN: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
  else
    raise Exception.CreateFmt('Invalid argument type: %s size: %d', [ord(TArgTypeID(Arg)), Size]);
  end;
end;

procedure ARM32Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgList; const Data: PNativeUInt);
var
  Context: TCallContext;
  i, ac: Integer;
begin
  //writeln('np1');
  Context.Init(Data);
  ac := Length(ArgsInfo);
  for i := 0 to ac - 1 do
    Context.AddArg(ArgsInfo[i]);
  //writeln('np2');
  armasmcall(Context.Regs, Context.FloatRegs, ProcAddr, Context.Stack, Context.stindex, rtINT);  { ignore return }
  //writeln('np3');
end;

procedure ARM32Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgList; const Data: PNativeUInt; const ResultData: PNativeUInt; const ResultInfo: TArgTypeID);
var
  Context: TCallContext;
  i, ac: Integer;
  Res: Int64;
begin
  //writeln('Result Info: ', ResultInfo);
  //writeln('Result Data: ', ResultData^);
  Context.Init(Data);
  case ResultInfo of
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4, atStaticArray5,
    atStaticArray6, atStaticArray7, atStaticArray8, atStaticArrayN: Context.AddGen(NativeUInt(ResultData));

    atDynArray, atString, atInterface, atVariant: Context.AddGen(NativeUInt(ResultData));

    atRecord5, atRecord6, atRecord7, atRecord8, atRecordN: Context.AddGen(NativeUInt(ResultData));

    atSet5, atSet6, atSet7,  atSet8, atSetN: Context.AddGen(NativeUInt(ResultData));
  end;
  ac := Length(ArgsInfo);
  for i := 0 to ac - 1 do
    Context.AddArg(ArgsInfo[i]);
  //writeln('asmc12');
  with Context do
  case ResultInfo of
    atInt8, atSet1, atRecord1: begin
      Res := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtINT);
      PInt8(ResultData)^ := Int8(Res);
    end;
    atInt16, atSet2, atRecord2: begin
      Res := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtINT);
      PInt16(ResultData)^ := Int16(Res);
    end;
    atInt32, atNativeInt, atSet4, atSet3, atRecord3, atRecord4: begin
      Res := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtINT);
      PInt32(ResultData)^ := Int32(Res);
    end;
    atInt64: PInt64(ResultData)^ := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex,rtINT);
    atFloat32: begin
      Res := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtFLOAT);
      PInt32(ResultData)^ := Int32(Res);
    end;
    atFloat64: PInt64(ResultData)^ := armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtFLOAT);
  else
    armasmcall(Regs, FloatRegs, ProcAddr, Stack, stindex, rtINT);
  end;
  //writeln('asmc2-end');
end;

end.
