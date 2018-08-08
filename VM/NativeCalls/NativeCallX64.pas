{ implementation of x64 abi }
unit NativeCallX64;

{$i compilers.inc}

interface

uses SysUtils, Classes, TypInfo, NativeCalls;

procedure X64Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal); overload;
procedure X64Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data, ResData: PCardinal; const ResDataTID: TArgTypeID); overload;

implementation

const PTR_SIZE = SizeOf(Pointer);

//procedure DebugBreak; external 'Kernel32.dll';
const
  EmptyPchar: array[0..0] of char = #0;
{$IFDEF FPC}
  {$ASMMODE INTEL}
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$DEFINE WINDOWS}
{$ENDIF}

{$DEFINE PS_RESBEFOREPARAMETERS}
{$DEFINE x64_string_result_as_varparameter}

{$IFDEF WINDOWS}
type
  TRegisters = packed record
    _RCX,                  // 0
    _RDX,                  // 8
    _R8,                   // 16
    _R9: NativeUInt;       // 24
    _XMM1,                 // 32
    _XMM2,                 // 40
    _XMM3: Double;         // 48
    Stack: Pointer;        // 56
    Items: {$IFDEF FPC}PtrUInt{$ELSE}IntPtr{$ENDIF}; // 64
    SingleBits: Integer; // 72
  end;

procedure x64call(
  Address: Pointer;
  out _RAX: NativeUInt;
  var _XMM0: Double;
  var Registers: TRegisters); assembler; {$IFDEF FPC}nostackframe;{$ENDIF}
asm
(* Registers:
    RCX: Address
    RDX: *_RAX
    R8:  * _XMM0
    R9: _REGISTERS
    fpc inserts an 20h empty space
*)
//{$IFDEF FPC}
  push rbp
  mov rbp,rsp
//{$ENDIF}
  push rcx  // address         ;rbp -8
  push rdx  // @_rax           ;rbp -16
  push r8   // @_xmm0          ;rbp -24
  push r9   // _registers      ;rbp -32

  mov rax, [rbp-32] //registers

  mov rcx, [rax+64] // items/count
  mov rdx, [rax+56] // stack
  jmp @compareitems
@work:
{$IFDEF FPC}
  push qword ptr [rdx]
{$ELSE}
  push [rdx]
{$ENDIF}
  dec rcx
  sub rdx,8
@compareitems:
  or rcx, rcx
  jnz @work

  // copy registers
  mov rcx, [rax+72] // single bits

  bt rcx, 1
  jnc @g1
  cvtsd2ss xmm1, [rax+32]
  jmp @g1e
  @g1:
  movsd xmm1, [rax+32]
  @g1e:


  bt rcx, 2
  jnc @g2
  cvtsd2ss xmm2, [rax+40]
  jmp @g2e
  @g2:
  movsd xmm2, [rax+40]
  @g2e:

  bt rcx, 3
  jnc @g3
  cvtsd2ss xmm3, [rax+48]
  jmp @g3e
  @g3:
  movsd xmm3, [rax+48]
  @g3e:



  // rbp-16: address of xmm0

  bt rcx, 0
  jnc @g0
  mov rdx, [rbp -24]
  cvtsd2ss xmm0, [rdx]
  jmp @g0e
  @g0:
  mov rdx, [rbp -24]
  movsd xmm0, [rdx]
  @g0e:

  // other registers
  mov rcx, [rax]
  mov rdx, [rax+8]
  mov r8, [rax+16]
  mov r9, [rax+24]


  mov RAX, [rbp-8]

  // weird thing on windows, it needs 32 bytes in the CALLEE side to do whatever in
  sub RSP, 32

  call RAX

  add RSP, 32 // undo the damage done earlier

  // copy result back
  mov RDX, [rbp-16]
  mov [RDX], RAX

  mov rax, [rbp-32] //registers

  bt [rax+72], 8                 // if atype.basetype  <> btSingle
  jnc @g5                        //
  cvtss2sd xmm1,xmm0             // convert single to double  into xmm1
  mov rdx,[rbp-24]               // @_xmm0  ;rbp -24
  movsd qword ptr [rdx], xmm1    // save  xmm1 to param _xmm0
  jmp @g5e                       // exit if atype.basetype  = btSingle

  @g5:                           //else "if atype.basetype  = btSingle"
    mov rdx,[rbp-24]             // @_xmm0  ;rbp -24
    movsd qword ptr [rdx], xmm0  // save  xmm1 to param _xmm0

  @g5e:


  leave
  ret
end;
{$ELSE}
type
  TRegisters = packed record
    _RDI,               //  0
    _RSI,               //  8
    _RDX,               // 16
    _RCX,               // 24
    _R8,                // 32
    _R9: NativeUInt;    // 40
    _XMM1,              // 48
    _XMM2,              // 56
    _XMM3,              // 64
    _XMM4,              // 72
    _XMM5,              // 80
    _XMM6,              // 88
    _XMM7: Double;      // 96
    SingleBits: Integer; //104
  end;

procedure x64call(
  Address: Pointer;
  out _RAX: NativeUInt;
  var Registers: TRegisters;
  aStack: Pointer; aItems: Integer; var _XMM0: Double); assembler; nostackframe;
asm
(* Registers:
    RDI: Address
    RSI: _RAX
    RDX: Registers
    RCX: aStack
    R8:  aItems
    R9:  XMM0

    rbp-8    addr
    rbp-16   _rax
    rbp-24   _xmm0
    rbp-32   regs
*)
  push rbp
  mov rbp,rsp
  push rdi  // address
  push rsi  // _rax
  push r9   // xmm0
  push rdx
{$IFDEF PS_STACKALIGN}
  bt r8, 0
  jnc @skipjump
  sub rsp, 8
@skipjump:
{$ENDIF}
  mov rax, rdx
  jmp @compareitems
@work:
{$IFDEF FPC}
  push qword ptr [rcx]
{$ELSE} 
  push [rcx]
{$ENDIF}  
  dec r8
  sub rcx,8
@compareitems:
  or r8, r8
  jnz @work

  // copy registers
  // xmm0
  mov rdx,[rbp-24]
  bt [rax+104], 0
  jnc @skipxmm0
  cvtsd2ss xmm0,[rdx]
  jmp @skipxmm0re
  @skipxmm0:
  movq xmm0,[rdx]            // move quadword to xmm0 from _XMM0
  @skipxmm0re:

  // xmm1
  bt [rax+104], 1
  jnc @skipxmm1
  cvtsd2ss xmm1,[rax+48]
  jmp @skipxmm1re
  @skipxmm1:
  movq xmm1,[rax+48]         // move quadword to xmm1 from Registers._XMM1
  @skipxmm1re:

  // xmm2
  bt [rax+104], 2
  jnc @skipxmm2
  cvtsd2ss xmm2,[rax+56]
  jmp @skipxmm2re
  @skipxmm2:
  movq xmm2,[rax+56]         // move quadword to xmm2 from Registers._XMM2
  @skipxmm2re:

  // xmm3
  bt [rax+104], 3
  jnc @skipxmm3
  cvtsd2ss xmm3,[rax+64]
  jmp @skipxmm3re
  @skipxmm3:
  movq xmm3,[rax+64]         // move quadword to xmm3 from Registers._XMM3
  @skipxmm3re:

  // xmm4
  bt [rax+104], 4
  jnc @skipxmm4
  cvtsd2ss xmm4,[rax+72]
  jmp @skipxmm4re
  @skipxmm4:
  movq xmm4,[rax+72]         // move quadword to xmm4 from Registers._XMM4
  @skipxmm4re:

  // xmm5
  bt [rax+104], 5
  jnc @skipxmm5
  cvtsd2ss xmm5,[rax+80]
  jmp @skipxmm5re
  @skipxmm5:
  movq xmm5,[rax+80]         // move quadword to xmm5 from Registers._XMM5
  @skipxmm5re:

  // xmm6
  bt [rax+104], 6
  jnc @skipxmm6
  cvtsd2ss xmm6,[rax+88]
  jmp @skipxmm6re
  @skipxmm6:
  movq xmm6,[rax+88]         // move quadword to xmm6 from Registers._XMM6
  @skipxmm6re:

// xmm7
  bt [rax+104], 7
  jnc @skipxmm7
  cvtsd2ss xmm7,[rax+96]
  jmp @skipxmm7re
  @skipxmm7:
  movq xmm7,[rax+96]         // move quadword to xmm7 from Registers._XMM7
  @skipxmm7re:


  mov RDI, [rax]
  mov RSI, [rax+ 8]
  mov RDX, [rax+16]
  mov RCX, [rax+24]
  mov R8,  [rax+32]
  mov R9,  [rax+40]

  // weird thing on windows, it needs 32 bytes in the CALLEE side to do whatever in; not sure about linux
  //sub RSP, 32

  mov rax, [rbp-8]
  call RAX

//  add rsp, 8

  // add RSP, 32 // undo the damage done earlier

  // copy result back
  mov rsi, [rbp-16]          // _RAX parameter
  mov [rsi], RAX
  mov rsi, [rbp-24]          // _XMM0 parameter

  // xmm0 res
  mov rax, [rbp-32]          // Registers parameter
  bt [rax+104], 8            // if atype.basetype  <> btSingle
  jnc @skipres               // then goto skipres else begin
  cvtss2sd xmm1,xmm0         // convert single to double  into xmm1
  movq [rsi],xmm1            // move quadword to _XMM0
  jmp @skipresre             // end
  @skipres:
  movq [rsi],xmm0            // move quadword to _XMM0
  @skipresre:


  pop rdx
  pop r9   // xmm0
  pop rsi  // _rax
  pop rdi  // address
  leave
  ret
end;
{$ENDIF}

type
  PInt8 = ^ShortInt;
  PInt16 = ^SmallInt;
  PInt32 = ^Integer;

  TCallContext = record
    _XMM0: Double;
    _RAX: NativeUInt;
    Registers: TRegisters;
    Data: PCardinal;
    Stack: array of Byte;
    RegUsage: Integer;
    {$IFNDEF WINDOWS}
    RegUsageFloat: Byte;
    {$ENDIF}
    procedure Init(const Data: PCardinal);
    procedure AddGen(const Data: NativeUInt); overload;
    procedure AddFloat64(const Data: Double); overload;
    procedure AddFloat32(const Data: Single); overload;
    procedure AddArg(const Arg: NativeUInt);
  end;

{ TCallContext }

procedure TCallContext.AddFloat32(const Data: Single);
var
  p: Pointer;
begin
  case RegUsage of
    0: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 1;_XMM0 := Data; end;
    1: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 2; Registers._XMM1 := Data; end;
    2: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 4;Registers._XMM2 := Data; end;
    3: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 8; Registers._XMM3 := Data; end;
  else
    SetLength(Stack, Length(Stack) + 8);
    p := @Stack[Length(Stack) - 8];
    Double(p^) := Data;
  end;
end;

procedure TCallContext.AddFloat64(const Data: Double);
var
  p: Pointer;
begin
  case RegUsage of
    0: begin inc(RegUsage); _XMM0 := Data; end;
    1: begin inc(RegUsage); Registers._XMM1 := Data; end;
    2: begin inc(RegUsage); Registers._XMM2 := Data; end;
    3: begin inc(RegUsage); Registers._XMM3 := Data; end;
  else
    SetLength(Stack, Length(Stack) + 8);
    p := @Stack[Length(Stack) - 8];
    Double(p^) := Data;
  end;
end;

procedure TCallContext.AddGen(const Data: NativeUInt);
var p: Pointer;
begin
  case RegUsage of
    0: begin inc(RegUsage); Registers._RCX := Data; end;
    1: begin inc(RegUsage); Registers._RDX := Data; end;
    2: begin inc(RegUsage); Registers._R8 := Data; end;
    3: begin inc(RegUsage); Registers._R9 := Data; end;
  else
    SetLength(Stack, Length(Stack) + 8);
    p := @Stack[Length(Stack) - 8];
    NativeInt(p^) := Data;
  end;
end;

procedure TCallContext.Init(const Data: PCardinal);
begin
  FillChar(Self, Sizeof(Self), 0);
  Self.Data := Data;
end;

procedure TCallContext.AddArg(const Arg: NativeUInt);
var
  ID: TArgTypeID;
begin
  ID := TArgTypeID(Arg);
  case ID of
    atInt8,
    atInt16,
    atInt32: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atNativeInt, atInt64, atDynArray, atString, atInterface: begin
      AddGen(PNativeUInt(Data)^);
      Inc(Data, 2);
    end;
    atFloat32: begin
      AddFloat32(PSingle(Data)^);
      Inc(Data, 1);
    end;
    atFloat64: begin
      AddFloat64(PDouble(Data)^);
      Inc(Data, 2);
    end;
    atVariant: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    {$IFDEF FPC}
    atSet1, atSet2, atSet3, atSet4, atSetN: begin AddGen(Data^); Inc(Data); end;
    atSet5, atSet6, atSet7, atSet8: begin AddGen(NativeInt(Data)); Inc(Data); end;
    atRecord1, atRecord2, atRecord3, atRecord4, atRecord5, atRecord6, atRecord7, atRecord8, atRecordN, atStaticArrayN: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4,
    atStaticArray5, atStaticArray6, atStaticArray7, atStaticArray8: begin
      AddGen(NativeInt(Data));
      Inc(Data, 1);
    end;
    {$ELSE}

    atSet1, atSet2, atSet3, atSet4,

    atRecord1, atRecord2, atRecord4,
    atStaticArray1, atStaticArray2, atStaticArray4: begin
      AddGen(Data^);
      Inc(Data, 1);
    end;


    atRecord3, atRecord5, atRecord6, atRecord7,
    atStaticArray3, atStaticArray5, atStaticArray6, atStaticArray7: begin
      AddGen(NativeUInt(Data));
      Inc(Data, 2);
    end;

        atSet5, atSet6, atSet7, atSet8, atSetN,
    atRecord8, atRecordN,
    atStaticArray8, atStaticArrayN: begin
      AddGen(PNativeUInt(Data)^);
      Inc(Data, 2);
    end;
    {$ENDIF}
  else
    raise Exception.CreateFmt('Invalid argument type: %s', [TypInfo.GetEnumName(TypeInfo(TArgTypeID), Ord(ID))]);
  end;
end;

procedure X64Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data: PCardinal); overload;
var
  CC: TCallContext;
  i, ac, StackLength: Integer;
  StackPtr: Pointer;
begin
  CC.Init(Data);
  ac := Length(ArgsInfo);

  for i := 0 to ac - 1 do
    CC.AddArg(ArgsInfo[i]);

  StackLength := Length(CC.Stack);
  {$IFDEF WINDOWS}
  if (StackLength mod 16) <> 0 then begin
    StackLength := StackLength + 16 - (StackLength mod 16);
    SetLength(CC.Stack, StackLength);
  end;
  {$ENDIF}
  if CC.Stack = nil then
    StackPtr := nil
  else
    StackPtr := addr(CC.Stack[StackLength - 8]);

  {$IFDEF WINDOWS}
  CC.Registers.Stack := StackPtr;
  CC.Registers.Items := StackLength div 8;
  x64call(ProcAddr, CC._RAX, CC._XMM0, CC.Registers);
  {$ELSE}
  x64call(ProcAddr, CC._RAX, CC.Registers, StackPtr, StackLength div 8, _XMM0);
  {$ENDIF}
end;

type
  PUInt8 = ^UInt8;
  PUInt16 = ^UInt16;
  PUInt32 = ^UInt32;
  PUInt64 = ^UInt64;

procedure X64Invoke(const ProcAddr: Pointer; const ArgsInfo: TArgs.TArgList; const Data, ResData: PCardinal; const ResDataTID: TArgTypeID); overload;
var
  CC: TCallContext;
  i, ac, StackLength: Integer;
  StackPtr: Pointer;
begin
  CC.Init(Data);
  ac := Length(ArgsInfo);

  if (ResDataTID = atFloat32) then
    CC.Registers.Singlebits := CC.Registers.Singlebits or 256;

  case ResDataTID of
    atString, atInterface, atDynArray, atVariant: CC.AddGen(NativeUInt(ResData));
    {$IFDEF FPC}
    atSet5, atSet6, atSet7, atSet8, atSetN: CC.AddGen(NativeUInt(ResData));
    atStaticArray1, atStaticArray2, atStaticArray3, atStaticArray4, atStaticArray5,
    atStaticArray6, atStaticArray7, atStaticArray8, atStaticArrayN: CC.AddGen(NativeUInt(ResData));
    {$ELSE}
    atSetN: CC.AddGen(NativeUInt(ResData));
    atStaticArray3, atStaticArray5, atStaticArray6, atStaticArray7, atStaticArrayN: CC.AddGen(NativeUInt(ResData));
    {$ENDIF}
    atRecord3, atRecord5, atRecord6, atRecord7, atRecordN: CC.AddGen(NativeUInt(ResData));
  end;

  for i := 0 to ac - 1 do
    CC.AddArg(ArgsInfo[i]);

  StackLength := Length(CC.Stack);
  {$IFDEF WINDOWS}
  if (StackLength mod 16) <> 0 then begin
    StackLength := StackLength + 16 - (StackLength mod 16);
    SetLength(CC.Stack, StackLength);
  end;
  {$ENDIF}
  if CC.Stack = nil then
    StackPtr := nil
  else
    StackPtr := addr(CC.Stack[StackLength - 8]);

  {$IFDEF WINDOWS}
  CC.Registers.Stack := StackPtr;
  CC.Registers.Items := StackLength div 8;
  x64call(ProcAddr, CC._RAX, CC._XMM0, CC.Registers);
  {$ELSE}
  x64call(ProcAddr, CC._RAX, CC.Registers, StackPtr, StackLength div 8, _XMM0);
  {$ENDIF}

  case ResDataTID of
    atInt8: PUInt8(ResData)^ := UInt8(CC._RAX);
    atInt16: PUInt16(ResData)^ := UInt16(CC._RAX);
    atInt32: PUInt32(ResData)^ := UInt32(CC._RAX);
    atNativeInt, atInt64: PUInt64(ResData)^ := CC._RAX;
    atFloat32: PSingle(ResData)^ := CC._XMM0;
    atFloat64: PDouble(ResData)^ := CC._XMM0;
    {$IFNDEF FPC}
    atStaticArray1: PUInt8(ResData)^ := UInt8(CC._RAX);
    atStaticArray2: PUInt16(ResData)^ := UInt16(CC._RAX);
    atStaticArray4: PUInt32(ResData)^ := UInt32(CC._RAX);
    atStaticArray8: PUInt64(ResData)^ := CC._RAX;
    atRecord8, atSet8: PUInt64(ResData)^ := CC._RAX;
    {$ENDIF}
    atRecord1, atSet1: PUInt8(ResData)^ := UInt8(CC._RAX);
    atRecord2, atSet2: PUInt16(ResData)^ := UInt16(CC._RAX);
    atRecord4, atSet4: PUInt32(ResData)^ := UInt32(CC._RAX);
    atSet3: Move(CC._RAX, ResData^, 3);
    atSet6: Move(CC._RAX, ResData^, 6);
    atSet7: Move(CC._RAX, ResData^, 7);
  end;
end;


end.
