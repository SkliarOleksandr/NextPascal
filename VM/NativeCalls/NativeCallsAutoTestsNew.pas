unit NativeCallsAutoTestsNew;

interface

{$IFDEF FPC}
  {$MODE DELPHIUNICODE}
{$ENDIF}

uses SysUtils, NativeCalls;

procedure RunALlTests;

implementation

{$SCOPEDENUMS ON}

type
  TEnum8 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07);
  TEnum16 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15);
  TEnum24 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23);
  TEnum32 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31);
  TEnum48 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31,
             _a32,_a33,_a34,_a35,_a36,_a37,_a38,_a39,_a40,_a41,_a42,_a43,_a44,_a45,_a46,_a47);
  TEnum56 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31,
             _a32,_a33,_a34,_a35,_a36,_a37,_a38,_a39,_a40,_a41,_a42,_a43,_a44,_a45,_a46,_a47,_a48,_a49,_a50,_a51,_a52,_a53,_a54,_a55);
  TEnum64 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31,
             _a32,_a33,_a34,_a35,_a36,_a37,_a38,_a39,_a40,_a41,_a42,_a43,_a44,_a45,_a46,_a47,_a48,_a49,_a50,_a51,_a52,_a53,_a54,_a55,_a56,_a57,_a58,_a59,_a60,_a61,_a62,_a63);

  TSet8 = packed set of TEnum8;
  TSet16 = packed set of TEnum16;
  TSet24 = packed set of TEnum24;
  TSet32 = packed set of TEnum32;
  TSet48 = packed set of TEnum48;
  TSet56 = packed set of TEnum56;
  TSet64 = packed set of TEnum64;
  TSet80 = packed set of 0..79;

  TRect = record
    Left, Top, Right, Bottom: Int32;
    //Left1, Top1, Right1, Bottom1: Int32;
  end;

  TSmallRect1 = packed record x: byte; end;
  TSmallRect2 = packed record x, y: byte; end;
  TSmallRect3 = packed record x, y, z: byte; end;
  TSmallRect4 = packed record x, y, z, w: byte; end;
  TSmallRect5 = packed record x, y, z, w, q: byte; end;
  TSmallRect6 = packed record x, y, z, w, q, a: byte; end;
  TSmallRect7 = packed record x, y, z, w, q, a, b: byte; end;
  TSmallRect8 = packed record x, y, z, w, q, a, b, c: byte; end;
  TSmallRect9 = packed record x, y: int32; z: byte; end;
  TSmallRect10 = packed record x, y: int32; z: word; end;
  TSmallRect11 = packed record x, y: int32; z: word; w: byte; end;

  TSArray = array[0..9] of int32;
  TSmallSArray1 = packed array [0..0] of Byte;
  TSmallSArray2 = packed array [0..1] of Byte;
  TSmallSArray3 = packed array [0..2] of Byte;
  TSmallSArray4 = packed array [0..3] of Byte;
  TSmallSArray5 = packed array [0..4] of Byte;
  TSmallSArray6 = packed array [0..5] of Byte;
  TSmallSArray7 = packed array [0..6] of Byte;
  TSmallSArray8 = packed array [0..7] of Byte;

  TDynArrayInt = array of Integer;

  TMyClass = class
  private
    FData: Integer;
  public
    procedure Method1;
    function Method_i32(a: Int32): Int32;
    constructor Create; overload;
    constructor Create2(a, b, c: Integer); overload;
  end;

var
  I32: Int32;
  I64: Int64;
  F32: Single;
  F64: Double;
  USTR: string;
  VVAR: Variant;
  ASTR: AnsiString;
  Set8: TSet8;
  Set16: TSet16;
  Set32: TSet32;
  Set64: TSet64;
  Set80: TSet80;

procedure NCall(ProcAddr: Pointer; const Args: TArgs);
var
  Ptr: PNativeUInt;
begin
  if Length(Args.Data) > 0 then
    Ptr := @Args.Data[0]
  else
    Ptr := nil;
  if not Args.HasResult then
    NativeCallNew(ProcAddr, Args.Args, Ptr)
  else
    NativeCallNew(ProcAddr, Args.Args, Ptr, Args.ResultPtr, Args.ResultTID);
end;

procedure cbReg_proc_i8(a: int8); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := a;
end;

procedure cbReg_proc_USTR(a: string); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  USTR := a;
end;

procedure cbReg_proc_ASTR(a: AnsiString); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  ASTR := a;
end;

procedure cbReg_proc_F32(a: Single); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  F32 := a;
end;

procedure cbReg_proc_F64(a: Double); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  F64 := a;
end;

procedure cbReg_proc_ref_i8(var a: int8); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  a := a + 1;
end;

procedure cbReg_proc_variant(a: Variant); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  VVAR := a;
end;

procedure cbReg_proc_set8(a: TSet8); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Set8 := a;
end;

procedure cbReg_proc_set16(a: TSet16); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Set16 := a;
end;

procedure cbReg_proc_set32(a: TSet32); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Set32 := a;
end;

procedure cbReg_proc_set64(a: TSet64); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Set64 := a;
end;

procedure cbReg_proc_set80(a: TSet80); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Set80 := a;
end;

procedure cbReg_proc_i8_5(a, b, c, d, e: int8); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := a + b + c + d + e;
end;

procedure cbReg_proc_i16(i: int16); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := i;
end;

procedure cbReg_proc_i16_5(a, b, c, d, e: int16); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := a + b + c + d + e;
end;

procedure cbReg_proc_i32(i: int32); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := i;
end;

procedure cbReg_proc_i32_5(a, b, c, d, e: int32); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I32 := a + b + c + d + e;
end;

procedure cbReg_proc_i64(i: int64); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I64 := i;
end;

procedure cbReg_proc_i64_5(a, b, c, d, e: int64); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I64 := a + b + c + d + e;
end;

function cbReg_func_i32: Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := 5;
end;

function cbReg_func_i32_5(a, b, c, d, e: int32): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b + c + d + e;
end;

function cbReg_func_i64: Int64; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := 555555555555;
end;

function cbReg_func_f32: Single; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := 5.111;
end;

function cbReg_func_f64: Double; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := 5.11111111;
end;

function cbReg_func_DynArray(a: TDynArrayInt): TDynArrayInt; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  SetLength(Result, Length(a) + 1);
end;

function cbReg_func_variant(a: Variant): Variant; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_variant_sum(a, b: Variant): Variant; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b;
end;

function cbReg_func_int_i32(a: int32): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_F64Sum(a, b: Double): Double; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b;
end;

function cbReg_func_F32Sum(a, b: Single): Single; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b;
end;

function cbReg_func_USTRSum(a, b: string): string; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b;
end;

function cbReg_func_set8(a: TSet8): TSet8; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set16(a: TSet16): TSet16; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set24(a: TSet24): TSet24; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set32(a: TSet32): TSet32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set48(a: TSet48): TSet48; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set56(a: TSet56): TSet56; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set64(a: TSet64): TSet64; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_set80(a: TSet80): TSet80; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TRect(a: TRect): TRect; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect1(a: TSmallRect1): TSmallRect1; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect2(a: TSmallRect2): TSmallRect2; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect3(a: TSmallRect3): TSmallRect3; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect4(a: TSmallRect4): TSmallRect4; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect5(a: TSmallRect5): TSmallRect5; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect6(a: TSmallRect6): TSmallRect6; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect7(a: TSmallRect7): TSmallRect7; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect8(a: TSmallRect8): TSmallRect8; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect9(a: TSmallRect9): TSmallRect9; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect10(a: TSmallRect10): TSmallRect10; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallRect11(a: TSmallRect11): TSmallRect11; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray1(a: TSmallSArray1): TSmallSArray1; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray2(a: TSmallSArray2): TSmallSArray2; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray3(a: TSmallSArray3): TSmallSArray3; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray4(a: TSmallSArray4): TSmallSArray4; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray5(a: TSmallSArray5): TSmallSArray5; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray6(a: TSmallSArray6): TSmallSArray6; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray7(a: TSmallSArray7): TSmallSArray7; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSmallSArray8(a: TSmallSArray8): TSmallSArray8; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ret_TSArray(a: TSArray): TSArray; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_sum_TSArrays(a, b: TSArray): TDynArrayInt; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: Integer;
begin
  SetLength(Result, Length(a) + Length(b));
  for i := 0 to Length(a) - 1 do
    Result[i] := a[i];
  for i := 0 to Length(b) - 1 do
    Result[Length(a) + i] := Result[Length(a) + i] + b[i];
end;

procedure cbReg_proc_int_all(a1: int8;
                             a2: uint8;
                             a3: int16;
                             a4: uint16;
                             a5: int32;
                             a6: uint32;
                             a7: int64;
                             a8: uint64;
                             a9: AnsiChar;
                             a10: WideChar); {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  I64 := a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + ord(a9) + ord(a10);
end;

function cbReg_func_TRect_width(a: TRect): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.Right - a.Left;
end;

function cbReg_func_TSmallRect1(a: TSmallRect1): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.x;
end;

function cbReg_func_TSmallRect2(a: TSmallRect2): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.x + a.y;
end;

function cbReg_func_TSmallRect3(a: TSmallRect3): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.x + a.y + a.z;
end;

function cbReg_func_TSmallRect4(a: TSmallRect4): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.x + a.y + a.z + a.w;
end;

function cbReg_func_TSmallRect5(a: TSmallRect5): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a.x + a.y + a.z + a.w + a.q;
end;

function cbReg_func_TStaticArray_sum(a: TSArray): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_TSmallStaticArray1_sum(a: TSmallSArray1): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_TSmallStaticArray2_sum(a: TSmallSArray2): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_TSmallStaticArray3_sum(a: TSmallSArray3): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_TSmallStaticArray4_sum(a: TSmallSArray4): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_TSmallStaticArray5_sum(a: TSmallSArray5): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_DynArrayOfInt_sum(a: TDynArrayInt): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: int32;
begin
  Result := 0;
  for i := 0 to Length(a) - 1 do
    Result := Result + a[i];
end;

function cbReg_func_USTR(a: string): string; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_ASTR(a: AnsiString): AnsiString; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_func_INTF(a: IInterface): IInterface; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a;
end;

function cbReg_speed_test1(a, b, c: Int32): Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := a + b + c;
end;

function cbReg_func_mix_0(a: TSmallRect1; b: TSmallSArray1): TRect; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result.Left := SizeOF(a) + SizeOF(b);
end;

function cbReg_func_mix_1(a1: TSmallRect1;
                          a2: TSmallSArray1;
                          a3: TSet8;
                          a4: TRect): TRect; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result.Left := SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + SizeOf(a4);
end;

function cbReg_func_mix_2(a1: TSmallRect1;
                          a2: TSmallSArray1;
                          a3: TSet8;
                          a4: TDynArrayInt;
                          a5: TSArray;
                          a6: TRect;
                          a7: string;
                          a8: AnsiString;
                          a9: IInterface): TRect; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result.Left := SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + Length(a4) + SizeOF(a5) + SizeOF(a6) + SizeOF(a7) + Length(a7) + Length(a8) + SizeOf(A9);
end;

procedure test_cbReg_proc_i8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
begin
  Args.Init;
  Args.Add<Int8>(5);
  NCall(@cbReg_proc_i8, Args);
  Assert(i32 = 5);
  WriteLn('test_cbReg_proc_i8: done');
end;

procedure test_cbReg_proc_ref_i8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Int8;
begin
  a := 55;
  Args.Init;
  Args.Add<Int8>(a, True);
  NCall(@cbReg_proc_ref_i8, Args);
  Assert(a = 56);
  WriteLn('test_cbReg_proc_ref_i8: done');
end;

procedure test_cbReg_proc_USTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  s: string;
begin
  s := 'abcd';
  Args.Init;
  Args.Add<string>(s);
  NCall(@cbReg_proc_USTR, Args);
  Assert(USTR = s);
  WriteLn('test_cbReg_proc_USTR: done');
end;

procedure test_cbReg_proc_ASTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  s: AnsiString;
begin
  s := 'abcd';
  Args.Init;
  Args.Add<ansistring>(s);
  NCall(@cbReg_proc_ASTR, Args);
  Assert(ASTR = s);
  WriteLn('test_cbReg_proc_ASTR: done');
end;

procedure test_cbReg_proc_F32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Single;
begin
  a := 5.12;
  Args.Init;
  Args.Add<Single>(a);
  NCall(@cbReg_proc_F32, Args);
  Assert(F32 = a);
  WriteLn('test_cbReg_proc_F32: done');
end;

procedure test_cbReg_proc_F64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Double;
begin
  a := 5.12345678;
  Args.Init;
  Args.Add<Double>(a);
  NCall(@cbReg_proc_F64, Args);
  Assert(F64 = a);
  WriteLn('test_cbReg_proc_F64: done');
end;

procedure test_cbReg_proc_variant(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Variant;
begin
  a := 1234;
  Args.Init;
  Args.Add<Variant>(a);
  NCall(@cbReg_proc_variant, Args);
  Assert(VVAR = a);
  WriteLn('test_cbReg_proc_variant: done');
end;

procedure test_cbReg_proc_set8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSet8;
begin
  a := [TEnum8._a01];
  Args.Init;
  Args.Add<TSet8>(a);
  NCall(@cbReg_proc_set8, Args);
  Assert(Set8 = a);
  WriteLn('test_cbReg_proc_set8: done');
end;

procedure test_cbReg_proc_set16(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSet16;
begin
  a := [TEnum16._a02];
  Args.Init;
  Args.Add<TSEt16>(a);
  NCall(@cbReg_proc_set16, Args);
  Assert(Set16 = a);
  WriteLn('test_cbReg_proc_set16: done');
end;

procedure test_cbReg_proc_set32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSet32;
begin
  a := [TEnum32._a03];
  Args.Init;
  Args.Add<TSet32>(a);
  NCall(@cbReg_proc_set32, Args);
  Assert(Set32 = a);
  WriteLn('test_cbReg_proc_set16: done');
end;

procedure test_cbReg_proc_set64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSet64;
begin
  a := [TEnum64._a04];
  Args.Init;
  Args.Add<TSet64>(a);
  NCall(@cbReg_proc_set64, Args);
  Assert(Set64 = a);
  WriteLn('test_cbReg_proc_set64: done');
end;

procedure test_cbReg_proc_set80(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSet80;
begin
  a := [79];
  Args.Init;
  Args.Add<TSet80>(a);
  NCall(@cbReg_proc_set80, Args);
  Assert(Set80 = a);
  WriteLn('test_cbReg_proc_set80: done');
end;

procedure test_cbReg_func_variant(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: Variant;
begin
  r := 111;
  a := 333;
  Args.InitWithResult<Variant>(r);
  Args.Add<Variant>(a);
  NCall(@cbReg_func_variant, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_variant: done');
end;

procedure test_cbReg_func_variant_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a, b: Variant;
begin
  a := 3333;
  b := 3333;
  Args.InitWithResult<Variant>(r);
  Args.Add<Variant>(a);
  Args.Add<Variant>(b);
  NCall(@cbReg_func_variant_sum, Args);
  Assert(r = a + b);
  WriteLn('test_cbReg_func_variant_sum: done');
end;

procedure test_cbReg_proc_i8_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
begin
  Args.Init;
  Args.Add<int8>(1);
  Args.Add<int8>(2);
  Args.Add<int8>(3);
  Args.Add<int8>(4);
  Args.Add<int8>(5);
  NCall(@cbReg_proc_i8_5, Args);
  Assert(i32 = 15);
  WriteLn('test_cbReg_proc_i8_5: done');
end;

procedure test_cbReg_proc_i16(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Int16;
begin
  a := 55;
  Args.Init;
  Args.Add<int16>(a);
  NCall(@cbReg_proc_i16, Args);
  Assert(i32 = a);
  WriteLn('test_cbReg_proc_i16: done');
end;

procedure test_cbReg_proc_i16_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, c, d, e: Int16;
begin
  a := 100;
  b := 200;
  c := 300;
  d := 400;
  e := 500;
  Args.Init;
  Args.Add<int16>(a);
  Args.Add<int16>(b);
  Args.Add<int16>(c);
  Args.Add<int16>(d);
  Args.Add<int16>(e);
  NCall(@cbReg_proc_i16_5, Args);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i16_5: done');
end;

procedure test_cbReg_proc_i32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Int32;
begin
  a := 55;
  Args.Init;
  Args.Add<int32>(55);
  NCall(@cbReg_proc_i32, Args);
  Assert(i32 = a);
  WriteLn('test_cbReg_proc_i32: done');
end;

procedure test_cbReg_proc_i32_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, c, d, e: Int32;
begin
  a := 100000;
  b := 200000;
  c := 300000;
  d := 400000;
  e := 500000;
  Args.Init;
  Args.Add<int32>(a);
  Args.Add<int32>(b);
  Args.Add<int32>(c);
  Args.Add<int32>(d);
  Args.Add<int32>(e);
  NCall(@cbReg_proc_i32_5, Args);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i32_5: done');
end;

procedure test_cbReg_proc_i32_5s(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, c, d, e: Int32;
begin
  a := -1;
  b := -500;
  c := -80000;
  d := -4000000;
  e := -2000000000;
  Args.Init;
  Args.Add<int32>(a);
  Args.Add<int32>(b);
  Args.Add<int32>(c);
  Args.Add<int32>(d);
  Args.Add<int32>(e);
  NCall(@cbReg_proc_i32_5, Args);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i32_5s: done');
end;

procedure test_cbReg_proc_i64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Int64;
begin
  a := 55;
  Args.Init;
  Args.Add<int64>(a);
  NCall(@cbReg_proc_i64, Args);
  Assert(i64 = a);
  WriteLn('test_cbReg_proc_i64: done');
end;

procedure test_cbReg_proc_i64_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, c, d, e: Int64;
begin
  a := 100000000;
  b := 200000000;
  c := 300000000;
  d := 400000000;
  e := 500000000;
  Args.Init;
  Args.Add<int64>(a);
  Args.Add<int64>(b);
  Args.Add<int64>(c);
  Args.Add<int64>(d);
  Args.Add<int64>(e);
  NCall(@cbReg_proc_i64_5, Args);
  Assert(i64 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i64_5: done');
end;

procedure test_cbReg_proc_int_all(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a1: int8;
  a2: uint8;
  a3: int16;
  a4: uint16;
  a5: int32;
  a6: uint32;
  a7: int64;
  a8: uint64;
  a9: AnsiChar;
  a10: WideChar;
begin
  a1 := 1;
  a2 := 2;
  a3 := 3;
  a4 := 4;
  a5 := 5;
  a6 := 6;
  a7 := 7;
  a8 := 8;
  a9 := 'A';
  a10 := 'B';
  Args.Init;
  Args.Add<int8>(a1);
  Args.Add<uint8>(a2);
  Args.Add<int16>(a3);
  Args.Add<uint16>(a4);
  Args.Add<int32>(a5);
  Args.Add<uint32>(a6);
  Args.Add<int64>(a7);
  Args.Add<uint64>(a8);
  Args.Add<AnsiChar>(a9);
  Args.Add<WideChar>(a10);
  NCall(@cbReg_proc_int_all, Args);
  Assert(I64 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + ord(a9) + ord(a10));
  WriteLn('test_cbReg_proc_int_all: done');
end;

procedure test_cbReg_func_i32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r: Integer;
begin
  Args.InitWithResult<int32>(r);
  NCall(@cbReg_func_i32, Args);
  Assert(r = 5);
  WriteLn('test_cbReg_func_i32: done');
end;

procedure test_cbReg_func_i64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r: Int64;
begin
  Args.InitWithResult<int64>(r);
  NCall(@cbReg_func_i64, Args);
  Assert(R = 555555555555);
  WriteLn('test_cbReg_func_i64: done');
end;

procedure test_cbReg_func_i32_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a, b, c, d, e: Cardinal;
begin
  a := 1;
  b := 500;
  c := 80000;
  d := 4000000;
  e := 3000000000;

  Args.InitWithResult<Cardinal>(r);
  Args.Add<Cardinal>(a);
  Args.Add<Cardinal>(b);
  Args.Add<Cardinal>(c);
  Args.Add<Cardinal>(d);
  Args.Add<Cardinal>(e);

  NCall(@cbReg_func_i32_5, Args);
  Assert(r = a + b + c + d + e);
  WriteLn('test_cbReg_func_i32_5: done');
end;

procedure test_cbReg_func_i32_5s(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a, b, c, d, e: Int32;
begin
  a := -1;
  b := -500;
  c := -80000;
  d := -4000000;
  e := -2000000000;

  Args.InitWithResult<int32>(r);
  Args.Add<int32>(a);
  Args.Add<int32>(b);
  Args.Add<int32>(c);
  Args.Add<int32>(d);
  Args.Add<int32>(e);

  NCall(@cbReg_func_i32_5, Args);
  Assert(r = a + b + c + d + e);
  WriteLn('test_cbReg_func_i32_5s: done');
end;

procedure test_cbReg_func_f32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b: Single;
begin
  b := 5.111;
  Args.InitWithResult<single>(a);
  NCall(@cbReg_func_f32, Args);
  Assert(a = b);
  WriteLn('test_cbReg_func_f32: done');
end;

procedure test_cbReg_func_f64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: Double;
begin
  a := 5.11111111;
  Args.InitWithResult<Double>(r);
  Args.Add<Double>(a);
  NCall(@cbReg_func_f64, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_f64: done');
end;

procedure test_cbReg_func_int_i32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: int32;
begin
  a := 12;
  Args.InitWithResult<Int32>(r);
  Args.Add<Int32>(a);
  NCall(@cbReg_func_int_i32, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_int_i32: done');
end;

procedure test_cbReg_func_F64Sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, r: Double;
begin
  a := 1.11111111;
  b := 2.22222222;
  r := 0;
  Args.InitWithResult<Double>(r);
  Args.Add<Double>(a);
  Args.Add<Double>(b);
  NCall(@cbReg_func_F64Sum, Args);
  Assert(r = a + b);
  WriteLn('test_cbReg_func_F64Sum: done');
end;

procedure test_cbReg_func_F32Sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, r, w: Single;
begin
  a := 1.222;
  b := 1.333;
  w := a + b;
  Args.InitWithResult<Single>(r);
  Args.Add<Single>(a);
  Args.Add<Single>(b);
  NCall(@cbReg_func_F32Sum, Args);
  Assert(r = w);
  WriteLn('test_cbReg_func_F32Sum: done');
end;

procedure test_cbReg_func_USTRSum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b, r: string;
begin
  r := '---';
  a := 'aaa';
  b := 'bbb';
  Args.InitWithResult<string>(r);
  Args.Add<string>(a);
  Args.Add<string>(b);
  NCall(@cbReg_func_USTRSum, Args);
  Assert(r = a + b);
  WriteLn('test_cbReg_func_USTRSum: done');
end;

procedure test_cbReg_func_set8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet8;
begin
  r := [TEnum8._a00];
  a := [TEnum8._a01];
  Args.InitWithResult<Tset8>(r);
  Args.Add<Tset8>(a);
  NCall(@cbReg_func_set8, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set8: done');
end;

procedure test_cbReg_func_set16(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet16;
begin
  a := [TEnum16._a02];
  Args.InitWithResult<Tset16>(r);
  Args.Add<Tset16>(a);
  NCall(@cbReg_func_set16, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set16: done');
end;

procedure test_cbReg_func_set24(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet24;
begin
  a := [TEnum24._a03];
  Args.InitWithResult<Tset24>(r);
  Args.Add<Tset24>(a);
  NCall(@cbReg_func_set24, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set24: done');
end;

procedure test_cbReg_func_set32(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet32;
begin
  a := [TEnum32._a04];
  Args.InitWithResult<Tset32>(r);
  Args.Add<Tset32>(a);
  NCall(@cbReg_func_set32, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set32: done');
end;

procedure test_cbReg_func_set48(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet48;
begin
  a := [TEnum48._a05];
  Args.InitWithResult<Tset48>(r);
  Args.Add<Tset48>(a);
  NCall(@cbReg_func_set48, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set48: done');
end;

procedure test_cbReg_func_set56(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet56;
begin
  a := [TEnum56._a06];
  Args.InitWithResult<Tset56>(r);
  Args.Add<Tset56>(a);
  NCall(@cbReg_func_set56, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set56: done');
end;

procedure test_cbReg_func_set64(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet64;
begin
  a := [TEnum64._a07];
  Args.InitWithResult<Tset64>(r);
  Args.Add<Tset64>(a);
  NCall(@cbReg_func_set64, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set64: done');
end;

procedure test_cbReg_func_set80(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSet80;
begin
  a := [54];
  Args.InitWithResult<Tset80>(r);
  Args.Add<Tset80>(a);
  NCall(@cbReg_func_set80, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_set80: done');
end;

procedure test_cbReg_method(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  try
    Args.Init;
    Args.Add<TMyClass>(Obj);
    NCall(@TMyClass.Method1, Args);
    Assert(I32 = 111);
  finally
    Obj.Free;
  end;
  WriteLn('test_cbReg_method: done');
end;

procedure test_cbReg_method_i32(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TArgs;
  r, a: Int32;
begin
  Obj := TMyClass.Create;
  try
    a := 12;
    Args.InitWithResult<int32>(r);
    Args.Add<TMyClass>(Obj);
    Args.Add<int32>(a);
    NCall(@TMyClass.Method_i32, Args);
    Assert(r = a);
  finally
    Obj.Free;
  end;
  WriteLn('test_cbReg_method_i32: done');
end;

procedure test_cbReg_obj_create_0(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TArgs;
  cl: TClass;
begin
  cl := TMyClass;
  Args.InitWithResult<TMyClass>(Obj);
  Args.Add<TClass>(cl);
  Args.Add<int8>(1);
  NCall(@TMyClass.Create, Args);
  Assert(Obj.FData = 1);
  Obj.Free;
  WriteLn('test_cbReg_obj_create_0: done');
end;

procedure test_cbReg_obj_create_1(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TArgs;
  cl: TClass;
  a, b, c: Integer;
begin
  cl := TMyClass;
  a := 1;
  b := 2;
  c := 3;
  Args.InitWithResult<TMyClass>(Obj);
  Args.Add<TClass>(cl);
  Args.Add<int32>(1);
  Args.Add<int32>(a);
  Args.Add<int32>(b);
  Args.Add<int32>(c);
  NCall(@TMyClass.Create2, Args);
  Assert(Obj.FData = 6);
  Obj.Free;
  WriteLn('test_cbReg_obj_create_1: done');
end;

procedure test_cbReg_func_TRect_width(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: Integer;
  r: TRect;
begin
  r.Left := 10;
  r.Right := 30;
  Args.InitWithResult<int32>(a);
  Args.Add<TRect>(r);
  NCall(@cbReg_func_TRect_width, Args);
  Assert(a = 20);
  WriteLn('test_cbReg_func_TRect_width: done');
end;

procedure test_cbReg_func_TSmallRect1(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r: TSmallRect1;
  a: Int32;
begin
  r.x := 11;
  Args.InitWithResult<int32>(a);
  Args.Add<TSmallRect1>(r);
  NCall(@cbReg_func_TSmallRect1, Args);
  Assert(a = 11);
  WriteLn('test_cbReg_func_TSmallRect1: done');
end;

procedure test_cbReg_func_TSmallRect2(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r: TSmallRect2;
  a: Integer;
begin
  r.x := 10;
  r.y := 20;
  Args.InitWithResult<int32>(a);
  Args.Add<TSmallRect2>(r);
  NCall(@cbReg_func_TSmallRect2, Args);
  Assert(a = 30);
  WriteLn('test_cbReg_func_TSmallRect2: done');
end;

procedure test_cbReg_func_TSmallRect3(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallRect3;
  r: Integer;
begin
  a.x := 10;
  a.y := 20;
  a.z := 30;
  Args.InitWithResult<int32>(r);
  Args.Add<TSmallRect3>(a);
  NCall(@cbReg_func_TSmallRect3, Args);
  Assert(r = 60);
  WriteLn('test_cbReg_func_TSmallRect3: done');
end;

procedure test_cbReg_func_TSmallRect4(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r: TSmallRect4;
  a: Integer;
begin
  r.x := 10;
  r.y := 20;
  r.z := 30;
  r.w := 40;
  Args.InitWithResult<int32>(a);
  Args.Add<TSmallRect4>(r);
  NCall(@cbReg_func_TSmallRect4, Args);
  Assert(a = 100);
  WriteLn('test_cbReg_func_TSmallRect4: done');
end;

procedure test_cbReg_func_TSmallRect5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallRect5;
  r: Integer;
begin
  a.x := 10;
  a.y := 20;
  a.z := 30;
  a.w := 40;
  a.q := 50;
  Args.InitWithResult<int32>(r);
  Args.Add<TSmallRect5>(a);
  NCall(@cbReg_func_TSmallRect5, Args);
  Assert(r = 150);
  WriteLn('test_cbReg_func_TSmallRect5: done');
end;

procedure test_cbReg_func_TStaticArray_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSArray;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSArray>(a);
  NCall(@cbReg_func_TStaticArray_sum, Args);
  Assert(i = 100);
  WriteLn('test_cbReg_func_TStaticArray_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray1_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallSArray1;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSmallSArray1>(a);
  NCall(@cbReg_func_TSmallStaticArray1_sum, Args);
  Assert(i = 10);
  WriteLn('test_cbReg_func_TSmallStaticArray1_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray2_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallSArray2;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSmallSArray2>(a);
  NCall(@cbReg_func_TSmallStaticArray2_sum, Args);
  Assert(i = 20);
  WriteLn('test_cbReg_func_TSmallStaticArray2_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray3_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallSArray3;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSmallSArray3>(a);
  NCall(@cbReg_func_TSmallStaticArray3_sum, Args);
  Assert(i = 30);
  WriteLn('test_cbReg_func_TSmallStaticArray3_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray4_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallSArray4;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSmallSArray4>(a);
  NCall(@cbReg_func_TSmallStaticArray4_sum, Args);
  Assert(i = 40);
  WriteLn('test_cbReg_func_TSmallStaticArray4_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray5_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallSArray5;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TSmallSArray5>(a);
  NCall(@cbReg_func_TSmallStaticArray5_sum, Args);
  Assert(i = 50);
  WriteLn('test_cbReg_func_TSmallStaticArray5_sum: done');
end;

procedure test_cbReg_func_ret_TRect(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TRect;
begin
  r.Left := 11;
  Args.InitWithResult<TRect>(a);
  Args.Add<TRect>(r);
  NCall(@cbReg_func_ret_TRect, Args);
  Assert(a.Left = r.Left);
  WriteLn('test_cbReg_func_ret_TRect: done');
end;

procedure test_cbReg_func_ret_TSmallRect1(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect1;
begin
  r.x := 12;
  Args.InitWithResult<TSmallRect1>(a);
  Args.Add<TSmallRect1>(r);
  NCall(@cbReg_func_ret_TSmallRect1, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect1: done');
end;

procedure test_cbReg_func_ret_TSmallRect2(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect2;
begin
  r.x := 13;
  Args.InitWithResult<TSmallRect2>(a);
  Args.Add<TSmallRect2>(r);
  NCall(@cbReg_func_ret_TSmallRect2, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect2: done');
end;

procedure test_cbReg_func_ret_TSmallRect3(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect3;
begin
  r.x := 14;
  Args.InitWithResult<TSmallRect3>(a);
  Args.Add<TSmallRect3>(r);
  NCall(@cbReg_func_ret_TSmallRect3, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect3: done');
end;

procedure test_cbReg_func_ret_TSmallRect4(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect4;
begin
  r.x := 15;
  Args.InitWithResult<TSmallRect4>(a);
  Args.Add<TSmallRect4>(r);
  NCall(@cbReg_func_ret_TSmallRect4, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect4: done');
end;

procedure test_cbReg_func_ret_TSmallRect5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect5;
begin
  r.x := 16;
  Args.InitWithResult<TSmallRect5>(a);
  Args.Add<TSmallRect5>(r);
  NCall(@cbReg_func_ret_TSmallRect5, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect5: done');
end;

procedure test_cbReg_func_ret_TSmallRect6(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect6;
begin
  r.x := 17;
  Args.InitWithResult<TSmallRect6>(a);
  Args.Add<TSmallRect6>(r);
  NCall(@cbReg_func_ret_TSmallRect6, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect6: done');
end;

procedure test_cbReg_func_ret_TSmallRect7(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect7;
begin
  r.x := 18;
  Args.InitWithResult<TSmallRect7>(a);
  Args.Add<TSmallRect7>(r);
  NCall(@cbReg_func_ret_TSmallRect7, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect7: done');
end;

procedure test_cbReg_func_ret_TSmallRect8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect8;
begin
  a.x := 18;
  Args.InitWithResult<TSmallRect8>(r);
  Args.Add<TSmallRect8>(a);
  NCall(@cbReg_func_ret_TSmallRect8, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect8: done');
end;

procedure test_cbReg_func_ret_TSmallRect9(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect9;
begin
  a.x := 11;
  a.y := 12;
  a.z := 13;
  r.x := 1;
  Args.InitWithResult<TSmallRect9>(r);
  Args.Add<TSmallRect9>(a);
  NCall(@cbReg_func_ret_TSmallRect9, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect9: done');
end;

procedure test_cbReg_func_ret_TSmallRect10(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect10;
begin
  a.x := 11;
  a.y := 12;
  a.z := 13;
  r.x := 1;
  Args.InitWithResult<TSmallRect10>(r);
  Args.Add<TSmallRect10>(a);
  NCall(@cbReg_func_ret_TSmallRect10, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect10: done');
end;

procedure test_cbReg_func_ret_TSmallRect11(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallRect11;
begin
  a.x := 11;
  a.y := 12;
  a.z := 13;
  r.x := 1;
  Args.InitWithResult<TSmallRect11>(r);
  Args.Add<TSmallRect11>(a);
  NCall(@cbReg_func_ret_TSmallRect11, Args);
  Assert(a.x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect11: done');
end;


procedure test_cbReg_func_ret_TSArray(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSArray;
begin
  r[0] := 11111;
  Args.InitWithResult<TSArray>(a);
  Args.Add<TSArray>(r);
  NCall(@cbReg_func_ret_TSArray, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSArray: done');
end;

procedure test_cbReg_func_sum_TSArrays(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, b: TSArray;
  i: Integer;
  r: TDynArrayInt;
begin
  for i := 0 to Length(a) - 1 do
  begin
    a[i] := i;
    b[i] := i;
  end;
  Args.InitWithResult<TDynArrayInt>(r);
  Args.Add<TSArray>(a);
  Args.Add<TSArray>(b);
  NCall(@cbReg_func_sum_TSArrays, Args);
  Assert(r[19] = 9);
  WriteLn('test_cbReg_func_sum_TSArrays: done');
end;

procedure test_cbReg_func_ret_TSmallSArray1(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  r, a: TSmallSArray1;
begin
  r[0] := 11;
  a[0] := 10;
  Args.InitWithResult<TSmallSArray1>(r);
  Args.Add<TSmallSArray1>(a);
  NCall(@cbReg_func_ret_TSmallSArray1, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray1: done');
end;

procedure test_cbReg_func_ret_TSmallSArray2(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray2;
begin
  r[0] := 11;
  Args.InitWithResult<TSmallSArray2>(a);
  Args.Add<TSmallSArray2>(r);
  NCall(@cbReg_func_ret_TSmallSArray2, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray2: done');
end;

procedure test_cbReg_func_ret_TSmallSArray3(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray3;
begin
  r[0] := 12;
  Args.InitWithResult<TSmallSArray3>(a);
  Args.Add<TSmallSArray3>(r);
  NCall(@cbReg_func_ret_TSmallSArray3, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray3: done');
end;

procedure test_cbReg_func_ret_TSmallSArray4(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray4;
begin
  r[0] := 13;
  Args.InitWithResult<TSmallSArray4>(a);
  Args.Add<TSmallSArray4>(r);
  NCall(@cbReg_func_ret_TSmallSArray4, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray4: done');
end;

procedure test_cbReg_func_ret_TSmallSArray5(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray5;
begin
  r[0] := 14;
  Args.InitWithResult<TSmallSArray5>(a);
  Args.Add<TSmallSArray5>(r);
  NCall(@cbReg_func_ret_TSmallSArray5, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray5: done');
end;

procedure test_cbReg_func_ret_TSmallSArray6(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray6;
begin
  r[0] := 15;
  Args.InitWithResult<TSmallSArray6>(a);
  Args.Add<TSmallSArray6>(r);
  NCall(@cbReg_func_ret_TSmallSArray6, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray6: done');
end;

procedure test_cbReg_func_ret_TSmallSArray7(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray7;
begin
  r[0] := 16;
  Args.InitWithResult<TSmallSArray7>(a);
  Args.Add<TSmallSArray7>(r);
  NCall(@cbReg_func_ret_TSmallSArray7, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray7: done');
end;

procedure test_cbReg_func_ret_TSmallSArray8(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TSmallSArray8;
begin
  r[0] := 17;
  Args.InitWithResult<TSmallSArray8>(a);
  Args.Add<TSmallSArray8>(r);
  NCall(@cbReg_func_ret_TSmallSArray8, Args);
  Assert(a[0] = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray8: done');
end;

procedure test_cbReg_func_DynArrayOfInt_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TDynArrayInt;
  i: integer;
begin
  SetLength(a, 10);
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Args.InitWithResult<int32>(i);
  Args.Add<TDynArrayInt>(a);
  NCall(@cbReg_func_DynArrayOfInt_sum, Args);
  Assert(i = 100);
  WriteLn('test_cbReg_func_DynArrayOfInt_sum: done');
end;

procedure test_cbReg_func_USTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: string;
begin
  a := 'abcdef_unicode';
  r := '';
  Args.InitWithResult<string>(r);
  Args.Add<string>(a);
  NCall(@cbReg_func_USTR, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_USTR: done');
end;

procedure test_cbReg_func_ASTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: AnsiString;
begin
  a := 'abcdef_ansi';
  r := '';
  Args.InitWithResult<AnsiString>(r);
  Args.Add<AnsiString>(a);
  NCall(@cbReg_func_ASTR, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_ASTR: done');
end;

procedure test_cbReg_func_INTF(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: IInterface;
begin
  r := nil;
  a := TInterfacedObject.Create;
  Args.InitWithResult<IInterface>(r);
  Args.Add<IInterface>(a);
  NCall(@cbReg_func_INTF, Args);
  Assert(r = a);
  WriteLn('test_cbReg_func_INTF: done');
end;

procedure test_cbReg_func_DynArray(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a, r: TDynArrayInt;
begin
  r := nil;
  SetLength(a,  1);
  Args.InitWithResult<TDynArrayInt>(r);
  Args.Add<TDynArrayInt>(a);
  NCall(@cbReg_func_DynArray, Args);
  Assert(Length(r) = 2);
  WriteLn('test_cbReg_func_DynArray: done');
end;

procedure test_cbReg_speed_test1(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  i, a, b, c, r: Int32;
  dt: TDateTime;
begin
  dt := Now;
  for i := 0 to 1000000 do begin
    r := cbReg_speed_test1(a, b, c);
    Assert(r = a + b + c);
  end;
  WriteLn('test_cbReg_speed_test1: (native) done at ' + FormatDateTime('HH:SS.ZZZ', Now - dt));


  Args.InitWithResult<Int32>(r);
  Args.Add<Int32>(a);
  Args.Add<Int32>(b);
  Args.Add<Int32>(c);

  dt := Now;
  for i := 0 to 1000000 do
  begin
    NCall(@cbReg_speed_test1, Args);
    Assert(r = a + b + c);
  end;
  WriteLn('test_cbReg_speed_test1: (adapter) done at ' + FormatDateTime('HH:SS.ZZZ', Now - dt));
end;

procedure test_cbReg_func_mix_0(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a: TSmallRect1;
  b: TSmallSArray1;
  r: TRect;
begin
  Args.InitWithResult<TRect>(r);
  Args.Add<TSmallRect1>(a);
  Args.Add<TSmallSArray1>(b);
  NCall(@cbReg_func_mix_0, Args);
  Assert(r.Left = SizeOF(a) + SizeOF(b));
  WriteLn('test_cbReg_func_mix_0: done');
end;

procedure test_cbReg_func_mix_1(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a1: TSmallRect1;
  a2: TSmallSArray1;
  a3: TSet8;
  a4, r: TRect;
begin
  a1.x := 1;
  a2[0] := 2;
  a3 := [TEnum8._A02];
  Args.InitWithResult<TRect>(r);
  Args.Add<TSmallRect1>(a1);
  Args.Add<TSmallSArray1>(a2);
  Args.Add<TSet8>(a3);
  Args.Add<TRect>(a4);
  NCall(@cbReg_func_mix_1, Args);
  Assert(r.Left = SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + SizeOF(a4));
  WriteLn('test_cbReg_func_mix_1: done');
end;

procedure test_cbReg_func_mix_2(cc: TNCCallingConvention = cdRegister);
var
  Args: TArgs;
  a1: TSmallRect1;
  a2: TSmallSArray1;
  a3: TSet8;
  a4: TDynArrayInt;
  a5: TSArray;
  a6, r: TRect;
  a7: string;
  a8: AnsiString;
  a9: IInterface;
begin
  a1.x := 1;
  a2[0] := 2;
  a3 := [TEnum8._a02];
  SetLength(a4, 2);
  a5[0] := 12;
  a7 := 'unicode';
  a8 := 'ansi';
  a9 := nil;
  Args.InitWithResult<TRect>(r);
  Args.Add<TSmallRect1>(a1);
  Args.Add<TSmallSArray1>(a2);
  Args.Add<TSet8>(a3);
  Args.Add<TDynArrayInt>(a4);
  Args.Add<TSArray>(a5);
  Args.Add<TRect>(a6);
  Args.Add<string>(a7);
  Args.Add<AnsiString>(a8);
  Args.Add<IInterface>(a9);
  NCall(@cbReg_func_mix_2, Args);
  Assert(r.Left = SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + Length(a4) + SizeOF(a5) + SizeOF(a6) + SizeOF(a7) + Length(a7) + Length(a8) + SizeOf(A9));
  WriteLn('test_cbReg_func_mix_2: done');
end;

procedure RunALlTests;
const
  cc: TNCCallingConvention = cdRegister;
begin
  test_cbReg_proc_i8(cc);
  test_cbReg_proc_i8_5(cc);
  test_cbReg_proc_ref_i8(cc);

  test_cbReg_proc_i16(cc);
  test_cbReg_proc_i16_5(cc);

  test_cbReg_proc_i32(cc);
  test_cbReg_proc_i32_5(cc);
  test_cbReg_proc_i32_5s(cc);

  test_cbReg_proc_i64(cc);
  test_cbReg_proc_i64_5(cc);

  test_cbReg_proc_int_all(cc);

  test_cbReg_proc_USTR(cc);
  test_cbReg_proc_ASTR(cc);

  test_cbReg_proc_F32(cc);
  test_cbReg_proc_F64(cc);
  test_cbReg_proc_variant(cc);

  test_cbReg_proc_set8(cc);
  test_cbReg_proc_set16(cc);
  test_cbReg_proc_set32(cc);
  test_cbReg_proc_set64(cc);
  test_cbReg_proc_set80(cc);

  test_cbReg_func_i32(cc);

  test_cbReg_func_i32_5(cc);
  test_cbReg_func_i32_5s(cc);

  test_cbReg_func_i64(cc);
  test_cbReg_func_int_i32(cc);

  test_cbReg_func_f32(cc);
  test_cbReg_func_f64(cc);

  test_cbReg_func_F32Sum(cc);
  test_cbReg_func_F64Sum(cc);
  test_cbReg_func_USTRSum(cc);
  test_cbReg_func_variant(cc);
  test_cbReg_func_variant_sum(cc);

  test_cbReg_func_set8(cc);
  test_cbReg_func_set16(cc);
  test_cbReg_func_set24(cc);
  test_cbReg_func_set32(cc);
  test_cbReg_func_set48(cc);
  test_cbReg_func_set56(cc);
  test_cbReg_func_set64(cc);
  test_cbReg_func_set80(cc);

  test_cbReg_obj_create_0(cc);
  test_cbReg_obj_create_1(cc);
  test_cbReg_method(cc);
  test_cbReg_method_i32(cc);

  test_cbReg_func_TRect_width(cc);

  test_cbReg_func_TSmallRect1(cc);
  test_cbReg_func_TSmallRect2(cc);
  test_cbReg_func_TSmallRect3(cc);
  test_cbReg_func_TSmallRect4(cc);
  test_cbReg_func_TSmallRect5(cc);

  test_cbReg_func_ret_TSmallRect1(cc);
  test_cbReg_func_ret_TSmallRect2(cc);
  test_cbReg_func_ret_TSmallRect3(cc);
  test_cbReg_func_ret_TSmallRect4(cc);
  test_cbReg_func_ret_TSmallRect5(cc);
  test_cbReg_func_ret_TSmallRect6(cc);
  test_cbReg_func_ret_TSmallRect7(cc);
  test_cbReg_func_ret_TSmallRect8(cc);
  test_cbReg_func_ret_TSmallRect9(cc);
  test_cbReg_func_ret_TSmallRect10(cc);
  test_cbReg_func_ret_TSmallRect11(cc);

  test_cbReg_func_TStaticArray_sum(cc);
  test_cbReg_func_TSmallStaticArray1_sum(cc);
  test_cbReg_func_TSmallStaticArray2_sum(cc);
  test_cbReg_func_TSmallStaticArray3_sum(cc);
  test_cbReg_func_TSmallStaticArray4_sum(cc);
  test_cbReg_func_TSmallStaticArray5_sum(cc);

  test_cbReg_func_ret_TSmallSArray1(cc);
  test_cbReg_func_ret_TSmallSArray2(cc);
  test_cbReg_func_ret_TSmallSArray3(cc);
  test_cbReg_func_ret_TSmallSArray4(cc);
  test_cbReg_func_ret_TSmallSArray5(cc);
  test_cbReg_func_ret_TSmallSArray6(cc);
  test_cbReg_func_ret_TSmallSArray7(cc);
  test_cbReg_func_ret_TSmallSArray8(cc);

  test_cbReg_func_DynArrayOfInt_sum(cc);

  test_cbReg_func_USTR(cc);
  test_cbReg_func_ASTR(cc);
  test_cbReg_func_INTF(cc);
  test_cbReg_func_DynArray(cc);

  test_cbReg_func_ret_TSArray(cc);   {}
  test_cbReg_func_sum_TSArrays(cc);

  test_cbReg_func_mix_0(cc);
  test_cbReg_func_mix_1(cc);
  test_cbReg_func_mix_2(cc);
  test_cbReg_speed_test1(cc);

  {}
end;
{ TMyClass }

constructor TMyClass.Create2(a, b, c: Integer);
begin
  FData := a + b + c;
end;

constructor TMyClass.Create;
begin
  FData := 1;
end;

procedure TMyClass.Method1;
begin
  I32 := 111;
end;

function TMyClass.Method_i32(a: Int32): Int32;
begin
  Result := a;
end;

var
  s1, s2: TSet8;
  r1, r2: TSmallRect8;
  i: int32;
initialization
  r1.X := 1;
  r2 := cbReg_func_ret_TSmallRect8(r1);
  r2.X := 2;
end.
