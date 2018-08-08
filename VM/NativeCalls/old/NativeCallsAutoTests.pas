unit NativeCallsAutoTests;

interface

{$ifdef FPC}
  {$MODE DELPHI}
{$endif}

{$DEFINE CC_REGISTER}
//{$DEFINE CC_STDCALL}

{$SCOPEDENUMS ON}

procedure RunTests;

implementation

uses SysUtils, NativeCalls, NativeCallsWithOldIntf;

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

  TRect = record
    Left, Top, Right, Bottom: Int32;
  end;

  TSmallRect1 = packed record x: byte; end;
  TSmallRect2 = packed record x, y: byte; end;
  TSmallRect3 = packed record x, y, z: byte; end;
  TSmallRect4 = packed record x, y, z, w: byte; end;
  TSmallRect5 = packed record x, y, z, w, q: byte; end;
  TSmallRect6 = packed record x, y, z, w, q, a: byte; end;
  TSmallRect7 = packed record x, y, z, w, q, a, b: byte; end;
  TSmallRect8 = packed record x, y, z, w, q, a, b, c: byte; end;

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

type
  TMyClass = class
  private
    FData: Integer;
  public
    procedure Method1;
    function Method_i32(a: Int32): Int32;
    constructor Create; overload;
    constructor Create2(a, b, c: Integer); overload;
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

function cbReg_func_int: Int32; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
begin
  Result := 5;
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
  Result := a;
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
  Args: TNCArgs;
  a: Int8;
begin
  a := 55;
  Args := CreateNativeCallArgs([btS8], [@a]);
  NativeCall(nil, @cbReg_proc_i8, cc, Args, nil);
  Assert(i32 = a);
  WriteLn('test_cbReg_proc_i8: done');
end;

procedure test_cbReg_proc_ref_i8(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Int8;
begin
  a := 55;
  Args := CreateNativeCallArgs([btVarParam], [@a]);
  NativeCall(nil, @cbReg_proc_ref_i8, cc, Args, nil);
  Assert(a = 56);
  WriteLn('test_cbReg_proc_ref_i8: done');
end;

procedure test_cbReg_proc_USTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  s: string;
begin
  s := 'abcd';
  Args := CreateNativeCallArgs([btUnicodeString], [@s]);
  NativeCall(nil, @cbReg_proc_USTR, cc, Args, nil);
  Assert(USTR = s);
  WriteLn('test_cbReg_proc_USTR: done');
end;

procedure test_cbReg_proc_ASTR(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  s: AnsiString;
begin
  s := 'abcd';
  Args := CreateNativeCallArgs([btAnsiString], [@s]);
  NativeCall(nil, @cbReg_proc_ASTR, cc, Args, nil);
  Assert(ASTR = s);
  WriteLn('test_cbReg_proc_ASTR: done');
end;

procedure test_cbReg_proc_F32(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Single;
begin
  a := 5.12;
  Args := CreateNativeCallArgs([btSingle], [@a]);
  NativeCall(nil, @cbReg_proc_F32, cc, Args, nil);
  Assert(F32 = a);
  WriteLn('test_cbReg_proc_F32: done');
end;

procedure test_cbReg_proc_F64(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Double;
begin
  a := 5.12345678;
  Args := CreateNativeCallArgs([btDouble], [@a]);
  NativeCall(nil, @cbReg_proc_F64, cc, Args, nil);
  Assert(F64 = a);
  WriteLn('test_cbReg_proc_F64: done');
end;

procedure test_cbReg_proc_variant(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Variant;
begin
  a := 1234;
  Args := CreateNativeCallArgs([btVariant], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_proc_variant, cc, Args, nil);
  Assert(VVAR = a);
  WriteLn('test_cbReg_proc_variant: done');
end;

procedure test_cbReg_proc_set8(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: TSet8;
begin
  a := [TEnum8._a01];
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_proc_set8, cc, Args, nil);
  Assert(Set8 = a);
  WriteLn('test_cbReg_proc_set8: done');
end;

procedure test_cbReg_proc_set16(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: TSet16;
begin
  a := [TEnum16._a02];
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_proc_set16, cc, Args, nil);
  Assert(Set16 = a);
  WriteLn('test_cbReg_proc_set16: done');
end;

procedure test_cbReg_proc_set32(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: TSet32;
begin
  a := [TEnum32._a03];
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_proc_set32, cc, Args, nil);
  Assert(Set32 = a);
  WriteLn('test_cbReg_proc_set16: done');
end;

procedure test_cbReg_proc_set64(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: TSet64;
begin
  a := [TEnum64._a04];
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_proc_set64, cc, Args, nil);
  Assert(Set64 = a);
  WriteLn('test_cbReg_proc_set64: done');
end;

procedure test_cbReg_func_variant(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Variant;
  r: PNCArg;
begin
  a := 3333;
  r := CreateNativeCallArg(btVariant);
  r.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btVariant], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_variant, cc, Args, r);
  Assert(PVariant(r.Dta)^ = a);
  WriteLn('test_cbReg_func_variant: done');
end;

procedure test_cbReg_func_variant_sum(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a, b: Variant;
  r: PNCArg;
begin
  a := 3333;
  b := 3333;
  r := CreateNativeCallArg(btVariant);
  r.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btVariant, btVariant], [@a, @b]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_variant_sum, cc, Args, r);
  Assert(PVariant(r.Dta)^ = a + b);
  WriteLn('test_cbReg_func_variant_sum: done');
end;

procedure test_cbReg_proc_i8_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a, b, c, d, e: Int8;
begin
  a := 1;
  b := 2;
  c := 3;
  d := 4;
  e := 5;
  Args := CreateNativeCallArgs([btS8, btS8, btS8, btS8, btS8], [@a, @b, @c, @d, @e]);
  NativeCall(nil, @cbReg_proc_i8_5, cc, Args, nil);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i8_5: done');
end;

procedure test_cbReg_proc_i16(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Int16;
begin
  a := 55;
  Args := CreateNativeCallArgs([btS16], [@a]);
  NativeCall(nil, @cbReg_proc_i16, cc, Args, nil);
  Assert(i32 = a);
  WriteLn('test_cbReg_proc_i16: done');
end;

procedure test_cbReg_proc_i16_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a, b, c, d, e: Int16;
begin
  a := 100;
  b := 200;
  c := 300;
  d := 400;
  e := 500;
  Args := CreateNativeCallArgs([btS16, btS16, btS16, btS16, btS16], [@a, @b, @c, @d, @e]);
  NativeCall(nil, @cbReg_proc_i16_5, cc, Args, nil);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i16_5: done');
end;

procedure test_cbReg_proc_i32(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Int32;
begin
  a := 55;
  Args := CreateNativeCallArgs([btS32], [@a]);
  NativeCall(nil, @cbReg_proc_i32, cc, Args, nil);
  Assert(i32 = a);
  WriteLn('test_cbReg_proc_i32: done');
end;

procedure test_cbReg_proc_i32_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a, b, c, d, e: Int32;
begin
  a := 100000;
  b := 200000;
  c := 300000;
  d := 400000;
  e := 500000;
  Args := CreateNativeCallArgs([btS32, btS32, btS32, btS32, btS32], [@a, @b, @c, @d, @e]);
  NativeCall(nil, @cbReg_proc_i32_5, cc, Args, nil);
  Assert(i32 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i32_5: done');
end;

procedure test_cbReg_proc_i64(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a: Int64;
begin
  a := 55;
  Args := CreateNativeCallArgs([btS64], [@a]);
  NativeCall(nil, @cbReg_proc_i64, cc, Args, nil);
  Assert(i64 = a);
  WriteLn('test_cbReg_proc_i64: done');
end;

procedure test_cbReg_proc_i64_5(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
  a, b, c, d, e: Int64;
begin
  a := 100000000;
  b := 200000000;
  c := 300000000;
  d := 400000000;
  e := 500000000;
  Args := CreateNativeCallArgs([btS64, btS64, btS64, btS64, btS64], [@a, @b, @c, @d, @e]);
  NativeCall(nil, @cbReg_proc_i64_5, cc, Args, nil);
  Assert(i64 = (a + b + c + d + e));
  WriteLn('test_cbReg_proc_i64_5: done');
end;

procedure test_cbReg_proc_int_all(cc: TNCCallingConvention = cdRegister);
var
  Args: TNCArgs;
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
  Args := CreateNativeCallArgs([btu8, bts8, btu16, bts16, btu32, bts32, bts64, bts64, btAnsiChar, btWideChar],
                               [@a1, @a2, @a3, @a4, @a5, @a6, @a7, @a8, @a9, @a10]);
  NativeCall(nil, @cbReg_proc_int_all, cc, Args, nil);
  Assert(I64 = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + ord(a9) + ord(a10));
  WriteLn('test_cbReg_proc_int_all: done');
end;

procedure test_cbReg_func_int(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
begin
  Res := CreateNativeCallArg(btS32);
  NativeCall(nil, @cbReg_func_int, cc, nil, Res);
  Assert(PInteger(Res.Dta)^ = 5);
  WriteLn('test_cbReg_func_int: done');
end;

procedure test_cbReg_func_f32(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  a: Single;
begin
  a := 5.111;
  Res := CreateNativeCallArg(btSingle);
  NativeCall(nil, @cbReg_func_f32, cc, nil, Res);
  Assert(PSingle(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_f32: done');
end;

procedure test_cbReg_func_f64(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  a: Double;
begin
  a := 5.11111111;
  Res := CreateNativeCallArg(btDouble);
  NativeCall(nil, @cbReg_func_f64, cc, nil, Res);
  Assert(PDouble(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_f64: done');
end;

procedure test_cbReg_func_int_i32(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: int32;
begin
  a := 12;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btS32], [@a]);
  NativeCall(nil, @cbReg_func_int_i32, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_int_i32: done');
end;

procedure test_cbReg_func_F64Sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a, b, r: Double;
begin
  a := 1.11111111;
  b := 2.22222222;
  r := a + b;
  Res := CreateNativeCallArg(btDouble);
  Args := CreateNativeCallArgs([btDouble, btDouble], [@a, @b]);
  NativeCall(nil, @cbReg_func_F64Sum, cc, Args, Res);
  Assert(PDouble(Res.Dta)^ = r);
  WriteLn('test_cbReg_func_F64Sum: done');
end;

procedure test_cbReg_func_F32Sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a, b, r: Single;
begin
  a := 1.222;
  b := 1.333;
  r := a + b;
  Res := CreateNativeCallArg(btSingle);
  Args := CreateNativeCallArgs([btSingle, btSingle], [@a, @b]);
  NativeCall(nil, @cbReg_func_F32Sum, cc, Args, Res);
  Assert(PSingle(Res.Dta)^ = r);
  WriteLn('test_cbReg_func_F32Sum: done');
end;

procedure test_cbReg_func_USTRSum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a, b, r: string;
begin
  a := 'aaa';
  b := 'bbb';
  r := a + b;
  Res := CreateNativeCallArg(btUnicodeString);
  Args := CreateNativeCallArgs([btUnicodeString, btUnicodeString], [@a, @b]);
  NativeCall(nil, @cbReg_func_USTRSum, cc, Args, Res);
  Assert(PString(Res.Dta)^ = r);
  WriteLn('test_cbReg_func_USTRSum: done');
end;

procedure test_cbReg_func_set8(cc: TNCCallingConvention = cdRegister);
type
  PSet8 = ^TSet8;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet8;
begin
  a := [TEnum8._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set8, cc, Args, Res);
  Assert(PSet8(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set8: done');
end;

procedure test_cbReg_func_set16(cc: TNCCallingConvention = cdRegister);
type
  PSet16 = ^TSet16;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet16;
begin
  a := [TEnum16._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set16, cc, Args, Res);
  Assert(PSet16(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set16: done');
end;

procedure test_cbReg_func_set24(cc: TNCCallingConvention = cdRegister);
type
  PSet24 = ^TSet24;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet24;
begin
  a := [TEnum24._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set24, cc, Args, Res);
  Assert(PSet24(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set24: done');
end;

procedure test_cbReg_func_set32(cc: TNCCallingConvention = cdRegister);
type
  PSet32 = ^TSet32;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet32;
begin
  a := [TEnum32._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set32, cc, Args, Res);
  Assert(PSet32(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set32: done');
end;

procedure test_cbReg_func_set48(cc: TNCCallingConvention = cdRegister);
type
  PSet48 = ^TSet48;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet48;
begin
  a := [TEnum48._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set48, cc, Args, Res);
  Assert(PSet48(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set48: done');
end;

procedure test_cbReg_func_set56(cc: TNCCallingConvention = cdRegister);
type
  PSet56 = ^TSet56;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet56;
begin
  a := [TEnum56._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set56, cc, Args, Res);
  Assert(PSet56(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set56: done');
end;

procedure test_cbReg_func_set64(cc: TNCCallingConvention = cdRegister);
type
  PSet64 = ^TSet64;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSet64;
begin
  a := [TEnum64._a00];
  Res := CreateNativeCallArg(btSet);
  Res.DataSize := SizeOf(a);
  Args := CreateNativeCallArgs([btSet], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_set64, cc, Args, Res);
  Assert(PSet64(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_set64: done');
end;

procedure test_cbReg_method(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  try
    NativeCall(Obj, @TMyClass.Method1, cc, nil, nil);
    Assert(I32 = 111);
  finally
    Obj.Free;
  end;
  WriteLn('test_cbReg_method: done');
end;

procedure test_cbReg_method_i32(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TNCArgs;
  Res: PNCArg;
  a: Int32;
begin
  Obj := TMyClass.Create;
  try
    a := 12;
    Res := CreateNativeCallArg(btS32);
    Args := CreateNativeCallArgs([btS32], [@a]);
    NativeCall(Obj, @TMyClass.Method_i32, cc, Args, Res);
    Assert(PInteger(Res.Dta)^ = a);
  finally
    Obj.Free;
  end;
  WriteLn('test_cbReg_method_i32: done');
end;

procedure test_cbReg_obj_create_0(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TNCArgs;
  Res: PNCArg;
  cl: TClass;
  dl: Integer;
begin
  cl := TMyClass;
  dl := 1;
  Res := CreateNativeCallArg(btClass);
  Args := CreateNativeCallArgs([btS8], [@dl]);
  NativeCall(CL, @TMyClass.Create, cc, Args, Res);
  Obj := TMyClass(Res.Dta^);
  Assert(Obj.FData = 1);
  Obj.Free;
  WriteLn('test_cbReg_obj_create_0: done');
end;

procedure test_cbReg_obj_create_1(cc: TNCCallingConvention = cdRegister);
var
  Obj: TMyClass;
  Args: TNCArgs;
  Res: PNCArg;
  cl: TClass;
  dl, a, b, c: Integer;
begin
  cl := TMyClass;
  dl := 1;
  a := 1;
  b := 2;
  c := 3;
  Res := CreateNativeCallArg(btClass);
  Args := CreateNativeCallArgs([btS8, btS32, btS32, btS32], [@dl, @a, @b, @c]);
  NativeCall(CL, @TMyClass.Create2, cc, Args, Res);
  Obj := TMyClass(Res.Dta^);
  Assert(Obj.FData = 6);
  Obj.Free;
  WriteLn('test_cbReg_obj_create_1: done');
end;

procedure test_cbReg_func_TRect_width(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TRect;
begin
  r.Left := 10;
  r.Right := 30;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(TRect);
  NativeCall(nil, @cbReg_func_TRect_width, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 20);
  WriteLn('test_cbReg_func_TRect_width: done');
end;

procedure test_cbReg_func_TSmallRect1(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect1;
begin
  r.x := 11;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_TSmallRect1, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 11);
  WriteLn('test_cbReg_func_TSmallRect1: done');
end;

procedure test_cbReg_func_TSmallRect2(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect2;
begin
  r.x := 10;
  r.y := 20;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_TSmallRect2, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 30);
  WriteLn('test_cbReg_func_TSmallRect2: done');
end;

procedure test_cbReg_func_TSmallRect3(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect3;
begin
  r.x := 10;
  r.y := 20;
  r.z := 30;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_TSmallRect3, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 60);
  WriteLn('test_cbReg_func_TSmallRect3: done');
end;

procedure test_cbReg_func_TSmallRect4(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect4;
begin
  r.x := 10;
  r.y := 20;
  r.z := 30;
  r.w := 40;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_TSmallRect4, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 100);
  WriteLn('test_cbReg_func_TSmallRect4: done');
end;

procedure test_cbReg_func_TSmallRect5(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect5;
begin
  r.x := 10;
  r.y := 20;
  r.z := 30;
  r.w := 40;
  r.q := 50;
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_TSmallRect5, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 150);
  WriteLn('test_cbReg_func_TSmallRect5: done');
end;

procedure test_cbReg_func_TStaticArray_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSArray;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TStaticArray_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 100);
  WriteLn('test_cbReg_func_TStaticArray_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray1_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSmallSArray1;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TSmallStaticArray1_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 10);
  WriteLn('test_cbReg_func_TSmallStaticArray1_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray2_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSmallSArray2;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TSmallStaticArray2_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 20);
  WriteLn('test_cbReg_func_TSmallStaticArray2_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray3_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSmallSArray3;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TSmallStaticArray3_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 30);
  WriteLn('test_cbReg_func_TSmallStaticArray3_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray4_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSmallSArray4;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TSmallStaticArray4_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 40);
  WriteLn('test_cbReg_func_TSmallStaticArray4_sum: done');
end;

procedure test_cbReg_func_TSmallStaticArray5_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TSmallSArray5;
  i: integer;
begin
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btStaticArray], [@a]);
  Args[0].DataSize := SizeOf(a);
  NativeCall(nil, @cbReg_func_TSmallStaticArray5_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 50);
  WriteLn('test_cbReg_func_TSmallStaticArray5_sum: done');
end;

procedure test_cbReg_func_ret_TRect(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TRect;
begin
  r.Left := 11;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TRect, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = r.Left);
  WriteLn('test_cbReg_func_ret_TRect: done');
end;

procedure test_cbReg_func_ret_TSmallRect1(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect1;
begin
  r.x := 12;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect1, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect1: done');
end;

procedure test_cbReg_func_ret_TSmallRect2(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect2;
begin
  r.x := 13;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect2, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect2: done');
end;

procedure test_cbReg_func_ret_TSmallRect3(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect3;
begin
  r.x := 14;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect3, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect3: done');
end;

procedure test_cbReg_func_ret_TSmallRect4(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect4;
begin
  r.x := 15;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect4, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect4: done');
end;

procedure test_cbReg_func_ret_TSmallRect5(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect5;
begin
  r.x := 16;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect5, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect5: done');
end;

procedure test_cbReg_func_ret_TSmallRect6(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect6;
begin
  r.x := 17;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect6, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect6: done');
end;

procedure test_cbReg_func_ret_TSmallRect7(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect7;
begin
  r.x := 18;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect7, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect7: done');
end;

procedure test_cbReg_func_ret_TSmallRect8(cc: TNCCallingConvention = cdRegister);
type
   PSmallRect8 = ^TSmallRect8;
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallRect8;
begin
  r.x := 18;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btRecord], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallRect8, cc, Args, Res);
  Assert(PSmallRect8(Res.Dta).x = r.x);
  WriteLn('test_cbReg_func_ret_TSmallRect8: done');
end;

procedure test_cbReg_func_ret_TSArray(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSArray;
begin
  r[0] := 11111;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSArray, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSArray: done');
end;

procedure test_cbReg_func_ret_TSmallSArray1(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray1;
begin
  r[0] := 10;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray1, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray1: done');
end;

procedure test_cbReg_func_ret_TSmallSArray2(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray2;
begin
  r[0] := 11;
  r[1] := 0;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray2, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray2: done');
end;

procedure test_cbReg_func_ret_TSmallSArray3(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray3;
begin
  r[0] := 12;
  r[1] := 0;
  r[2] := 0;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray3, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray3: done');
end;

procedure test_cbReg_func_ret_TSmallSArray4(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray4;
begin
  r[0] := 13;
  r[1] := 0;
  r[2] := 0;
  r[3] := 0;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray4, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray4: done');
end;

procedure test_cbReg_func_ret_TSmallSArray5(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray5;
begin
  r[0] := 14;
  r[1] := 0;
  r[2] := 0;
  r[3] := 0;
  r[4] := 0;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray5, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray5: done');
end;

procedure test_cbReg_func_ret_TSmallSArray6(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray6;
begin
  r[0] := 15;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray6, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray6: done');
end;

procedure test_cbReg_func_ret_TSmallSArray7(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray7;
begin
  r[0] := 16;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray7, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray7: done');
end;

procedure test_cbReg_func_ret_TSmallSArray8(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  r: TSmallSArray8;
begin
  r[0] := 17;
  Res := CreateNativeCallArg(btStaticArray);
  Res.DataSize := SizeOf(r);
  Args := CreateNativeCallArgs([btStaticArray], [@r]);
  Args[0].DataSize := SizeOf(r);
  NativeCall(nil, @cbReg_func_ret_TSmallSArray8, cc, Args, Res);
  Assert(PByte(Res.Dta)^ = r[0]);
  WriteLn('test_cbReg_func_ret_TSmallSArray8: done');
end;

procedure test_cbReg_func_DynArrayOfInt_sum(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: array of integer;
  i: integer;
begin
  SetLength(a, 10);
  for i := 0 to Length(a) - 1 do
    a[i] := 10;

  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btDynArray], [@a]);
  NativeCall(nil, @cbReg_func_DynArrayOfInt_sum, cc, Args, Res);
  Assert(PInteger(Res.Dta)^ = 100);
  WriteLn('test_cbReg_func_DynArrayOfInt_sum: done');
end;

procedure test_cbReg_func_USTR(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: string;
begin
  a := 'abcdef';
  Res := CreateNativeCallArg(btUnicodeString);
  Args := CreateNativeCallArgs([btUnicodeString], [@a]);
  NativeCall(nil, @cbReg_func_USTR, cc, Args, Res);
  Assert(PString(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_USTR: done');
end;

procedure test_cbReg_func_ASTR(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  a: AnsiString;
begin
  a := 'abcdef';
  Res := CreateNativeCallArg(btAnsiString);
  Args := CreateNativeCallArgs([btAnsiString], [@a]);
  NativeCall(nil, @cbReg_func_ASTR, cc, Args, Res);
  Assert(PAnsiString(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_ASTR: done');
end;

procedure test_cbReg_func_INTF(cc: TNCCallingConvention = cdRegister);
type
  PInterface = ^IInterface;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: IInterface;
begin
  a := TInterfacedObject.Create;
  Res := CreateNativeCallArg(btInterface);
  Args := CreateNativeCallArgs([btInterface], [@a]);
  NativeCall(nil, @cbReg_func_INTF, cc, Args, Res);
  Assert(PInterface(Res.Dta)^ = a);
  WriteLn('test_cbReg_func_INTF: done');
end;

procedure test_cbReg_func_DynArray(cc: TNCCallingConvention = cdRegister);
type
  PDynArrayInt = ^TDynArrayInt;
var
  Res: PNCArg;
  Args: TNCArgs;
  a: TDynArrayInt;
begin
  SetLength(a, 3);
  Res := CreateNativeCallArg(btDynArray);
  Args := CreateNativeCallArgs([btDynArray], [@a]);
  NativeCall(nil, @cbReg_func_DynArray, cc, Args, Res);
  Assert(Length(TDynArrayInt(PDynArrayInt(Res.Dta)^)) = 3);
  WriteLn('test_cbReg_func_DynArray: done');
end;

procedure test_cbReg_speed_test1(cc: TNCCallingConvention = cdRegister);
var
  Res: PNCArg;
  Args: TNCArgs;
  i, a, b, c, r: Int32;
  dt: TDateTime;
begin
  Res := CreateNativeCallArg(btS32);
  Args := CreateNativeCallArgs([btS32, btS32, btS32], [@a, @b, @c]);

  dt := Now;
  for i := 0 to 1000000 do begin
    r := cbReg_speed_test1(a, b, c);
    Assert(r = a + b + c);
  end;
  WriteLn('test_cbReg_speed_test1: (native) done at ' + FormatDateTime('HH:SS.ZZZ', Now - dt));


  dt := Now;
  for i := 0 to 1000000 do
  begin
    NativeCall(nil, @cbReg_speed_test1, cc, Args, Res);
    Assert(PInteger(Res.Dta)^ = a + b + c);
  end;
  WriteLn('test_cbReg_speed_test1: (adapter) done at ' + FormatDateTime('HH:SS.ZZZ', Now - dt));
end;

procedure test_cbReg_func_mix_0(cc: TNCCallingConvention = cdRegister);
type
  PRect = ^TRect;
var
  Args: TNCArgs;
  Res: PNCArg;
  a: TSmallRect1;
  b: TSmallSArray1;
begin
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(TRect);
  Args := CreateNativeCallArgs([btRecord, btStaticArray], [@a, @b]);
  Args[0].DataSize := SizeOf(a);
  Args[1].DataSize := SizeOf(b);
  NativeCall(nil, @cbReg_func_mix_0, cc, Args, Res);
  Assert(PRect(Res.Dta).Left = SizeOF(a) + SizeOF(b));
  WriteLn('cbReg_func_mix_0: done');
end;

procedure test_cbReg_func_mix_1(cc: TNCCallingConvention = cdRegister);
type
  PRect = ^TRect;
var
  Args: TNCArgs;
  Res: PNCArg;
  a1: TSmallRect1;
  a2: TSmallSArray1;
  a3: TSet8;
  a4: TRect;
begin
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(TRect);
  Args := CreateNativeCallArgs([btRecord, btStaticArray, btSet, btRecord],
                               [@a1, @a2, @a3, @a4]);
  Args[0].DataSize := SizeOf(a1);
  Args[1].DataSize := SizeOf(a2);
  Args[2].DataSize := SizeOf(a3);
  Args[3].DataSize := SizeOf(a4);
  NativeCall(nil, @cbReg_func_mix_1, cc, Args, Res);
  Assert(PRect(Res.Dta).Left = SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + SizeOF(a4));
  WriteLn('test_cbReg_func_mix_1: done');
end;

procedure test_cbReg_func_mix_2(cc: TNCCallingConvention = cdRegister);
type
  PRect = ^TRect;
var
  Args: TNCArgs;
  Res: PNCArg;
  a1: TSmallRect1;
  a2: TSmallSArray1;
  a3: TSet8;
  a4: TDynArrayInt;
  a5: TSArray;
  a6: TRect;
  a7: string;
  a8: AnsiString;
  a9: IInterface;
begin
  SetLength(a4, 2);
  a7 := 'unicode';
  a8 := 'ansi';
  a9 := nil;
  Res := CreateNativeCallArg(btRecord);
  Res.DataSize := SizeOf(TRect);
  Args := CreateNativeCallArgs([btRecord, btStaticArray, btSet, btDynArray, btStaticArray, btRecord, btUnicodeString, btAnsiString, btInterface],
                               [@a1, @a2, @a3, @a4, @a5, @a6, @a7, @a8, @a9]);
  Args[0].DataSize := SizeOf(a1);
  Args[1].DataSize := SizeOf(a2);
  Args[2].DataSize := SizeOf(a3);
  Args[3].DataSize := SizeOf(a4);
  Args[4].DataSize := SizeOf(a5);
  Args[5].DataSize := SizeOf(a6);
  Args[6].DataSize := SizeOf(a7);
  Args[7].DataSize := SizeOf(a8);
  Args[8].DataSize := SizeOf(a9);
  NativeCall(nil, @cbReg_func_mix_2, cc, Args, Res);
  Assert(PRect(Res.Dta).Left = SizeOF(a1) + SizeOF(a2) + SizeOF(a3) + Length(a4) + SizeOF(a5) + SizeOF(a6) + SizeOF(a7) + Length(a7) + Length(a8) + SizeOf(A9));
  WriteLn('test_cbReg_func_mix_2: done');
end;

procedure RunTests;
  {$IFDEF CC_REGISTER} const cc: TNCCallingConvention = cdRegister; {$ENDIF}
  {$IFDEF CC_STDCALL} const cc: TNCCallingConvention = cdStdCall; {$ENDIF}
begin
  test_cbReg_proc_i8(cc);
  test_cbReg_proc_i8_5(cc);
  test_cbReg_proc_ref_i8(cc);

  test_cbReg_proc_i16(cc);
  test_cbReg_proc_i16_5(cc);

  test_cbReg_proc_i32(cc);
  test_cbReg_proc_i32_5(cc);

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

  test_cbReg_func_int(cc);
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

  test_cbReg_obj_create_0(cc);
  test_cbReg_obj_create_1(cc);
  test_cbReg_method(cc);
  test_cbReg_method_i32(cc);

  test_cbReg_func_TRect_width(cc);

  test_cbReg_func_TSmallRect1(cc);
  test_cbReg_func_TSmallRect2(cc);
  test_cbReg_func_TSmallRect3(cc);
  test_cbReg_func_TSmallRect4(cc);
  test_cbReg_func_TSmallRect5(cc);   {}

  test_cbReg_func_ret_TSmallRect1(cc);
  test_cbReg_func_ret_TSmallRect2(cc);
  test_cbReg_func_ret_TSmallRect3(cc);
  test_cbReg_func_ret_TSmallRect4(cc);
  test_cbReg_func_ret_TSmallRect5(cc);
  test_cbReg_func_ret_TSmallRect6(cc);
  test_cbReg_func_ret_TSmallRect7(cc);
  test_cbReg_func_ret_TSmallRect8(cc);

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

  test_cbReg_func_mix_0(cc);
  test_cbReg_func_mix_1(cc);
  test_cbReg_func_mix_2(cc);
  test_cbReg_speed_test1(cc);
  test_cbReg_func_ret_TSArray(cc);  {}

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

initialization

end.

