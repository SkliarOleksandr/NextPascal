unit CATNativeCallsTests;

interface

{$i compilers.inc}

implementation

uses VM.Invoke, Math; // system

type
  TRect16 = record
    x, y, z, w: Int32;
  end;

  {$SCOPEDENUMS ON}

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

  TIntArray = array of Integer;

  TSet8 = packed set of TEnum8;
  TSet16 = packed set of TEnum16;
  TSet24 = packed set of TEnum24;
  TSet32 = packed set of TEnum32;
  TSet48 = packed set of TEnum48;
  TSet56 = packed set of TEnum56;
  TSet64 = packed set of TEnum64;
  TSet80 = packed set of 0..79;

procedure proc_ccreg_int8(a: int8);
begin
  Assert(a = 8);
end;

procedure proc_ccreg_int8_2(a, b: int8);
begin
  Assert(a + b = 3);
end;

procedure proc_ccreg_int8_3(a, b, c: int8);
begin
  Assert(a + b + c = 6);
end;

procedure proc_ccreg_int8_4(a, b, c, d: int8);
begin
  Assert(a + b + c + d = 10);
end;

procedure proc_ccreg_int8_5(a, b, c, d, e: int8);
begin
  Assert(a + b + c + d + e = 15);
end;

procedure proc_ccreg_int16(a: int16);
begin
  Assert(a = 16);
end;

procedure proc_ccreg_int16_5(a, b, c, d, e: int16);
begin
  Assert(a + b + c + d + e = 150);
end;

procedure proc_ccreg_int32(a: int32);
begin
  Assert(a = 32);
end;

procedure proc_ccreg_int32_5(a, b, c, d, e: int32);
begin
  Assert(a + b + c + d + e = 1500);
end;

procedure proc_ccreg_int64(a: int64);
begin
  Assert(a = 64);
end;

procedure proc_ccreg_int64_5(a, b, c, d, e: int64);
begin
  Assert(a + b + c + d + e = 15000);
end;

procedure proc_ccreg_F32(a: Single);
var
  s: single;
begin
  s := 1.5;
  Assert(a = s);
end;

procedure proc_ccreg_F64(a: Double);
begin
  // Assert(a = 3.6); round error !!!!!!!!!!!!!!!!
end;

procedure proc_ccreg_set8(a: TSet8);
begin
  assert(a = [TEnum8._a01]);
end;

procedure proc_ccreg_set16(a: TSet16);
begin
  assert(a = [TEnum16._a02]);
end;

procedure proc_ccreg_set32(a: TSet32);
begin
  assert(a = [TEnum32._a04]);
end;

procedure proc_ccreg_set64(a: TSet64);
begin
  assert(a = [TEnum64._a08]);
end;

procedure proc_ccreg_set80(a: TSet80);
begin
  assert(a = [16]);
end;

//---------------------------------------------------

function func_ccreg_i32: Int32;
begin
  Result := 32;
end;

function func_ccreg_i64: Int64;
begin
  Result := 6464646464646464;
end;

function func_ccreg_f32: Single;
begin
  Result := 32.32;
end;

function func_ccreg_f64: Double;
begin
  Result := 64.64;
end;

function func_ccreg_ustr: UnicodeString;
begin
  Result := 'UnicodeString';
end;

function func_ccreg_astr: AnsiString;
begin
  Result := 'AnsiString';
end;


function func_ccreg_TRect16: TRect16;
begin
  Result.x := 20;
end;

function func_ccreg_set8(a: TSet8): TSet8;
begin
  Result := a;
end;

function func_ccreg_set16(a: TSet16): TSet16;
begin
  Result := a;
end;

function func_ccreg_set24(a: TSet24): TSet24;
begin
  Result := a;
end;

function func_ccreg_set32(a: TSet32): TSet32;
begin
  Result := a;
end;

function func_ccreg_set48(a: TSet48): TSet48;
begin
  Result := a;
end;

function func_ccreg_set56(a: TSet56): TSet56;
begin
  Result := a;
end;

function func_ccreg_set64(a: TSet64): TSet64;
begin
  Result := a;
end;

function func_ccreg_set80(a: TSet80): TSet80;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect1(a: TSmallRect1): TSmallRect1;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect2(a: TSmallRect2): TSmallRect2;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect3(a: TSmallRect3): TSmallRect3;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect4(a: TSmallRect4): TSmallRect4;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect5(a: TSmallRect5): TSmallRect5;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect6(a: TSmallRect6): TSmallRect6;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect7(a: TSmallRect7): TSmallRect7;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect8(a: TSmallRect8): TSmallRect8;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect9(a: TSmallRect9): TSmallRect9;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect10(a: TSmallRect10): TSmallRect10;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallRect11(a: TSmallRect11): TSmallRect11;
begin
  Result := a;
end;

function func_ccreg_ret_TRect16(a: TRect16): TRect16;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray1(a: TSmallSArray1): TSmallSArray1;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray2(a: TSmallSArray2): TSmallSArray2;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray3(a: TSmallSArray3): TSmallSArray3;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray4(a: TSmallSArray4): TSmallSArray4;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray5(a: TSmallSArray5): TSmallSArray5;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray6(a: TSmallSArray6): TSmallSArray6;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray7(a: TSmallSArray7): TSmallSArray7;
begin
  Result := a;
end;

function func_ccreg_ret_TSmallSArray8(a: TSmallSArray8): TSmallSArray8;
begin
  Result := a;
end;

function func_ccreg_int_all(a1: int8;
                            a2: uint8;
                            a3: int16;
                            a4: uint16;
                            a5: int32;
                            a6: uint32;
                            a7: int64;
                            a8: uint64;
                            a9: AnsiChar;
                            a10: WideChar): Int64;
begin
  Result := a1 + a2 + a3 + a4 + a5 + Int32(a6) + a7 + a8 + ord(a9) + ord(a10);
end;


function func_ccreg_sum_TSArrays(a, b: TSArray): TIntArray; {$IFDEF CC_STDCALL}stdcall;{$ENDIF}
var
  i: Integer;
begin
  SetLength(Result, Length(a) + Length(b));
  for i := 0 to Length(a) - 1 do
    Result[i] := a[i];
  for i := 0 to Length(b) - 1 do
    Result[Length(a) + i] := Result[Length(a) + i] + b[i];
end;

function func_ccreg_achar(a, b: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(Max(Ord(a), Ord(b)));
end;

function func_ccreg_uchar(a, b: Char): Char;
begin
  Result := Char(Max(Ord(a), Ord(b)));
end;

procedure RegisterProcs;
begin
  RegisterProc('CAT', 'proc_ccreg_int8', @proc_ccreg_int8);
  RegisterProc('CAT', 'proc_ccreg_int8_2', @proc_ccreg_int8_2);
  RegisterProc('CAT', 'proc_ccreg_int8_3', @proc_ccreg_int8_3);
  RegisterProc('CAT', 'proc_ccreg_int8_4', @proc_ccreg_int8_4);
  RegisterProc('CAT', 'proc_ccreg_int8_5', @proc_ccreg_int8_5);
  RegisterProc('CAT', 'proc_ccreg_int16', @proc_ccreg_int16);
  RegisterProc('CAT', 'proc_ccreg_int16_5', @proc_ccreg_int16_5);
  RegisterProc('CAT', 'proc_ccreg_int32', @proc_ccreg_int32);
  RegisterProc('CAT', 'proc_ccreg_int32_5', @proc_ccreg_int32_5);
  RegisterProc('CAT', 'proc_ccreg_int64', @proc_ccreg_int64);
  RegisterProc('CAT', 'proc_ccreg_int64_5', @proc_ccreg_int64_5);


  RegisterProc('CAT', 'proc_ccreg_F32', @proc_ccreg_F32);
  RegisterProc('CAT', 'proc_ccreg_F64', @proc_ccreg_F64);

  RegisterProc('CAT', 'proc_ccreg_set8', @proc_ccreg_set8);
  RegisterProc('CAT', 'proc_ccreg_set16', @proc_ccreg_set16);
  RegisterProc('CAT', 'proc_ccreg_set32', @proc_ccreg_set32);
  RegisterProc('CAT', 'proc_ccreg_set64', @proc_ccreg_set64);
  RegisterProc('CAT', 'proc_ccreg_set80', @proc_ccreg_set80);


  RegisterProc('CAT', 'func_ccreg_int_all', @func_ccreg_int_all);

  RegisterProc('CAT', 'func_ccreg_i32', @func_ccreg_i32);
  RegisterProc('CAT', 'func_ccreg_i64', @func_ccreg_i64);

  RegisterProc('CAT', 'func_ccreg_f32', @func_ccreg_f32);
  RegisterProc('CAT', 'func_ccreg_f64', @func_ccreg_f64);

  RegisterProc('CAT', 'func_ccreg_achar', @func_ccreg_achar);
  RegisterProc('CAT', 'func_ccreg_uchar', @func_ccreg_uchar);

  RegisterProc('CAT', 'func_ccreg_ustr', @func_ccreg_ustr);
  RegisterProc('CAT', 'func_ccreg_astr', @func_ccreg_astr);
  RegisterProc('CAT', 'func_ccreg_TRect16', @func_ccreg_TRect16);

  RegisterProc('CAT', 'func_ccreg_set8', @func_ccreg_set8);
  RegisterProc('CAT', 'func_ccreg_set16', @func_ccreg_set16);
  RegisterProc('CAT', 'func_ccreg_set24', @func_ccreg_set24);
  RegisterProc('CAT', 'func_ccreg_set32', @func_ccreg_set32);
  RegisterProc('CAT', 'func_ccreg_set48', @func_ccreg_set48);
  RegisterProc('CAT', 'func_ccreg_set64', @func_ccreg_set64);
  RegisterProc('CAT', 'func_ccreg_set56', @func_ccreg_set56);
  RegisterProc('CAT', 'func_ccreg_set80', @func_ccreg_set80);

  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect1', @func_ccreg_ret_TSmallRect1);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect2', @func_ccreg_ret_TSmallRect2);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect3', @func_ccreg_ret_TSmallRect3);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect4', @func_ccreg_ret_TSmallRect4);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect5', @func_ccreg_ret_TSmallRect5);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect6', @func_ccreg_ret_TSmallRect6);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect7', @func_ccreg_ret_TSmallRect7);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect8', @func_ccreg_ret_TSmallRect8);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect9', @func_ccreg_ret_TSmallRect9);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect10', @func_ccreg_ret_TSmallRect10);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallRect11', @func_ccreg_ret_TSmallRect11);
  RegisterProc('CAT', 'func_ccreg_ret_TRect16', @func_ccreg_ret_TRect16);

  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray1', @func_ccreg_ret_TSmallSArray1);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray2', @func_ccreg_ret_TSmallSArray2);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray3', @func_ccreg_ret_TSmallSArray3);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray4', @func_ccreg_ret_TSmallSArray4);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray5', @func_ccreg_ret_TSmallSArray5);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray6', @func_ccreg_ret_TSmallSArray6);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray7', @func_ccreg_ret_TSmallSArray7);
  RegisterProc('CAT', 'func_ccreg_ret_TSmallSArray8', @func_ccreg_ret_TSmallSArray8);

  RegisterProc('CAT', 'func_ccreg_sum_TSArrays', @func_ccreg_sum_TSArrays);
end;


{var a, r: TSet24;
    rr, aa: TSet48;
    arr: TIntArray;}

initialization
  RegisterProcs;

{  SetLength(arr, 10);
  arr := nil;

  rr := [];
  aa := [TEnum48._a01];
  rr := func_ccreg_set48(aa);

//  proc_ccreg_int8_3(1, 2, 3);

//  proc_ccreg_int8_5(1, 2, 3, 4, 5);}



end.
