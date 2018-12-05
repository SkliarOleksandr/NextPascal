unit inc_1;

interface

implementation

var 
  i8: int8;
  u8: uint8;
  i16: int16;
  u16: uint16;
  i32: int32;
  u32: uint32;
  i64: int64;
  u64: uint64;

procedure Init;
begin
  i8 := 0;
  u8 := 0;
  i16 := 0;
  u16 := 0;
  i32 := 0;
  u32 := 0;
  i64 := 0;
  u64 := 0;
end;


procedure Test1;
begin
  inc(i8);
  inc(u8);
  inc(i16);
  inc(u16);
  inc(i32);
  inc(u32);
  inc(i64);
  inc(u64);
end;

procedure Test2;
begin
  inc(i8, 0);
  inc(u8, 1);
  inc(i16, 2);
  inc(u16, 3);
  inc(i32, 4);
  inc(u32, 5);
  inc(i64, 6);
  inc(u64, 7);
end;

procedure Test3;
var
  D: Int32;
begin
  D := 10;
  inc(i8, D);
  inc(u8, D);
  inc(i16, D);
  inc(u16, D);
  inc(i32, D);
  inc(u32, D);
  inc(i64, D);
  inc(u64, D);
end;

initialization
  Init();
  Test1();
  Test2();
  Test3();      

finalization
  Assert(i8 = 11);
  Assert(u8 = 12);
  Assert(i16 = 13);
  Assert(u16 = 14);
  Assert(i32 = 15);
  Assert(u32 = 16);
  Assert(i64 = 17);
  Assert(u64 = 18);
end.