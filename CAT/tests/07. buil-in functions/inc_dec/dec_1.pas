unit dec_1;

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
  i8 := 100;
  u8 := 100;
  i16 := 100;
  u16 := 100;
  i32 := 100;
  u32 := 100;
  i64 := 100;
  u64 := 100;
end;

procedure Test1;
begin
  dec(i8);
  dec(u8);
  dec(i16);
  dec(u16);
  dec(i32);
  dec(u32);
  dec(i64);
  dec(u64);
end;

procedure Test2;
begin
  dec(i8, 0);
  dec(u8, 1);
  dec(i16, 2);
  dec(u16, 3);
  dec(i32, 4);
  dec(u32, 5);
  dec(i64, 6);
  dec(u64, 7);
end;

procedure Test3;
var
  D: Int32;
begin
  D := 10;
  dec(i8, D);
  dec(u8, D);
  dec(i16, D);
  dec(u16, D);
  dec(i32, D);
  dec(u32, D);
  dec(i64, D);
  dec(u64, D);
end;

initialization
  Init();
  Test1();
  Test2();
  Test3();      

finalization
  Assert(i8 = 89);
  Assert(u8 = 88);
  Assert(i16 = 87);
  Assert(u16 = 86);
  Assert(i32 = 85);
  Assert(u32 = 84);
  Assert(i64 = 83);
  Assert(u64 = 82);

end.