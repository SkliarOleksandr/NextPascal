unit shl_2;

interface

implementation

var
  i8: UInt8;
  i16: UInt16;
  i32: UInt32;
  i64: UInt64;

procedure Init;
begin
  i8 := 9;
  i16 := 9;
  i32 := 9;
  i64 := 9;
end;

procedure Test;
begin
  i8 := i8 shl 1;
  i16 := i16 shl 1;
  i32 := i32 shl 1;
  i64 := i64 shl 1;      
end;

initialization
  Init();
  Test();

finalization
  Assert(i8 = 18);
  Assert(i16 = 18);
  Assert(i32 = 18);
  Assert(i64 = 18);
end.