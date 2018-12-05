unit shr_1;

interface

implementation

var
  i8: Int8;
  i16: Int16;
  i32: Int32;
  i64: Int64;

procedure Init;
begin
  i8 := 9;
  i16 := 9;
  i32 := 9;
  i64 := 9;
end;

procedure Test;
begin
  i8 := i8 shr 1;
  i16 := i16 shr 1;
  i32 := i32 shr 1;
  i64 := i64 shr 1;      
end;

initialization
  Init();
  Test();

finalization
  Assert(i8 = 4);
  Assert(i16 = 4);
  Assert(i32 = 4);
  Assert(i64 = 4);
end.