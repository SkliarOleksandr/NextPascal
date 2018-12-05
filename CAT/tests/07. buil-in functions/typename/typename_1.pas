unit typename_1;

interface

implementation

var 
  i8: Int8;
  i16: Int16;
  i32: Int32;
  i64: Int64;  
  u8: UInt8;
  u16: UInt16;
  u32: UInt32;  
  u64: UInt64;
  f32: float32;
  f64: float64;  
  b: Boolean;
   
procedure Test;
begin
  Assert(typename(i8) = 'Int8');
  Assert(typename(i16) = 'Int16'); 
  Assert(typename(i32) = 'Int32'); 
  Assert(typename(i64) = 'Int64');
  Assert(typename(u8) = 'UInt8');
  Assert(typename(u16) = 'UInt16'); 
  Assert(typename(u32) = 'UInt32'); 
  Assert(typename(u64) = 'UInt64');
  Assert(typename(f32) = 'Float32'); 
  Assert(typename(f64) = 'Float64');
  Assert(typename(b) = 'Boolean');                  
end;

initialization
  Test();

finalization

end.