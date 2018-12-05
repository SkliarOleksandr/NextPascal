unit Unit1;

interface

implementation

var
  i8: Int8;
  i16: Int16;
  i32: Int32; 
  i64: Int64;         
  ui8: UInt8;
  ui16: UInt16;
  ui32: UInt32; 
  ui64: UInt64;
  f32: float32;
  f64: float64;
  s16: string;
  s8: ansistring;
  b: boolean; 
  ni: NativeInt;
  nui: NativeUInt;
  
procedure Test;
begin
  i8   := 1;
  i16  := 1;
  i32  := 1;
  i64  := 1;
  ui8  := 1;
  ui16 := 1;
  ui32 := 1; 
  ui64 := 1;
  f32  := 1.0;
  f64  := 1.0;
  s16  := 'asdf';
  s8   := 'asdf'; 
  b    := true;    
  ni := -12345;  
  nui := 12345;    
end; 

initialization
  Test();

end.