unit gvar_init_2;

interface

implementation

var 
  i8: Int8 = -127;
  u8: UInt8 = +255;
  i16: Int16 = -32000;
  u16: UInt16 = +32000;
  i32: Int32 = -2000000000;
  u32: UInt32 = 4000000000; 
  i64: Int64 = -400000000000000000; 
  u64: UInt64 = 400000000000000000;  
    b: Boolean = True;  
  f32: float32 = 5.2;
  f64: float32 = 0.3332;
  ach: AnsiChar = 'A';
  uch: Char = '׳';      
  astr: AnsiString = 'asdf';
  ustr: String = 'אבגדה';  
  Ptr: Pointer = nil;       
       

initialization
  Assert(i8 = -127);
  Assert(u8 = 255);
  Assert(i16 = -32000);
  Assert(u16 = 32000);
  Assert(i32 = -2000000000);
  Assert(u32 = 4000000000); 
  Assert(i64 = -400000000000000000); 
  Assert(u64 = 400000000000000000);  
  Assert(  b = True);  
  Assert(f32 = 5.2);
  Assert(f64 = 0.3332);     
  Assert(ach = 'A'); 
  Assert(uch = '׳');    
  Assert(astr = 'asdf');   
  Assert(ustr = 'אבגדה');
  Assert(Ptr = nil);        
      
finalization

end.