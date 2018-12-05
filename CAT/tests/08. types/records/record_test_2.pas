unit record_test_2;
interface

type
  TRec = record
    i8: Int8;   
    u8: UInt8;    
    i16: Int16;
    u16: UInt16;    
    i32: Int32;   
    u32: UInt32;     
    i64: Int64;   
    u64: UInt64;                                 
    f32: float64;           
    f64: float64;
    c: char;
    b: Boolean;
  end;    
  
implementation  
  
var
  R1, R2: TRec;  
  _i8: Int8;
  _u8: UInt8;    
  _i16: Int16;
  _u16: UInt16;    
  _i32: Int32;
  _u32: UInt32;    
  _i64: Int64;
  _u64: UInt64;
  _f32: float64;  
  _f64: float64;
  _c: Char;
  _b: Boolean;  
  
procedure Test;
begin
  R1.i8 := 1;
  R1.u8 := 2;  
  R1.i16 := 3;
  R1.u16 := 4;  
  R1.i32 := 5; 
  R1.u32 := 6;  
  R1.i64 := 7;  
  R1.u64 := 8;       
  R1.f32 := 9.1;
  R1.f64 := 9.2;  
  R1.c := 'A';
  R1.b := True;  
   
  R2 := R1;
  
  _i8 := R2.i8;
  _u8 := R2.u8;    
  _i16 := R2.i16;
  _u16 := R2.u16;    
  _i32 := R2.i32;
  _u32 := R2.u32;    
  _i64 := R2.i64;
  _u64 := R2.u64;
  _f32 := R2.f32;  
  _f64 := R2.f64;
  _c := R2.c;
  _b := R2.b;    
end;

initialization
  Test();
  
finalization
  Assert(_i8 = 1);
  Assert(_u8 = 2);
  Assert(_i16 = 3);
  Assert(_u16 = 4);
  Assert(_i32 = 5);
  Assert(_u32 = 6);
  Assert(_i64 = 7);
  Assert(_u64 = 8);
  Assert(_f32 = 9.1);
  Assert(_f64 = 9.2);
  Assert(_c = 'A'); 
  Assert(_b = True); 
end.    