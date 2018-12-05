unit class_ctor_1;
interface

uses System;

type
  
 TC1 = class
    a, b, c: int32;    
    constructor Create; 
  end;    
  
implementation  
  
var
  C: TC1;  
  GA, GB, GC: Int32;
  
  
constructor TC1.Create; 
begin
  a := 11;
  b := 22;
  c := 33;
end; 
  
procedure Test;
begin
  C := TC1.Create();
  GA := C.a;
  GB := C.b;
  GC := C.c;    
end;

initialization
  Test();
  
finalization
  Assert(GA = 11);
  Assert(GB = 22);  
  Assert(GC = 33);  
end.  