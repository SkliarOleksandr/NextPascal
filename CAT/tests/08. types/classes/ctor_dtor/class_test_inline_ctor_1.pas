unit class_test_1;
interface

uses System;

type
  
  TC1 = class
    a, b: int32;    
    constructor Create; inline; 
    destructor Destroy; override;       
  end;    
  
implementation  
  
var
  C: TC1;  
  GA, GB, GC: Int32;
  
destructor TC1.Destroy;
begin
  GC := 33;  
end;    
  
constructor TC1.Create; 
begin
  a := 11;
  b := 22;
end; 
  
procedure Test;
begin
  C := TC1.Create();
  GA := C.a; 
  GB := C.b;  
end;

initialization
  Test();
  
finalization 
  C := nil;
  Assert(GA = 11);
  Assert(GB = 22);
  Assert(GC = 33);  
   
end.  