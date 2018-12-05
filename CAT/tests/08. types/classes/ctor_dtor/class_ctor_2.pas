unit class_ctor_2;
interface

uses System;

type
  
 TC1 = class
    F: int32;    
    constructor Create; 
    destructor Destroy; override;
  end;    
  
implementation  
  
var
  GA, GF: Int32; 
   
constructor TC1.Create; 
begin
  F := 11;
end; 

destructor TC1.Destroy; 
begin
  GF := 12;
end; 
  
procedure Test;
var
  Obj: TC1;  
  A: Int32;
begin
  Obj := TC1.Create();
  A := Obj.F;
  Obj := nil;
  GA := A;
end;

initialization
  Test();
  
finalization
  Assert(GA = 11);
  Assert(GF = 12);

end.  