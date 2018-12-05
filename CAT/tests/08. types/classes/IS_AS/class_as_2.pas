unit class_as_2;

interface

implementation

uses System;

type 
  TC1 = class(TObject) end;
  TC2 = class(TC1) end;   
  TC3 = class(TC2) end;  

var 
  C0, C1, C2, C3:  TObject;

procedure Test;
begin
  C0 := TC2.Create();
  C1 := C0 as TC1; 
  C2 := C0 as TC2;
  //C3 := C0 as TC3; // exception 
end;

initialization
  Test();

finalization
  Assert(C1 = C0);
  Assert(C2 = C0);
  //Assert(C3 = nil);    
end.