unit class_packed_1;

interface

implementation

uses System;

type
  TC = packed class
    a, b: Boolean;
  end;
  
var 
  C: TC;
  x, y: Boolean;  

procedure Test;
begin
  C := TC.Create();
  C.a := True;
  C.b := True;
  x := C.a;
  y := C.b;  
end;

initialization
  Test();

finalization
  Assert(x);
  Assert(y);  
end.