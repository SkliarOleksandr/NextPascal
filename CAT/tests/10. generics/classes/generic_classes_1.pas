unit generic_classes_1;

interface

implementation

uses System;

type
  TC<T> = class  
    A: T;   
  end; 

var
  C: TC<Int32>;
  G: Int32;

procedure Test;
begin
  C := TC<Int32>.Create(); 
  C.A := 55;
  G := C.A;
end;

initialization
  Test();

finalization
  Assert(G = 55);
  
end.