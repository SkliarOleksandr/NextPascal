unit class_init_0;

interface

implementation

uses System;

type
  TC = class
    a: Int32 = 3;    // todo ...
    b: Int32 = 5;    // todo ...
  end;

var
  G1, G2: Int32;
  Obj: TC;

procedure Test;
begin
  Obj := TC.Create(); 
  G1 := Obj.a;
  G2 := Obj.b;  
end;

initialization
  Test();

finalization

end.