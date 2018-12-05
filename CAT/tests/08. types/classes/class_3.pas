unit class_3;

interface

implementation

uses System;

type 
  TC = class
    F: Int32;
    procedure SetF; 
  end; 

procedure TC.SetF;
var
  C: TC; 
begin
  C := Self;
  C.F := 12; 
end;

var
  Obj: TC;  

procedure Test;
begin
  Obj := TC.Create();
  Obj.SetF();
end;

initialization
  Test();

finalization

end.