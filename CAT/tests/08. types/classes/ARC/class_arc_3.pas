unit class_arc_3;

interface

implementation

uses system;

type 
  TC = class
    F: TC;    
  end;
     
var 
  C: TC;

procedure Test;
begin
  C.F := C;
end;

initialization
  C := TC.Create(); 
  Test();

finalization
  Assert(C = C.F);
  C.F := nil;

end.