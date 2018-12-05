unit class_arc_weak_1;

interface
uses System;

implementation

var
  S1, S2: TObject;
   
weak 
  WC: TObject;

procedure Test;
begin
  S1 := TObject.Create();
  WC := S1;
  S2 := WC;
end;

initialization
  Test();

finalization
  Assert(Assigned(WC));
  Assert(S2 = S1); 
end.