unit class_arc_weak_3;

interface

implementation

uses System;

procedure Test;
var
  C1, C2: TObject;
var
  B: Boolean;
weak
  W:  TObject;
begin
  C1 := TObject.Create();
  W := C1;
  B := GetRef(W, C2);
  Assert(B);
  Assert(C2 <> nil);
  /////////////////////////  
  C1 := nil;
  C2 := nil;
  B := GetRef(W, C2);
  Assert(not B);
  Assert(C2 = nil);            
end;

initialization
  Test();

finalization

end.