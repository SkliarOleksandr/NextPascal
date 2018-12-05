unit refcount_3;

interface

implementation

var
  A1, A2: array of int32;
  C: Int32;
   
procedure Test;
begin
  A1 := [1, 2, 3, 4, 5];
  A2 := A1;
  C := refcount(A1);
end;

initialization
  Test();

finalization
  Assert(C = -1);
end.