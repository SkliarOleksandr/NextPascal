unit refcount_4;

interface

implementation

var
  A1, A2: array of int32;
  C: Int32;
   
procedure Test;
begin
  SetLength(A1, 2);
  A2 := A1;
  C := refcount(A1);
end;

initialization
  Test();

finalization
  Assert(C = 2);

end.