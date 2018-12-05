unit low_high_4;

interface

implementation

var
  a: array of int32;

procedure Test;
begin
  SetLength(a, 10);
  for var i := Low(a) to High(a) do
    a[i] := i;
end;

initialization
  Test();

finalization
  Assert(A[High(a)] = High(a));
  
end.