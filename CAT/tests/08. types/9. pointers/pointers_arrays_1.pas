unit pointers_arrays_1;

interface

implementation

type
  TA = array [2] of Int32;
  PA = ^TA;

var
  A: TA = (11, 22);
  P: PA;

procedure Test;
begin
  P := @A;
  P[0] := 33;
end;

initialization
  Test();

finalization
  Assert(A[0] = 33);
end.