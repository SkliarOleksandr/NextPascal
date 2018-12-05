unit sarray_2dim_1;

interface

type
  TA = array [2, 2] of Int32; 

var
  A: TA;

implementation

procedure Test;
begin
  A[0, 0] := 1;
  A[0, 1] := 2;
  A[1, 0] := 3;
  A[1, 1] := 4;      
end;

initialization
  Test();

finalization
  Assert(A[1, 1] = 4);
end.