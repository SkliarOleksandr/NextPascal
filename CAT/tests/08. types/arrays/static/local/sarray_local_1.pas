unit sarray_local_1;

interface

implementation

type
  TArr = array [3] of Int32;

procedure Test;
var
  A: TArr;
begin
  A[0] := 1;
  A[1] := 2;
  A[2] := 3;
  Assert(A[0] = 1);
  Assert(A[1] = 2);
  Assert(A[2] = 3);     
end;

initialization
  Test();

finalization

end.