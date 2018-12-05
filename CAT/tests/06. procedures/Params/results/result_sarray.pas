unit result_sarray;

interface

implementation

type
  TArr = array [3] of Int32;

function Get: TArr;
begin
  Result[0] := 1;
  Result[1] := 2;
  Result[2] := 3;     
end;

procedure Test;
var 
  A: TArr;
begin
  A := Get();
  Assert(A[0] = 1);
  Assert(A[1] = 2);
  Assert(A[2] = 3);    
end;

initialization
  Test();

finalization

end.