unit result_darray;

interface

implementation

type
  TArr = array of Int32;

function Get: TArr;
begin
  SetLength(Result, 3);
  Result[0] := 11;
  Result[1] := 21;
  Result[2] := 31;     
end;

procedure Test;
var 
  A: TArr;
begin
  A := Get();
  Assert(A[0] = 11);
  Assert(A[1] = 21);
  Assert(A[2] = 31);    
end;

initialization
  Test();

finalization

end.