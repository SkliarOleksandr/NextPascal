unit sarray_init_2;

interface

implementation

type TArr = array [2] of string;

procedure Test;
var 
  A: TArr;
  S: string;
begin
  S := copy('str0');
  A[0] := S;
  A[1] := S;
  Assert(A[0] = S);
  Assert(A[1] = S);            
end;

initialization
  Test();

finalization

end.