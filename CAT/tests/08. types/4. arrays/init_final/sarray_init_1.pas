unit sarray_init_1;

interface

implementation

type TArr = array [3] of string;

var A: TArr;
    S: string;

procedure Test;
begin
  A[0] := S;
  A[1] := S;
  A[2] := S;     
end;

initialization
  S := copy('string');
  Test();

finalization
  Assert(A[2] = 'string');
end.