unit sarray_managed_1;

interface

implementation

var A: array [3] of string;
    S: string;

procedure Test;
begin
  A[0] := S;
  A[1] := S;
  A[2] := S;    
end;

initialization
  S := Copy('string'); 
  Test();

finalization
  Assert(A[0] = 'string');
  Assert(A[1] = 'string');
  Assert(A[2] = 'string');    
end.