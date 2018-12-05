unit dynarray_of_string_1;

interface

implementation

var A: array of string;

procedure Test;
begin
  SetLength(A, 1);
  A[0] := 'ASDF';
  SetLength(A, 2);
  A[1] := A[0];  
end;

initialization
  Test();

finalization
  Assert(A[0] = 'ASDF');
  Assert(A[1] = 'ASDF');   
end.