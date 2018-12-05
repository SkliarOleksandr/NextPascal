unit copy_dynarray_1;

interface

implementation

var A1, A2: array of Int32;

procedure Test;
begin
  A1 := [1, 2, 3];
  A2 := copy(A1, 1, 2);
end;

initialization
  Test();

finalization
  Assert(A2[0] = A1[1]); 
  Assert(A2[1] = A1[2]);  
end.