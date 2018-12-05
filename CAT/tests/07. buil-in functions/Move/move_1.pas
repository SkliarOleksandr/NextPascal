unit move_1;

interface

implementation

var A: array [3] of Int8 = (1, 2, 3);

procedure Test;
begin
  Move(A, 1, A, 0, 1); 
end;

initialization
  Test();

finalization
  Assert(A[0] = 2);   
end.