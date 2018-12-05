unit generic_sarrays_1;

interface

implementation

type
  TSArray<T> = array[0..2] of T;

var
  A: TSArray<Int32>;
  
procedure Test;
begin
  A[0] := 1;
  A[1] := 2;
  A[2] := 3;      
end;

initialization
  Test();

finalization
  Assert(A[2] = 3); 
end.