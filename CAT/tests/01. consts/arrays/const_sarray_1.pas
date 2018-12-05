unit const_sarray_1;

interface

implementation

const A: array [4] of Int32 = (1, 2, 3, 4);

var G1, G2: Int32;

procedure Test;
begin
  G1 := A[0];
  G2 := A[3];  
end;

initialization
  Test();

finalization
  Assert(G1 = 1);
  Assert(G2 = 4);  
end.

