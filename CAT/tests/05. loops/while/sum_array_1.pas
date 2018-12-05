unit sum_array_1;

interface

implementation

var A: array of Int32;
    G: Int32;


function SUM(I: Int32): Int32;
begin 
  G := G + I;
  Result := I;    
end;

procedure Test;
var
  i: Int32;
begin
  i := 0; 
  A := [1, 2, 3, 4, 5, 6, 0];
  while SUM(A[i]) <> 0 do
    Inc(I);
end;

initialization
  G := 0;
  Test();

finalization
  Assert(G = 21);
end.