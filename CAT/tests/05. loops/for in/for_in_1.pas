unit for_in_1;

interface

implementation

var
   A: array [10] of int32;
   G: Int32;

procedure Test;
var
  i: int32;
begin
  for i := Low(a) to High(A) do
    A[i] := i; 

  G := 0;
  for i in a do 
  begin
    G := G + i;
  end;  
end;

initialization
  Test();

finalization
  Assert(G = 45);
end.