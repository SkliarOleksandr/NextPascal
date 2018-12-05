unit array_dynamic_4;

interface

implementation

type
  TIntArray = array of int32;
  
var 
  A: TIntArray;
  G, X: Int32;
 

procedure Test(Arr: TIntArray);
var
  L, R: Int32;
begin
  L := 0;
  R := 9;
  X := L + (R - L) shr 1; 
  G := Arr[X];
end;

initialization
  A := [4, 7, 1 , 6, 2, 0, 3, 5, 9, 8];
  Test(A);

finalization
  Assert(G = 2);
end.