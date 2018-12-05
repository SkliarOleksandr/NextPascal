unit generic_darrays_1;

interface

implementation

type
  TDArray<T> = array of T;
  
  
var
  A: TDArray<Int32>;
  G1, G2, G3: Int32;   

procedure Test;
begin
  SetLength(A, 3);
  A[0] := 11;
  A[1] := 12;
  A[2] := 13;
  G1 := A[0];
  G2 := A[1];
  G3 := A[2];       
end;

initialization
  Test();

finalization
  Assert(G1 = 11);
  Assert(G2 = 12);
  Assert(G3 = 13);    
end.