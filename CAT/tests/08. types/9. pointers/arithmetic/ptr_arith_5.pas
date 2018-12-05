unit ptr_arith_5;

interface

implementation

var Arr: array [2] of Int32 = ($A, $B);
    P: ^Int32; 
    G1, G2: Int32;
    
procedure Test;
begin
  P := @Arr[0];
  G1 := P^;
  Inc(P);
  G2 := P^;  
end;

initialization
  Test();

finalization
  Assert(G1 = $A);
  Assert(G2 = $B);  

end.