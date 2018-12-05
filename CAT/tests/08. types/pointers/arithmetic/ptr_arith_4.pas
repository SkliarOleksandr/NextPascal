unit ptr_arith_4;

interface

implementation

var Arr: array [2] of Int16 = ($0001, $0002);
    P: ^Int16; 
    G1, G2: Int16;
    
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
  Assert(G1 = 1);
  Assert(G2 = 2);  
end.