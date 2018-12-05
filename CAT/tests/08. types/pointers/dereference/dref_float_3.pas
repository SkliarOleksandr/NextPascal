unit dref_float_3;

interface

implementation

var 
  G1: Float32; 
  G2: Float64;
  P: ^Float32;
    
procedure Test;
begin
  G1 := 0.124;
  P := @G1;
  G2 := P^;
end;

initialization
  Test();

finalization
  Assert(G2 = G1);

end.