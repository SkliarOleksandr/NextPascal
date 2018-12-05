unit dref_float_2;

interface

implementation

var 
  G1, G2: Float64;
  P: ^Float64;
    
procedure Test;
begin
  G1 := 5.1245789;
  P := @G1;
  G2 := P^;
end;

initialization
  Test();

finalization
  Assert(G2 = G1);

end.