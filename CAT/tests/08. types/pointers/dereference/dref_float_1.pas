unit dref_float_1;

interface

implementation

var 
  G1, G2: Float32;
  P: ^Float32;
    
procedure Test;
begin
  G1 := 5.12;
  P := @G1;
  G2 := P^;
end;

initialization
  Test();

finalization
  Assert(G2 = G1);

end.