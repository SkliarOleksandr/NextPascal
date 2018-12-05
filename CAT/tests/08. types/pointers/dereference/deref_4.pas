unit deref_4;

interface

implementation

var 
  G1, G2: Boolean;
  P: ^Boolean;
    
procedure Test;
begin
  G1 := True;
  P := @G1;
  G2 := P^;
end;

initialization
  Test();

finalization
  Assert(G2 = G1);

end.