unit deref_1;

interface

implementation

var 
  G1, G2: Int32;
  P: ^Int32;
    
procedure Test;
begin
  G1 := 111;
  P := @G1;
  G2 := P^;
end;

initialization
  Test();

finalization
  Assert(G2 = G1);
end.