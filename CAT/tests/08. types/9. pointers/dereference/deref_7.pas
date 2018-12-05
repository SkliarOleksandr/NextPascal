unit deref_7;

interface

implementation

var G1, G2: Int32;
    PT: ^Int32;
     
procedure Test;
begin
  G1 := 1;
  G2 := 1;
  PT := @G2;
  PT^ := G1 + PT^;   
end;

initialization
  Test();

finalization
  Assert(G2 = 2);
end.