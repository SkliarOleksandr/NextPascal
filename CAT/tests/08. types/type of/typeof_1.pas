unit typeof_1;

interface

var G1: Int32;
    G2: type of G1;

implementation

procedure Test;
begin
  G1 := 5;
  G2 := G1;
end;

initialization
  Test();

finalization
  Assert(G1 = G2);

end.