unit guid_0;

interface

implementation

var G1, G2: TGUID;

initialization
  G1 := G2;
 
finalization
  Assert(G1 = G2);

end.