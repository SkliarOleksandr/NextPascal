unit int_32_2;

interface

implementation

var G1, G2: Int32; 
    G3: Int8;

procedure Test;
begin
  G1 := 0;
  G2 := 128;
  G3 := G1 - G2;
end;

initialization
  Test();

finalization
  Assert(G3 = -128);
end.