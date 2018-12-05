unit constexpr_2;

interface

implementation

function Add(a, b: Int32): Int32; pure;
begin
  Result := a + b;
end;

var G1, G2: Int32;

procedure Test;
begin
  G1 := Add(1, 2);    
  G2 := Add(G1, 1);
end;

initialization
  Test();

finalization
  Assert(G1 = 3);
  Assert(G2 = 4);
end.