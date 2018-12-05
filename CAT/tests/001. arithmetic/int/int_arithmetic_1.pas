unit int_arithmetic_1;

interface

implementation

var
  G, x, y, z: int32;

procedure Test;
begin
  G := x + y + z;
end;

initialization
  x := 1;
  y := 2;
  z := 3;
  Test();

finalization
  Assert(G = 6);
end.