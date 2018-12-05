unit int_32_3;

interface

implementation

var G: Int32;

procedure Test;
var
  x, y, z, a, b, c: Int32;
begin
  x := -1;
  y := -2;
  z := -3;
  a := x + y;
  b := x + z;
  c := a + b;
  G := x*a + y*b + z*c; 
end;

initialization
  Test();

finalization
  Assert(G = 32);
end.