unit ifthen_2;

interface

implementation

var
  G: int32;

procedure Test;
var
  x, y, z: int32;
begin
  x := 1;
  y := 2;
  z := 3;
  if (x = 1) or (y = 0) or (z = 0) then
    G := 1
  else
    G := 2;   
end;

initialization
  Test();

finalization
  Assert(G = 1);
end.