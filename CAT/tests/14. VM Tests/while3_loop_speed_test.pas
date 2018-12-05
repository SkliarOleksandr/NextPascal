unit while3_loop_speed_test;

interface

implementation

var
  G: Int32;

procedure Test;
const
  c = 40;
var 
  x, y, z : Int32;
begin
  G := 0;
  x := 0;
  while x < c do begin
    y := 0;
    while y < c do begin
      z := 0;
      while z < c do begin
        G := x + y + z;
        z := z + 1;
      end;
      y := y + 1;
    end;
    x := x + 1;
  end;
end;

initialization
  Test();

finalization
  Assert(G = 117);
end.