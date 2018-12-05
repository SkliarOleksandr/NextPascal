unit repeat_3loop_speed_test;

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
  x := 0;
  repeat
    y := 0;
    repeat
      z := 0;
      repeat       
        G := x + y + z;
        z := z + 1;
      until z = c;      
      y := y + 1;
    until y = c;  
    x := x + 1;    
  until x = c;  
end;

initialization
  Test();

finalization
  Assert(G = 117);
end.