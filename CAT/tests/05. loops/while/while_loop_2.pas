unit while_loop_2;

interface

var 
  G: Int32;

implementation

procedure Test;
var 
  x: Int32;
begin
  x := 0;
  while x < 100 do
    x := x + 1;
  G := x;
end;

initialization
  Test();  
  
finalization  
  Assert(G = 100);
  
end.