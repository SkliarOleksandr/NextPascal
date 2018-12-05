unit while_loop_3;

interface

var 
  G: Int32;

implementation

procedure Test;
var 
  x, y: Int32;
begin 
  G := 0;
  x := 0;  
  while x < 10 do
  begin
    y := 0; 
    while y < 10 do
    begin
      y := y + 1;
      G := G + 1; 
    end;         	  
    x := x + 1;
  end;	  
end;

initialization
  Test();  
  
finalization  
  Assert(G = 100);
  
end.