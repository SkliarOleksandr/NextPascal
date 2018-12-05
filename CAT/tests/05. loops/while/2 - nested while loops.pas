unit Unit1;

interface

var 
  G: Int32;

implementation


procedure Test;
const
  c = 10;
var 
  x, y, z, r : Int32;
  
begin
  r := 0;
  x := 0;    
  while x < c do begin
    y := 0;
    while y < c do begin
      z := 0;
      while z < c do begin
        r := r + 1;
        z := z + 1;
      end;  
      y := y + 1;
    end;   
    x := x + 1;
  end;
  G := r;
end;

initialization
  Test();
  
finalization  
  Assert(G = 1000);
  
end.