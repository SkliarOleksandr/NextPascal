unit while_loop_1;

interface

var 
  G: Int32;

implementation

procedure Test;
begin
  while False do
    G := 12;
  
  while True do
  begin   
    G := 14;
    Break;
  end;    
  
  while False do
    while True do G := 13;
end;

initialization
  Test();  
  
finalization  
  Assert(G = 14);
  
end.