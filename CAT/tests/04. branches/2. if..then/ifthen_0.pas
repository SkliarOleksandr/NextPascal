unit ifthen_0;

interface

var
  G: Int32;
    
implementation

procedure Test;
begin
  G := 1;
  if G > 1 then
    G := 1
  else 
    G := 2;    
end;

initialization
  Test();

finalization  
  Assert(G = 2);
  
end.