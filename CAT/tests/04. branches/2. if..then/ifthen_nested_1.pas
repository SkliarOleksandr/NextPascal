unit ifthen_nested_1;

interface

var
  G: Int32;
    
implementation

procedure Test;
begin
  G := 1;
  if G = 1 then
  begin
    G := 10;
    if G = 2 then
      G := 200
    else
      G := 300;       
  end else 
  begin
    if G = 2 then
      G := 2000
    else
      G := 3000;
  end;       
end;

initialization
  Test();

finalization
  Assert(G = 300);
end.