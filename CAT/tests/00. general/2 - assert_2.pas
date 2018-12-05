unit accert_2;

interface

var
  G: Int32;
  
implementation
  
procedure Test;
begin
  G := 1;
  Assert(G = 1);
  G := 2;
  Assert(G = 2);
  G := 3;        
end;

initialization
  Test();

finalization  
  Assert(G = 3);

end.