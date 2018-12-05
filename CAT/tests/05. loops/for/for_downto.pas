unit for_downto;

interface

implementation

var
  G: Int32;

procedure Test;
var
  i: Int32;
begin
  G := 0;
  for i := 9 downto 0 do 
    G := G + 1;
end;

initialization
  Test();

finalization
  Assert(G = 10);

end.