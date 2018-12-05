unit ifthen_3;

interface

implementation

var
  G: Int32;

procedure Test;
var
  B: Boolean;
begin
  B := True;
  if B then
    G := 2
  else
    G := 1;  
end;

initialization
  Test();

finalization
  Assert(G = 2);

end.