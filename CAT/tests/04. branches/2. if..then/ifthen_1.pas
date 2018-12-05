unit ifthen_1;

interface

implementation

var
  G: Int32;

function GetFalse: Boolean;
begin
  Result := False;
end;

procedure Test;
begin
  if GetFalse() then
    G := 1
  else
    G := 2;  
end;

initialization
  Test();

finalization
  Assert(G = 2);

end.