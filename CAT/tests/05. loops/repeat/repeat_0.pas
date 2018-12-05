unit repeat_0;

interface

implementation

var G: Int32;

procedure Test;
begin
  G := 0;
  repeat
    G := G + 1;
    if G > 8 then
      break;
  until G = 10; 
  G := G*10;
end;

initialization
  Test();

finalization
  Assert(G = 90);
end.