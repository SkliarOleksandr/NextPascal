unit for_1;

interface

implementation

var G: Int32 = 0;

procedure Test;
begin
  for var i := 0 to 10 do
    Inc(G);
end;

initialization
  Test();

finalization
  Assert(G = 11);
end.