unit consts_float_0;

interface

implementation
            
var G: Float32;            
            
procedure Test;
begin
  G := 2.4;
end;

initialization
  Test();

finalization
  Assert(G = 2.4);
end.