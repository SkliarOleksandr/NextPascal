unit consts_float_1;

interface

implementation

const 
  F1 = 1.123;
  F2 = -1.321;
  
var
  G1, G2: Float32;  

procedure Test;
begin
  G1 := F1;
  G2 := F2;
end;

initialization
  Test();

finalization
  Assert(G1 = F1);
  Assert(G2 = F2);  
end.