unit iif_1;

interface

implementation

var 
  G1, G2: Int32;
  a, b: Int32;
  
procedure Test;
begin
  a := 4;
  b := 3;
  G1 := iif(a > b, 11, 22);
  G2 := iif(a < b, 11, 22);  
end;

initialization
  Test();

finalization
  Assert(G1 = 11);
  Assert(G2 = 22);  
end.