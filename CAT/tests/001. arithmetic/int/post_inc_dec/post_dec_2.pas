unit post_dec_2;

interface

implementation

var A: Int32 = -1;
var G1, G2: Int32;

procedure Test;
begin  
  G1 := a-- + a--;
  G2 := a-- + a--;       
end;

initialization
  Test();

finalization
  Assert(A = -5);
  Assert(G1 = -3);
  Assert(G2 = -7);  
end.