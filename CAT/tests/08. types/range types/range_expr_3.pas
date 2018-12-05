unit range_expr_3;

interface

implementation

var
  G1, G2: Boolean;
 
procedure Test;
var
  a: Int32;
begin
  a := 2;
  G1 := a in 1..5;
  a := 0;
  G2 := a in 1..5;
end;

initialization
  Test();

finalization
  Assert(G1 = True);
  Assert(G2 = False);  
end.