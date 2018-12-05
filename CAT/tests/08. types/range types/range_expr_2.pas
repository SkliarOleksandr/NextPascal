unit range_expr_2;

interface

implementation

var
  G1, G2: Boolean;
 
procedure Test;
var
  a, b: Int32;
begin
  a := 1;
  b := 5;
  G1 := 6 in a..b;
  G2 := 1 in a..b;  
end;

initialization
  Test();

finalization
  Assert(G1 = False);
  Assert(G2 = True);  

end.