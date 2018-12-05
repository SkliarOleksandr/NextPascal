unit range_expr_1;

interface

implementation

var
  G1, G2, G3: Boolean;

procedure Test;
begin
  G1 := 1 in 0..5;
  G2 := 6 in 0..5;    
end;

initialization
  Test();

finalization
  Assert(G1);
  Assert(not G2);   
end.