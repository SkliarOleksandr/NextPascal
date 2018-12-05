unit range_expr_5;

interface

implementation

var
  G1, G2: Boolean;
 
function InRange(Value: Int32; LoBound, HiBound: Int32): Boolean;
begin
  Result := Value in LoBound..HiBound;
end; 
 
procedure Test;
begin
  G1 := InRange(6, 0, 5);
  G1 := InRange(2, 1, 6);  
end;

initialization
  Test();

finalization
  Assert(G1 = True);
  Assert(G2 = False);  
end.