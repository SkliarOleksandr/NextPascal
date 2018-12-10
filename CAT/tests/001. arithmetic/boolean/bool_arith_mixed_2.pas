unit bool_arith_mixed_2;

interface

implementation
    
var G: boolean;    
    
function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

procedure Test;
begin
  G := GetBool(True) or (GetBool(False) and GetBool(True));
end;

initialization
  Test();

finalization
  Assert(G);
end.