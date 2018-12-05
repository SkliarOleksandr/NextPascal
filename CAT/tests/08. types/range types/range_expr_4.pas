unit range_expr_4;

interface

implementation

var
  G1, G2: Boolean;
 
function GetA: Int32;
begin
  Result := 2;
end;

function GetB: Int32;
begin
  Result := 7;
end; 
 
procedure Test;
begin
  G1 := GetA() in 1..5;
  G2 := GetB() in 1..5;
end;

initialization
  Test();

finalization
  Assert(G1 = True);
  Assert(G2 = False);  

end.