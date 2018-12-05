unit constexpr_recursion_1;

interface

implementation

// todo: recursion call dont allow drop unused procs...               
function Calc(Val, Cnt: Int32): Int32; pure;
begin
  if Cnt > 0 then
    Result := Val + Calc(Val, Cnt - 1)
  else
    Result := 0;     
end;

var G: Int32;

procedure Test;
begin
  G := Calc(3, 5);
end;

initialization
  Test();

finalization
  Assert(G = 15);
end.