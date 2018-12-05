unit constexpr_for_1;

interface

implementation

function SumN(Val, Cnt: Int32): Int32; pure;
begin
  Result := 0;
  for var i := 1 to Cnt do
    Inc(Result, Val);
end;

var
  G0, G5: Int32;

procedure Test;
begin
  G0 := SumN(6, 0); 
  G5 := SumN(6, 5);
end;

initialization
  Test();

finalization
  Assert(G0 = 0);
  Assert(G5 = 30);
  
end.