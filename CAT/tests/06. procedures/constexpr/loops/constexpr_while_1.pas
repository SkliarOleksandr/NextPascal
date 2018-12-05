unit constexpr_while_1;

interface

implementation

function SumN(Val, Cnt: Int32): Int32; pure;
begin
  Result := 0;
  while Cnt > 0 do
  begin
    Inc(Result, Val);
    Dec(Cnt);
  end; 
end;

var
  G0, G5: Int32;

procedure Test;
begin
  G0 := SumN(5, 0); 
  G5 := SumN(5, 5);
end;

initialization
  Test();

finalization
  Assert(G0 = 0);
  Assert(G5 = 25);
end.