unit constexpt_if_1;

interface

implementation

function Add(a, b: Int32): Int32; pure;
begin
  if a > b then
    Result := a - b
  else
    Result := a + b;  
end;

var G1, G2: Int32;

procedure Test;
begin
  G1 := Add(4, 3);
  G2 := Add(3, 5);
end;  

initialization
  Test();

finalization
  Assert(G1 = 1);
  Assert(G2 = 8);  

end.