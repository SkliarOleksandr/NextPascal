unit constexpr_3;

interface

implementation

function AddMul2Neg(a, b, c: Int32): Int32; pure;
begin
  Result := - (a + b) * c;
end;       
     
var G: Int32;     
       
procedure Test;
begin
  G := AddMul2Neg(2, 3, 3);
end;

initialization
  Test();

finalization
  Assert(G = -15);
end.