unit deref_9;

interface

implementation

function SUM(a, b: Int32): Int32;
begin
  Result := a + b;
end;

var G1, G2, G3: Int32;
    P1, P2, P3: ^Int32;

procedure Test;
begin
  P3^ := SUM(P1^, P2^); 
end;

initialization
  G1 := 1;
  G2 := 2;
  P1 := @G1;
  P2 := @G2;
  P3 := @G3;    
  Test();

finalization
  Assert(G3 = 3);
end.