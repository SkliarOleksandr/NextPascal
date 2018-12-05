unit deref_8;

interface

implementation

var G1, G2: Int32;
    P1, P2: ^Int32;

procedure Test;
begin
  P1 := @G1;
  P2 := @G2;
  P1^ := 5;  
  P2^ := P1^;
end;

initialization
  Test();

finalization
  Assert(G1 = 5);
  Assert(G2 = 5);
end.