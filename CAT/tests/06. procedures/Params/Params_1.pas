unit Params_1;

interface

implementation

var
  G1, G2: Int32;

procedure P1(A, B: Int32);
begin
  G1 := A;
  G2 := B;
end;

procedure Test;
begin
  P1(222, 333);
end;

initialization
  Test();

finalization
  Assert(G1 = 222);
  Assert(G2 = 333);

end.