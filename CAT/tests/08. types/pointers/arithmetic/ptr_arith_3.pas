unit ptr_arith_3;

interface

implementation

type PByte = ^UInt8;

var G: Int64;
    P: PByte;
    A: array [8] of UInt8;

procedure Test;
begin
  G := $01020304050607;
  P := PByte(@G);
  for var i := 0 to sizeof(G) - 1 do
  begin
    A[i] := P^;
    Inc(P);
  end;    
end;

initialization
  Test();

finalization
  Assert(A[0] = 7);
end.