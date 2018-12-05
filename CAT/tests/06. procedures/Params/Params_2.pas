unit Params_2;

interface

implementation

function P1(A00, A01, A02, A03, A04, A05, A06, A07, A08, A09,
            A10, A11, A12, A13, A14, A15, A16, A17, A18, A19,
            A20, A21, A22, A23, A24, A25, A26, A27, A28, A29: Int32): Int32;
begin
  Result := A00 + A01 + A02 + A03 + A04 + A05 + A06 + A07 + A08 + A09 +
            A10 + A11 + A12 + A13 + A14 + A15 + A16 + A17 + A18 + A19 +
            A20 + A21 + A22 + A23 + A24 + A25 + A26 + A27 + A28 + A29;
end;              

function F1(A00, A01, A02, A03, A04, A05, A06, A07, A08, A09, 
            A10, A11, A12, A13, A14, A15, A16, A17, A18, A19,
            A20, A21, A22, A23, A24, A25, A26, A27, A28, A29: Float32): Float32;
begin
  Result := A00 + A01 + A02 + A03 + A04 + A05 + A06 + A07 + A08 + A09 +
            A10 + A11 + A12 + A13 + A14 + A15 + A16 + A17 + A18 + A19 +
            A20 + A21 + A22 + A23 + A24 + A25 + A26 + A27 + A28 + A29;
end;

var  G: Int32;
     F: Float32;

procedure Test;
begin
  G := P1(00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 
          10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 24, 25, 26, 27, 28, 29);
  F := F1(00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 
          10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
          20, 21, 22, 23, 24, 25, 26, 27, 28, 29);                     
end; 

initialization
  Test();

finalization
  Assert(G = 435);
  Assert(F = 435);  
end.