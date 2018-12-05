unit array_dynamic_1;
interface
implementation

var
  A: array of int32;  
  L, G1, G2: Int32;
  
procedure Test;
begin
  SetLength(A, 2);
  
  A[0] := 33;
  A[1] := 44;
  
  G1 := A[0];
  G2 := A[1];
      
  L := Length(A);
end;

initialization
  Test();

finalization
  Assert(L = 2);
  Assert(G1 = 33);
  Assert(G2 = 44);

end.