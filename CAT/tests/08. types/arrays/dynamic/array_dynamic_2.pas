unit array_dynamic_2;
interface
implementation

var
  A32: array of int32;
  A16: array of int16;
  A08: array of int8;  
  L1, L2, L3: Int32;  
  G1, G2: Int32; 
  
procedure Test;
begin
  SetLength(A32, 2);
  SetLength(A16, 3);
  SetLength(A08, 4);
  
  A32[0] := 777;
  A32[1] := 888;
  
  G1 := A32[0];
  G2 := A32[1];
   
  A16[0] := 77;
  A16[1] := 88;
  
  A08[0] := 7;
  A08[1] := 8;  
  
  L1 := Length(A32);
  L2 := Length(A16); 
  L3 := Length(A08);
  
  G1 := A32[0];
  G2 := A32[1];
  Assert(G1 = 777);
  Assert(G2 = 888);
  
  G1 := A16[0];
  G2 := A16[1];
  Assert(G1 = 77);
  Assert(G2 = 88);  
  
  G1 := A08[0];
  G2 := A08[1];
  Assert(G1 = 7);
  Assert(G2 = 8);    
end;

initialization
  Test();

finalization
  Assert(L1 = 2);
  Assert(L2 = 3);
  Assert(L3 = 4);

end.