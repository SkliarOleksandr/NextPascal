unit consts_int_1;

interface

implementation

const  
  C1 = 123;
  C2 = -321;
  
var
  G1, G2: Int32;  

procedure Test;
begin
  G1 := C1;
  G2 := C2;
end;

initialization
  Test();

finalization
  Assert(G1 = C1);
  Assert(G2 = C2);  
end.