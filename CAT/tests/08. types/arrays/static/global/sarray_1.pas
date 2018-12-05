unit arrays;

interface

type
  TArray1 = array [10] of int32;
  TArray2 = array [0..9] of int32;  

var
  G1, G2: Int32;  
  
implementation

procedure Test;
var
  a1: TArray1;
  a2: TArray2;  
begin
  G1 := Length(a1);
  G2 := Length(a2);  
end;

initialization
  Test();

finalization
  Assert(G1 = 10);
  Assert(G2 = 10);

end.