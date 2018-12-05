unit arrays;

interface

type
  TArray = array [3] of Int32;
  
var
  A: TArray;
  G: Int32;  
  
implementation

procedure Test;
var
  i: Int32;  
begin
  A[0] := 1;
  A[1] := 2;
  A[2] := 3;
  for i := 0 to Length(A) - 1 do
    G := G + A[i];
end;

initialization
  G := 0;
  Test();

finalization
  Assert(G = 6);

end.