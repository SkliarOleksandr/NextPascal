unit ift_binand_2;

interface

implementation

var
  A: Boolean = False; 
  B, C, D: Boolean = True;

procedure Test;
begin
  if B and C and D then 
    A := True
  else
    A := False;  
end;

initialization
  Test();

finalization
  Assert(A);

end.