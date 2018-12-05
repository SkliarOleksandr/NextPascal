unit ift_binand_1;

interface

implementation

var
  A: Boolean = False; 
  B, C: Boolean = True;

procedure Test;
begin
  if B and C then 
    A := True
  else
    A := False;  
end;

initialization
  Test();

finalization
  Assert(A);
end.