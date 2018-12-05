unit bool_xor_if_1;

interface

implementation

var A: Boolean = False; 
    B, C: Boolean = True; 

procedure Test;
begin
  if B xor C then
    A := True
  else
    A := False;  
end;

initialization
  Test();

finalization
  Assert(not A);
end.