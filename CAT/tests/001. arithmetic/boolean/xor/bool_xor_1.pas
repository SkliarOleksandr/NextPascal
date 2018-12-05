unit bool_xor_1;

interface

implementation

var A: Boolean = False; 
    B, C: Boolean = True; 

procedure Test;
begin
  A := B xor C;
end;

initialization
  Test();

finalization
  Assert(not A);
end.