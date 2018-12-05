unit bool_xor_2;

interface

implementation

var A1, A2, E: Boolean = False; 
    B, C, D: Boolean = True; 

procedure Test;
begin
  A1 := B xor C xor D;
  A2 := B xor C xor E;  
end;

initialization
  Test();

finalization
  Assert(A1);
  Assert(not A2);  

end.