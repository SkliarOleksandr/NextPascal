unit bool_or_3;

interface

implementation

var
  d1, d2: Boolean = False; 
  b1, b2, b3: Boolean = True; 

procedure Test;
begin
  d1 := b1 or b2 or b3;
  d2 := not(b1 or b2 or b3);  
end;

initialization
  Test();

finalization
  Assert(d1);
  Assert(not d2);

end.