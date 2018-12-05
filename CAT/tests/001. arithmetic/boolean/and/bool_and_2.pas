unit bool_and_2;

interface

implementation

var
  d1, d2, b4: Boolean = False; 
  b1, b2, b3: Boolean = True; 

procedure Test;
begin
  d1 := b1 and b2 and b3;
  
  d2 := d2 and b1 and b2 and b3 and b4;
end;

initialization
  Test();

finalization
  Assert(d1);
  Assert(not d2);
  
end.