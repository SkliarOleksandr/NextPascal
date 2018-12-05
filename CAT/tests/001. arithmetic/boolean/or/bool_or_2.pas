unit bool_or_2;

interface

implementation

var
  d1, d2, b4: Boolean = False; 
  b1, b2, b3: Boolean = True; 

procedure Test;
begin
  d1 := b1 or b2 or b3 or b4;
  d2 := d2 or b4 or b3;  
end;

initialization
  Test();

finalization
  Assert(d1);
  Assert(d2);

end.