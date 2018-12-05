unit bool_or_1;

interface

implementation

var
  b1, b2: Boolean = False;    
  b3: Boolean = True;

procedure Test;
begin
  b1 := b2 or b3;
end;

initialization
  Test();

finalization
  Assert(b1);
  Assert(not b2);
  Assert(b3);  

end.