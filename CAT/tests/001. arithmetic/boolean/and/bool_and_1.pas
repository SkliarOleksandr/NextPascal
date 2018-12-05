unit bool_and_1;

interface

implementation

var
  b1: Boolean = False; 
  b2, b3: Boolean = True;

procedure Test;
begin
  b1 := b2 and b3;
end;

initialization
  Test();

finalization
  Assert(b1);
  Assert(b2);
  Assert(b3);    
end.