unit class_cast_1;

interface
uses system;
implementation
type
  TC1 = class;
  TC2 = class(TC1);

var a: TC1;
    b: TC2;
    g: int32;  

procedure Test;
begin
  b := TC2.Create();
  a := b;  
  if a = b then
    g := 1
  else
    g := 0;   
end;

initialization
  Test();

finalization
  Assert(g = 1);
end.