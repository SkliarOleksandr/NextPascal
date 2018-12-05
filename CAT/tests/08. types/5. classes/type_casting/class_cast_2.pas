unit class_cast_2;

interface
uses system;
implementation
type
  TC1 = class;
  TC2 = class;

var a: TC1;
    b: TC2;
    g: int32;  

procedure Test;
begin
  a := nil;
  b := nil;
  if TObject(a) = TObject(b) then
    g := 1
  else
    g := 0;   
end;

initialization
  Test();

finalization

end.