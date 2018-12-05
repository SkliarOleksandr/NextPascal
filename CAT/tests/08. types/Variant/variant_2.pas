unit variant_2;

interface

implementation

var
  V: Variant;
  G: NativeInt;
 
procedure Test;
begin
  V := 5;
  G := V;
  Assert(V = 5);
  V := 1.2;
  Assert(V = 1.2);
  V := 'asdf';
  Assert(V = 'asdf');    
end;

initialization
  Test();

finalization

end.