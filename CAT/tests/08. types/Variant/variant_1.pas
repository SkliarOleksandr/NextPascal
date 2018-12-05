unit Variant_1;

interface
implementation

var
  i32: Int32;
  f64: Float64;
  b: Boolean;
  
procedure Test;
var
  V: Variant;
begin
  V := 12;
  i32 := V;
  V := 3.2;
  f64 := V;
  V := True;
  b := V;
end;

initialization
  Test();

finalization
  Assert(i32 = 12);
  Assert(f64 = 3.2);
  Assert(b = True);

end.