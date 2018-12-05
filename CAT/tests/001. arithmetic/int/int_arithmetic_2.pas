unit int_arithmetic_2;

interface

implementation

var G: Int32;

procedure Test;
begin
  var a := 11;
  var b := 22;
  G := a + -b;
end;

initialization
  Test();

finalization

end.