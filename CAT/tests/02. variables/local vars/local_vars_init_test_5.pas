unit local_vars_init_test_5;

interface

implementation

var
  G: Int32;

procedure Test;
var
  a, b: int32;
begin 
  a := 1;
  for a := 0 to 10 do
  begin
    if a > 5 then
      b := 1
    else 
      b := 2;  
  end; 
  G := b;  
end;

initialization
  Test();

finalization

end.