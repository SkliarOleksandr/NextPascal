unit local_vars_init_test_4;

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
    b := a + 1;
  end;          
  G := b;    
end;

initialization
  Test();

finalization

end.