unit local_vars_init_test2;

interface

implementation

procedure Test;
var
  a, b: Int32;
begin
  a := 1;
  case a of 
    1: b := 1;
    2: b := 2;
  else
    b := 3;
  end;  
  Assert(b = a);  
end;

initialization
  Test();

finalization

end.