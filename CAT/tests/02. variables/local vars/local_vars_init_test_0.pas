unit local_vars_init_test_0;

interface

implementation

var G: Int32;

procedure Test;
var
  i, c: Int32;
begin
  i := 0;
  if i > 0 then 
    c := 0 
  else
    c := 1;      
  G := i + c; 
end;

initialization
  Test();

finalization
  Assert(G = 1);
end.