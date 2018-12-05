unit local_vars_init_test_3;

interface

implementation

var
  G: Int32;

procedure Test;
var
  a, b, c: int32;
begin 
  a := 1;
  if a = 1 then
  begin   
    b := a + 1;
    if b = a then
      c := 10
    else
      c := 11;         
  end else begin      
    b := 12;
    if b = 12 then
      c := 100
    else
      c := 110;        
  end;          
  G := a + b + c;   
end;

initialization
  Test();

finalization
  Assert(G = 14);

end.