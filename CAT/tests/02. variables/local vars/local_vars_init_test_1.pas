unit local_vars_init_test1;

interface

implementation

var
  G: Int32;

procedure Test;
var
  a, b: int32;
begin 
  a := 1;
  if a = 1 then
  begin   
    b := 11;
  end else begin      
    b := 12;  
  end;          
  G := a + b;    
end;

initialization
  Test();

finalization

end.