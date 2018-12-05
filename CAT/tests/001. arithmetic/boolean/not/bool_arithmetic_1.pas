unit bool_arithmetic_1;

interface

implementation

var
  B1, B2:  Boolean;

procedure Test;
begin
  B1 := False;
  B2 := not B1;  
end;

initialization
  Test();

finalization 
  Assert(not B1);
  Assert(B2);    

end.