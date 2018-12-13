unit bool_and_not_2;

interface

implementation

procedure Test1;
var 
  r, a, b: Boolean;
begin
  a := false;
  b := false;  
  r := a and not b;
  Assert(not r);  
end;

procedure Test2;
var 
  r, a, b: Boolean;
begin
  a := true;
  b := false;  
  r := a and not b;
  Assert(r);  
end;

procedure Test3;
var 
  r, a, b: Boolean;
begin
  a := false;
  b := true;  
  r := a or not b; 
  Assert(not r);  
end;

procedure Test4;
var 
  r, a, b: Boolean;
begin
  a := true;
  b := true;  
  r := a and not b;
  Assert(not r);  
end;        

initialization
  Test1();
  Test2();
  Test3();
  Test4();      

finalization

end.