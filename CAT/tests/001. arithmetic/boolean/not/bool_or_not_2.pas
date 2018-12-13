unit bool_not_4;

interface

implementation

procedure Test1;
var 
  r, a, b: Boolean;
begin
  a := false;
  b := false;  
  r := a or not b; // true;
  Assert(r);  
end;

procedure Test2;
var 
  r, a, b: Boolean;
begin
  a := true;
  b := false;  
  r := a or not b; // true;
  Assert(r);  
end;
            
  
procedure Test3;
var 
  r, a, b: Boolean;
begin
  a := false;
  b := true;    
  r := a or not b; // false;
  Assert(not r);  
end; 

procedure Test4;
var 
  r, a, b: Boolean;
begin
  a := true;
  b := true;  
  r := a or not b; // true;
  Assert(r);  
end;

initialization
  Test1();
  Test2();
  Test3();
  Test4();      

finalization

end.