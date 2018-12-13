unit bool_or_not_3;

interface

implementation

interface

implementation

function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

procedure Test1;
begin
  var r := not GetBool(false) or GetBool(false);
  Assert(r);  
end;

procedure Test2;
begin
  var r := not GetBool(True) or GetBool(false);
  Assert(not r);  
end;
              
procedure Test3;
begin 
  var r := not GetBool(False) or GetBool(True);
  Assert(r);  
end; 

procedure Test4;
begin
  var r := not GetBool(True) or GetBool(True);
  Assert(r);  
end;

initialization
  Test1();
  Test2();
  Test3();
  Test4();      

finalization

end.