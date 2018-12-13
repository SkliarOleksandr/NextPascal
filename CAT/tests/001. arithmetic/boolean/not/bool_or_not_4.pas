unit bool_or_not_4;

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
  var r := GetBool(false) or not GetBool(false);
  Assert(r);  
end;

procedure Test2;
begin
  var r := GetBool(True) or not GetBool(false);
  Assert(r);  
end;
              
procedure Test3;
begin 
  var r := GetBool(False) or not GetBool(True);
  Assert(not r);  
end; 

procedure Test4;
begin
  var r := GetBool(True) or not GetBool(True);
  Assert(r);  
end;

initialization
  Test1();
  Test2();
  Test3();
  Test4();      

finalization

end.