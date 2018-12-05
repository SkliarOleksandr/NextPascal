unit bool_arith_mixed_2;

interface

implementation
    
var G: boolean;    
    
function GetBool1(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;

function GetBool2(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;     

function GetBool3(Cond: Boolean): Boolean;
begin
  Result := Cond;
end;      
    
procedure Test;
begin
  G := GetBool1(True) or GetBool2(False) and GetBool3(True);
end;

initialization
  Test();

finalization

end.