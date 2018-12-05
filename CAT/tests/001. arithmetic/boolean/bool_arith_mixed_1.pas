unit bool_arith_mixed_1;

interface

implementation

function GetBool(Cond: Boolean): Boolean;
begin
  Result := Cond;
end; 

var
  R: Boolean;

procedure Test; 
begin
  R := GetBool(True) and not GetBool(False);
end;

initialization
  Test();

finalization
  Assert(R);
end.