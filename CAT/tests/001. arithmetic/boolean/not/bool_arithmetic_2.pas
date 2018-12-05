unit bool_arithmetic_2;

interface

implementation

function GetBool(B: Boolean): Boolean;
begin
  Result := B;
end;

var R1, R2: Int32;

procedure Test;
begin
  if not GetBool(False) then
    R1 := 11
  else
    R1 := 22;
    
  if not GetBool(True) then
    R2 := 11
  else
    R2 := 22;       
end;

initialization
  Test();

finalization
  Assert(R1 = 11);
  Assert(R2 = 22);

end.