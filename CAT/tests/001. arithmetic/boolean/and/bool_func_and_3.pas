unit bool_func_and_3;

interface

implementation

function GetBool(Value: Boolean): Boolean;
begin
  Result := Value;
end;

var B: Boolean = True;

procedure Test;
begin
  B := GetBool(True) and GetBool(False);
end;

initialization
  Test();

finalization
  Assert(not B); 
end.