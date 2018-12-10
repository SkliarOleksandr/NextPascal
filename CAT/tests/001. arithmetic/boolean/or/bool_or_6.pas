unit bool_or_6;

interface

implementation

function GetBool(Value: Boolean): Boolean;
begin
  Result := Value;
end;

var B: Boolean = False;

procedure Test;
begin
  B := GetBool(False) or GetBool(False) or GetBool(True);
end;

initialization
  Test();

finalization
  Assert(B);
end.