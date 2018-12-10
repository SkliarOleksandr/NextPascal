unit bool_or_4;

interface

implementation

function GetTrue: Boolean;
begin
  Result := True;
end;

var B: Boolean = False;

procedure Test;
begin
  B := GetTrue() or GetTrue();
end;

initialization
  Test();

finalization
  Assert(B);
end.