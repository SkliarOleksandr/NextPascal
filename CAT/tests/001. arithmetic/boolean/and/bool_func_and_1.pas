unit bool_func_and_1;

interface

implementation

function GetTrue: Boolean;
begin
  Result := True;
end;

var A: Boolean;

procedure Test;
begin
  A := GetTrue() and GetTrue() and GetTrue();
end;

initialization
  Test();

finalization
  Assert(A);
end.