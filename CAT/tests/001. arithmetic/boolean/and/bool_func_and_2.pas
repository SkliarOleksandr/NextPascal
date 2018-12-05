unit bool_func_and_2;

interface

implementation

function GetTrue: Boolean;
begin
  Result := True;
end;

function GetFalse: Boolean;
begin
  Result := False;
end;

var A: Boolean;

procedure Test;
begin
  A := GetTrue() and GetTrue() and GetTrue() and GetFalse();
end;

initialization
  Test();

finalization
  Assert(not A);

end.