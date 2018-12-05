unit assigned_3;

interface

implementation

function GetPtr: Pointer;
begin
  Result := nil;
end;

procedure Test;
begin           
  if not Assigned(GetPtr()) then
    Assert(not Assigned(GetPtr()));
end;

initialization
  Test();

finalization

end.