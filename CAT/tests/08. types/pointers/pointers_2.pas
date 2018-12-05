unit pointers_2;

interface

implementation

var
  P: Pointer;

procedure Test;
begin
  P := @P;
end;

initialization
  Test();

finalization
  Assert(P <> nil);

end.