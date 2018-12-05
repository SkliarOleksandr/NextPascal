unit datetime_1;

interface

implementation

var
  dt: DateTime;

procedure Test;
begin
  dt := Now();
end;

initialization
  Test();

finalization
  Assert(dt <> 0);
end.