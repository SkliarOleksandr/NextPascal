unit while_inf_2;

interface

implementation

var X: Int32;

procedure Test;
begin
  while true do
  begin
    inc(X);
    if X < 5 then continue;
    break;
  end;
end;

initialization
  Test();

finalization
  Assert(X = 5);
end.