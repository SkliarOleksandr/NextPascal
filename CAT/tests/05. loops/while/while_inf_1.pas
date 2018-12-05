unit while_inf_1;

interface

implementation

var
  x: integer;

procedure Test;
begin
  x := 0;
  while true do
  begin
    if x > 10 then
      break;
    inc(x);     
  end;
end;

initialization
  Test();

finalization
  Assert(X = 11);
end.