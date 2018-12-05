unit low_high_3;

interface

implementation

var
  a: array [0..15] of int8;

procedure Test;
begin
  for var i := Low(a) to High(a) do
    a[i] := i;
end;

initialization
  Test();

finalization
  Assert(a[15] = 15);
end.