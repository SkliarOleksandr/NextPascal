unit exclude_3;

interface

implementation

var S1: set of (a1, a2, a3);

procedure Test;
begin
  S1 := [a1, a2, a3];
  exclude(S1, [a1, a3]);
end;

initialization
  Test();

finalization
  Assert(S1 = [a2]);

end.