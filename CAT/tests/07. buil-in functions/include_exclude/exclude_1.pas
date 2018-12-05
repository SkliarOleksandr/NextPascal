unit exclude_1;

interface

implementation

var S1, S2: set of (a1, a2, a3);

procedure Test;
begin
  S1 := [a1, a2, a3];
  S2 := [a1, a2];
  exclude(S1, S2);
end;

initialization
  Test();

finalization
  Assert(S1 = [a3]);

end.