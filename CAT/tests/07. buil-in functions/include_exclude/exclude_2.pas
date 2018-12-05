unit exclude_2;

interface

implementation

var S1: set of (a1, a2, a3);

procedure Test;
begin
  s1 := [a1, a2, a3];
  exclude(S1, a2);
end;

initialization
  Test();

finalization
  Assert(s1 = [a1, a3]);

end.