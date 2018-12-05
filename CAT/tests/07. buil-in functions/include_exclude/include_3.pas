unit include_3;

interface

implementation

var S1: set of (a1, a2, a3);

procedure Test;
begin
  include(S1, [a1, a3]);
end;

initialization
  Test();

finalization
  Assert(S1 = [a1, a3]);

end.