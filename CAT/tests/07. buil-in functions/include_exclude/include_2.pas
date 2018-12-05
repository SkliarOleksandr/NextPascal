unit include_2;

interface

implementation

var S1: set of (a1, a2, a3);

procedure Test;
begin
  include(S1, a2);
end;

initialization
  Test();

finalization
  Assert(S1 = [a2]);

end.