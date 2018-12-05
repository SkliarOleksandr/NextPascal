unit deref_6;

interface

implementation

var G: Int32;

procedure Test;
var
  P: ^Int32;
begin
  P := @G;
  P^ := 5;
end;

initialization
  Test();

finalization
  Assert(G = 5);
end.