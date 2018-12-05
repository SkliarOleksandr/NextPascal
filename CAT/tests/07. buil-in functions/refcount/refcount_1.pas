unit refcount_1;

interface

implementation

const  S = 'const';

var
  G: Int32; 

procedure Test;
begin
  G := refcount(S);
end;

initialization
  Test();

finalization
  Assert(G = -1);
end.