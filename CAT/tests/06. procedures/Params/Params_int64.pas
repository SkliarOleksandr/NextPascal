unit Params_int64;

interface

implementation

var G: int64;

procedure P(i: int64);
begin
  G := i; 
end;

procedure Test;
begin
  p(64);
end;
 
initialization
  Test();

finalization
  Assert(G = 64);

end.