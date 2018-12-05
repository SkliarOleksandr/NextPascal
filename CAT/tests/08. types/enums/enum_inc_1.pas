unit enum_inc_1;

interface

implementation

type
  TEnum = (v1, v2, v3, v4, v5);

var
  G: TEnum;  

procedure Test;
begin
  G := v1;
  inc(G);
end;

initialization
  Test();

finalization
  Assert(G = v2);
end.