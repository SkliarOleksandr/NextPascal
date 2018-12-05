unit variant_params_1;

interface

implementation

var G: Int32;

procedure P(const V: Variant);
begin
  G := V;
end;

var
  V: Variant;

procedure Test;
begin
  V := 56;
  P(V);
end;

initialization
  Test();

finalization
  Assert(G = 56);
end.