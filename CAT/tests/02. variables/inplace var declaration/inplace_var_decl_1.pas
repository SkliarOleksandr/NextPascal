unit inplace_var_decl_1;

interface

implementation

procedure GetVal(out V: int32);
begin
  V := 3;
end;

var
  G: int32;

procedure Test;
begin
  GetVal(var X);
  g := x;  
end;

initialization
  Test();

finalization
  Assert(G = 3);
end.