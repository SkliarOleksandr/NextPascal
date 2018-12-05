unit enum_dec_1;

interface

implementation

type
  TEnum = (v1, v2, v3, v4, v5);

var
  G: TEnum;  

procedure Test;
begin
  G := v4;
  dec(G);
end;

initialization
  Test();

finalization
  Assert(G = v3);

end.