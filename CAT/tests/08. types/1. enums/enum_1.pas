unit enum_1;

interface
implementation

type
  TEnum = (v1, v2, v3, v4, v5);

var
  G1, G2: TEnum;  

procedure Test;
begin
  G1 := v2;  
  G2 := G1;
end;

initialization
  Test();

finalization
  Assert(G1 = V2);  
  Assert(G2 = V2);  

end.