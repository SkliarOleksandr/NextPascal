unit set_1;

interface
implementation

type
  TEnum = (v1 = 1, v2, v3, v4, v5);
  TSet = set of TEnum;

var
  S1, S2: TSet;  

procedure Test;
begin
  S1 := [v2, v3];  
  S2 := S1;
end;

initialization
  Test();

finalization
  Assert(S2 = S1);

end.