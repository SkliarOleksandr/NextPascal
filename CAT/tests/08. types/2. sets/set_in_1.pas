unit set_in_1;

interface

implementation

type
  TEnum = (i1, i2, i3, i4);

var
  S: set of TEnum;
  G1, G2: Boolean;

procedure Test;
begin
  S := [i1, i3];
  G1 := i3 in S;
  G2 := i2 in S;  
end;

initialization
  Test();

finalization
  Assert(G1 = true);
  Assert(G2 = false);
end.