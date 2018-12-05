unit set_in_2;

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
  G1 := (i3 in S) and (i2 in S);
  G2 := (i4 in S) or (i1 in S);  
end;

initialization
  Test();

finalization
  Assert(G1 = False);
  Assert(G2 = True);
end.