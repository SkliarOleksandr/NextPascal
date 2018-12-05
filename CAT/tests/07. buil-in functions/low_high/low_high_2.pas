unit low_high_2;

interface

implementation

type
  TEnum = (a = 1, b, c, d, e, f);

var
  L, H: TEnum;

procedure Test;
begin
  L := Low(TEnum);
  H := High(TEnum);  
end;

initialization
  Test();

finalization
  Assert(L = a);
  Assert(H = f);  
end.