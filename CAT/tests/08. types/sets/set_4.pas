unit set_4;

interface

implementation

type
 TSet3 = set of 0..23;
 
var
  S1, S2: TSet3;
  
procedure Test;
begin
  S1 := [3];
  S2 := S1;
end;

initialization
  Test();

finalization
  Assert(S1 = S2);  
end.