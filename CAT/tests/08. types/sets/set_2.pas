unit set_2;

interface

implementation

type
 TSet = set of 0..15;
 
var
  S: TSet;
  
procedure Test;
begin
  S[1] := True;
end;

initialization
  Test();

finalization

end.