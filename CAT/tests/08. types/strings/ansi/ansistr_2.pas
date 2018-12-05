unit ansistr_2;

interface

implementation

var S1, S2: AnsiString;
    G: Int32;
    
procedure Test;
begin
  SetLength(S1, 3);
  S1[0] := 'X';
  S1[1] := 'Y';
  S1[2] := 'Z';    
  S2 := S1;
  if S1 = S2 then
    G := 1;   
end;

initialization
  Test();

finalization
  Assert(G = 1);
end.