unit strings_4;

interface

implementation

var 
  S1, S2: string;
  G: Int32;

procedure Test;
begin
  SetLength(S1, 3);
  S1[0] := 'X';
  S1[1] := 'Y';
  S1[2] := 'Z';
  
  SetLength(S2, 3);
  S2[0] := 'X';
  S2[1] := 'Y';
  S2[2] := 'Z';  

  if S1 = S2 then
    G := 1;        
end;

initialization
  Test();

finalization
  Assert(G = 1);

end.