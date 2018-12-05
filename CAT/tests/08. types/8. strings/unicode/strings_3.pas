unit strings_3;

interface

implementation

var S: string;

procedure Test;
begin
  SetLength(S, 3);  
  S[0] := 'A';
  S[1] := 'B';
  S[2] := 'C';    
end;

initialization
  Test();

finalization
  Assert(S = 'ABC');
end.