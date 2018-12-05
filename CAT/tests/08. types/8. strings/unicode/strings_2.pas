unit strings_2;

interface

implementation

var S1, S2, S3: string;

procedure Test;
begin
  S1 := 'AAA';
  S2 := 'BBB';
  S3 := S1 + S2;  
end;

initialization
  Test();

finalization
  Assert(S3 = 'AAABBB');
end.