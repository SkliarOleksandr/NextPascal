unit strings_1;

interface

implementation

var
  S1, S2, S3: string;

procedure Test;
begin
  S1 := 'abcd';
  S2 := S1;
  S3 := 'X';
end;

initialization
  Test();

finalization
  Assert(S1 = S2);
  Assert(S3 = 'X');
end.