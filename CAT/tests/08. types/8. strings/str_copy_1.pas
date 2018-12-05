unit str_copy_1;

interface

implementation

var S: string;

procedure Test;
begin
  S := Copy('abcde', 1, 3) + Copy('abcde', 1, 3);
end;

initialization
  Test();

finalization
  Assert(S = 'bcdbcd');
end.