unit char_to_str_1;

interface

implementation

var S: string;

procedure Test;
begin
  s := 'V';
end;

initialization
  Test();

finalization
  Assert(S = 'V');
end.