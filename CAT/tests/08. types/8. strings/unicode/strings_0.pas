unit strings_0;

interface

implementation

var S: String;

procedure Test;
begin
  S := 'ABCD';
end;

initialization
  Test();

finalization
  Assert(S = 'ABCD');

end.