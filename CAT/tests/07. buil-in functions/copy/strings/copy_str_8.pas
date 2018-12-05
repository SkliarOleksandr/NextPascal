unit copy_str_8;

interface

implementation

var S: string;

procedure Test;
var
  B, M: string;
  i: Int32;
begin
  i := 3;
  B := '1234567890';
  M := '-';
  S := M + Copy(B, 1 + i, i);
end;

initialization
  Test();

finalization
  Assert(S = '-567');
end.