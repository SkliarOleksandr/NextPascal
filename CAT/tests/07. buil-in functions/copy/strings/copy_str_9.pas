unit copy_str_9;

interface

implementation

var G: string;

function Test: string;
var
  B, S: string;
  i, c: Int32;
begin
  i := 4;
  c := 1;
  B := '1234567890';
  S := '-';
  Result := S + Copy(B, i - c - 1, i);
end;

initialization
  G := Test();

finalization
  Assert(G = '-3456');
end.