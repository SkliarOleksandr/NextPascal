unit copy_str_3;

interface

implementation

var S1, S2: string;

procedure Test;
var
  i, c: Int32;
begin
  S1 := 'ABCD';
  i := 1;
  c := 3;
  S2 := Copy(S1, i, c);
end;

initialization
  Test();

finalization
  Assert(S2 = 'BCD');

end.