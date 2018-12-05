unit copy_str_2;

interface

implementation

var S1, S2: string;

procedure Test;
var
  i: Int32;
begin
  S1 := 'ABCD';
  i := 1;
  S2 := Copy(S1, i, 2);
end;

initialization
  Test();

finalization
  Assert(S2 = 'BC');

end.