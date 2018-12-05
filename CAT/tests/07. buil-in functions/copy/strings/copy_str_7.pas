unit copy_str_7;

interface

implementation

var S1, S2: string;

procedure Test;
var
  i, c: Int32;
begin
  i := 7;
  c := 2;
  S2 := copy(S1, i, c - i); 
end;

initialization
  S1 := 'ABCD';
  Test();

finalization
  Assert(S2 = '');
end.