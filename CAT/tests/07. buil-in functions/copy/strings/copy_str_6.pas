unit copy_str_6;

interface

implementation

var S1, S2: string;

procedure Test;
var
  i, c: Int32;
begin
  S1 := 'ABCD';
  i := 3;
  c := 3;
  S2 := copy(S1, i, c); 
end;

initialization
  Test();

finalization
  Assert(S2 = 'D');
end.