unit copy_str_1;

interface

implementation

var S1, S2: string;

procedure Test;
begin
  S1 := 'ABCD';
  S2 := Copy(S1, 2, 2);
end;

initialization
  Test();

finalization
  Assert(S2 = 'CD');
end.