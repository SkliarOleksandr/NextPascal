unit copy_str_0;

interface

implementation

var S1, S2: string;

procedure Test;
begin
  S1 := 'ABCD';
  S2 := copy(S1, 3, 4); 
end;

initialization
  Test();

finalization
  Assert(S2 = 'D');

end.