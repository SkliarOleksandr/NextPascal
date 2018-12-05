unit copy_str_4;

interface

implementation

var S1, S2: string;

procedure Test;
begin
  S1 := 'XYZ';
  S2 := Copy(S1);
end;

initialization
  Test();

finalization
  Assert(S2 = 'XYZ');

end.