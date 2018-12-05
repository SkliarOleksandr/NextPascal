unit copy_str_5;

interface

implementation

var S: string;

procedure Test;
begin
  S := Copy('RRTTYY');
end;

initialization
  Test();

finalization
  Assert(S = 'RRTTYY');

end.