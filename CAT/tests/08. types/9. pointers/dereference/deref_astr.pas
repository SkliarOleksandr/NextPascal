unit deref_astr;

interface

implementation

var S1, S2: AnsiString;
    P: ^AnsiString;

procedure Test;
begin
  S1 := 'asdf';
  P := @S1;
  S2 := P^;
end;

initialization
  Test();

finalization

end.