unit deref_ustr;

interface

implementation

var S1, S2: string;
    P: ^string;

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