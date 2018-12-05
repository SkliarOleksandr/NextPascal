unit ptr_arith_pchar_2;

interface

implementation

var S: string = 'ASDF';

procedure Test;
var
  P: ^Char;
begin
  P := @S[Low(s)]; 
  for var i := Low(S) to High(S) do
  begin
    P^ := 'a';
    Inc(P);
  end;      
end;

initialization
  Test();

finalization
  Assert(S = 'aaaa');
end.