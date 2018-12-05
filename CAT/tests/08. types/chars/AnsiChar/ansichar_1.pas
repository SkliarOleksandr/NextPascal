unit ansichar_1;

interface

implementation

var C: AnsiChar;

procedure Test;
begin
  C := 'A';
end;

initialization
  Test();

finalization
  Assert(C = AnsiChar('A'));
end.