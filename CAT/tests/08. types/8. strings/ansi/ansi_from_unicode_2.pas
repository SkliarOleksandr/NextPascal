unit ansi_from_unicode_2;

interface

implementation

var 
  SA: AnsiString;
  SU: string;
  
procedure Test;
begin
  SU := 'hello_ansi';
  SA := SU;
end;

initialization
  Test();

finalization
  Assert(SA = 'hello_ansi');
end.