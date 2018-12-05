unit ansi_from_unicode_1;

interface

implementation

var 
  SA: AnsiString;
  
procedure Test;
begin
  SA := 'hello_ansi';
end;

initialization
  Test();

finalization
  Assert(SA = 'hello_ansi');
end.