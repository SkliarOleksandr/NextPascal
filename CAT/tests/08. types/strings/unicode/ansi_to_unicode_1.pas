unit ansi_to_unicode_1;

interface

implementation

var
  SA: AnsiString;
  SU: String;
  

procedure Test;
begin
  SA := 'ansi';
  SU := SA;  
end;

initialization
  Test();

finalization
  Assert(SU = 'ansi');
end.