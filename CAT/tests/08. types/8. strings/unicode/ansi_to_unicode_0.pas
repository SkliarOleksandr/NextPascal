unit ansi_to_unicode_0;

interface

implementation

const 
  SA: AnsiString = 'ansi';
  
var
  SU: string;   

procedure Test;
begin
  SU := SA;
end;

initialization
  Test();

finalization
  Assert(SU = 'ansi');
end.