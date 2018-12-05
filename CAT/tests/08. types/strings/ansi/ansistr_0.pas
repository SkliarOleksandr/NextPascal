unit ansistr_0;

interface

implementation

var 
  SA: AnsiString;
  SU: string;

procedure Test;
begin
  SA := 'ansi string';
  SU := 'unicode string'; 
end;

initialization
  Test();

finalization

end.