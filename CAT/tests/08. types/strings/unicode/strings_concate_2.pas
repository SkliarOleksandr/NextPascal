unit strings_concate_2;

interface

implementation

procedure Test;
var
  S: string;
begin
  S := S + 'A';
  S := S + 'BB';
  S := S + 'CCC';      
  Assert(S = 'ABBCCC');
end;

initialization
  Test();

finalization

end.