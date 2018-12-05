unit strings_concate_1;

interface

implementation

var
  S: string;

initialization
  S := S + 'A';
  S := S + 'BB';
  S := S + 'CCC';      

finalization
  Assert(S = 'ABBCCC');
end.