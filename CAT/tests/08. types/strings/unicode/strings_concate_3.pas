unit strings_concate_3;

interface

implementation

function AddString(const Str1, Str2: string): string;
begin  
  Result := Str1 + ',' + Str2;
end;

var S: string;
       
initialization
  S := AddString(S, 'X');
  S := AddString(S, 'YY');

finalization
  Assert(S = ',X,YY');
end.