unit func_ccreg_ustr;

interface

implementation

function ExtProc: String; external 'CAT' name 'func_ccreg_ustr'; 

procedure Test;
var
  R: string;
begin
  R := ExtProc();
  Assert(R = 'UnicodeString');  
end;

initialization
  Test();

finalization

end.