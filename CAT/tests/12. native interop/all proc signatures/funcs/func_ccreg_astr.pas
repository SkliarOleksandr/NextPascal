unit func_ccreg_astr;

interface

implementation

function ExtProc: AnsiString; external 'CAT' name 'func_ccreg_astr'; 

var
  R: AnsiString;

procedure Test;
const 
  S: AnsiString = 'AnsiString';
begin
  R := ExtProc();
  //Assert(R = S); //cmp error !!!  
end;

initialization
  Test();

finalization

end.