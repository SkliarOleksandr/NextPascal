unit func_ccreg_i64;

interface

implementation

function ExtProc: Int64; external 'CAT' name 'func_ccreg_i64'; 

procedure Test;
var
  R: Int64;
begin
  R := ExtProc();
  Assert(R = 6464646464646464);  
end;

initialization
  Test();

finalization

end.