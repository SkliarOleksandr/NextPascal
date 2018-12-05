unit func_ccreg_i32;

interface

implementation

function ExtProc: Int32; external 'CAT' name 'func_ccreg_i32'; 

procedure Test;
var
  R: Int32;
begin
  R := ExtProc();
  Assert(R = 32);  
end;

initialization
  Test();

finalization

end.