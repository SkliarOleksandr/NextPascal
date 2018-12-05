unit func_ccreg_f32;

interface

implementation

function ExtProc: float32; external 'CAT' name 'func_ccreg_f32'; 



procedure Test;
var
  R: float32;
begin
  R := ExtProc();
  Assert(R = 32.32);  
end;

initialization
  Test();

finalization

end.