unit func_ccreg_f64;

interface

implementation

function ExtProc: Float64; external 'CAT' name 'func_ccreg_f64'; 

var
  R: float64;

procedure Test;
begin
  R := ExtProc();
  // Assert(R = 64.64); // round error !!!!  
end;

initialization
  Test();

finalization

end.