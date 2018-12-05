unit proc_ccreg_f64;

interface

implementation

procedure ExtProc(a: float64); external 'CAT' name 'proc_ccreg_f64'; 

procedure Test;
begin
  ExtProc(3.6);
end;

initialization
  Test();

finalization

end.