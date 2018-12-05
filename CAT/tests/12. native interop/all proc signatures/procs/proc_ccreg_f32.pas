unit proc_ccreg_f32;

interface

implementation

procedure ExtProc(a: float32); external 'CAT' name 'proc_ccreg_f32'; 

procedure Test;
begin
  ExtProc(1.5);
end;

initialization
  Test();

finalization

end.