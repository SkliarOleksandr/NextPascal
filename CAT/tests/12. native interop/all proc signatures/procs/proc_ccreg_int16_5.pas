unit proc_ccreg_int16_5;

interface

implementation

procedure ExtProc(a, b, c, d, e: Int16); external 'CAT' name 'proc_ccreg_int16_5'; 

procedure Test;
begin
  ExtProc(10, 20, 30, 40, 50);
end;

initialization
  Test();

finalization

end.