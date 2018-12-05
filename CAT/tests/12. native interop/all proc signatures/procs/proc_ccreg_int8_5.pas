unit proc_ccreg_int8_5;

interface

implementation

procedure ExtProc(a, b, c, d, e: Int8); external 'CAT' name 'proc_ccreg_int8_5'; 

procedure Test;
begin
  ExtProc(1, 2, 3, 4, 5);
end;

initialization
  Test();

finalization

end.