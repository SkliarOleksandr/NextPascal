unit proc_ccreg_int8_3;

interface

implementation

procedure ExtProc(a, b, c: Int8); external 'CAT' name 'proc_ccreg_int8_3'; 

procedure Test;
begin
  ExtProc(1, 2, 3);
end;

initialization
  Test();

finalization

end.