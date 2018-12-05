unit proc_ccreg_int8_2;

interface

implementation

procedure ExtProc(a, b: Int8); external 'CAT' name 'proc_ccreg_int8_2'; 

procedure Test;
begin
  ExtProc(1, 2);
end;

initialization
  Test();

finalization

end.