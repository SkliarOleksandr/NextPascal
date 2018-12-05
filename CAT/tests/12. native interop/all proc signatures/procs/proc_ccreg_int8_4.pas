unit proc_ccreg_int8_4;

interface

implementation

procedure ExtProc(a, b, c, d: Int8); external 'CAT' name 'proc_ccreg_int8_4'; 

procedure Test;
begin
  ExtProc(1, 2, 3, 4);
end;

initialization
  Test();

finalization

end.