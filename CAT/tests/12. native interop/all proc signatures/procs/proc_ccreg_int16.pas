unit proc_ccreg_int16;

interface

implementation

procedure ExtProc(a: Int16); external 'CAT' name 'proc_ccreg_int16'; 

procedure Test;
begin
  ExtProc(16);
end;

initialization
  Test();

finalization

end.