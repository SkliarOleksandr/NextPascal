unit proc_ccreg_int32;

interface

implementation

procedure ExtProc(a: Int32); external 'CAT' name 'proc_ccreg_int32'; 

procedure Test;
begin
  ExtProc(32);
end;

initialization
  Test();

finalization

end.