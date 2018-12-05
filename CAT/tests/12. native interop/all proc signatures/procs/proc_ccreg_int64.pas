unit proc_ccreg_int64;

interface

implementation

procedure ExtProc(a: Int64); external 'CAT' name 'proc_ccreg_int64'; 

procedure Test;
begin
  ExtProc(64);
end;

initialization
  Test();

finalization

end.