unit proc_ccreg_int64_5;

interface

implementation

procedure ExtProc(a, b, c, d, e: Int64); external 'CAT' name 'proc_ccreg_int64_5'; 

procedure Test;
begin
  ExtProc(1000, 2000, 3000, 4000, 5000);
end;

initialization
  Test();

finalization

end.