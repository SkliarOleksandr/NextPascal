unit proc_ccreg_int32_5;

interface

implementation

procedure ExtProc(a, b, c, d, e: Int32); external 'CAT' name 'proc_ccreg_int32_5'; 

procedure Test;
begin
  ExtProc(100, 200, 300, 400, 500);
end;

initialization
  Test();

finalization

end.