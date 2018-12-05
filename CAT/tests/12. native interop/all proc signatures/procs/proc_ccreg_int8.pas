unit proc_ccreg_int8;

interface             

implementation

procedure ExtProc(a: Int8); external 'CAT' name 'proc_ccreg_int8'; 

procedure Test;
begin
  ExtProc(8);
end;

initialization
  Test();

finalization

end.