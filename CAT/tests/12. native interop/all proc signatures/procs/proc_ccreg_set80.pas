#skip 'not supported yet'
unit proc_ccreg_set80;

interface             

implementation

type  
  TSet80 = packed set of 0..79;	

procedure ExtProc(a: TSet80); external 'CAT' name 'proc_ccreg_set80'; 

procedure Test;
var
  s: TSet80;
begin
  s := [16];
  ExtProc(s);
end;

initialization
  Test();

finalization

end.