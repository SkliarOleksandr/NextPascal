unit proc_ccreg_set8;

interface             

implementation

type
  TEnum8 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07);
  TSet8 = packed set of TEnum8;	

procedure ExtProc(a: TSet8); external 'CAT' name 'proc_ccreg_set8'; 

procedure Test;
var
  s: TSet8;
begin
  s := [_a01]; 
  ExtProc(s);
end;

initialization
  Test();

finalization

end.