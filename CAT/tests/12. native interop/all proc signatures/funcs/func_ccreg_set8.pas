unit func_ccreg_set8;

interface

implementation

type
  TEnum8 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07);
  TSet8 = set of TEnum8;

function F(a: TSet8): TSet8; external 'CAT' name 'func_ccreg_set8'; 

procedure Test;
var
  a, r: TSet8;
begin
  a := [_a01]; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.