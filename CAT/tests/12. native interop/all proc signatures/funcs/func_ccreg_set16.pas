unit func_ccreg_set16;

interface

implementation

type
  TEnum16 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15);
  TSet16 = packed set of TEnum16;

function F(a: TSet16): TSet16; external 'CAT' name 'func_ccreg_set16'; 

procedure Test;
var
  a, r: TSet16;
begin
  a := [_a01]; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.