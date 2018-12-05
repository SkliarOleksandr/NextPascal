unit func_ccreg_set24;

interface

implementation

type
  TEnum24 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23);
  TSet24 = packed set of TEnum24;

function F(a: TSet24): TSet24; external 'CAT' name 'func_ccreg_set24'; 

var
  a, r: TSet24;

procedure Test;
begin
  a := [_a01];
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.