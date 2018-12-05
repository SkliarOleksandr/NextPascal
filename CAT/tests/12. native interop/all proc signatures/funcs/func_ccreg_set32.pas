unit func_ccreg_set32;

interface

implementation

type
  TEnum32 = (_a00,_a01,_a02,_a03,_a04,_a05,_a06,_a07,_a08,_a09,_a10,_a11,_a12,_a13,_a14,_a15,_a16,_a17,_a18,_a19,_a20,_a21,_a22,_a23,_a24,_a25,_a26,_a27,_a28,_a29,_a30,_a31);
  TSet32 = packed set of TEnum32;

function F(a: TSet32): TSet32; external 'CAT' name 'func_ccreg_set32'; 

procedure Test;
var
  a, r: TSet32;
begin
  a := [_a01]; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.