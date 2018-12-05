unit func_ccreg_ret_TSmallSArray3;

interface

implementation

type
  TSmallSArray3 = packed array [0..2] of Uint8;
  
function F(a: TSmallSArray3): TSmallSArray3; external 'CAT' name 'func_ccreg_ret_TSmallSArray3'; 

procedure Test;
var
  a, r: TSmallSArray3;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.