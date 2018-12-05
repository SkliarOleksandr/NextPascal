unit func_ccreg_ret_TSmallSArray8;

interface

implementation

type
  TSmallSArray8 = packed array [0..7] of Uint8;
  
function F(a: TSmallSArray8): TSmallSArray8; external 'CAT' name 'func_ccreg_ret_TSmallSArray8'; 

procedure Test;
var
  a, r: TSmallSArray8;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.