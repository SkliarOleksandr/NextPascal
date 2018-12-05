unit func_ccreg_ret_TSmallSArray1;

interface

implementation

type
  TSmallSArray1 = packed array [0..0] of Uint8;
  
function F(a: TSmallSArray1): TSmallSArray1; external 'CAT' name 'func_ccreg_ret_TSmallSArray1'; 

procedure Test;
var
  a, r: TSmallSArray1;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.