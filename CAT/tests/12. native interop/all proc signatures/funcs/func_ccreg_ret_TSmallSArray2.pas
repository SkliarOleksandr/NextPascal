unit func_ccreg_ret_TSmallSArray2;

interface

implementation

type
  TSmallSArray2 = packed array [0..1] of Uint8;
  
function F(a: TSmallSArray2): TSmallSArray2; external 'CAT' name 'func_ccreg_ret_TSmallSArray2'; 

procedure Test;
var
  a, r: TSmallSArray2;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.