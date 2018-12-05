unit func_ccreg_ret_TSmallSArray5;

interface

implementation

type
  TSmallSArray5 = packed array [0..4] of Uint8;
  
function F(a: TSmallSArray5): TSmallSArray5; external 'CAT' name 'func_ccreg_ret_TSmallSArray5'; 

procedure Test;
var
  a, r: TSmallSArray5;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.