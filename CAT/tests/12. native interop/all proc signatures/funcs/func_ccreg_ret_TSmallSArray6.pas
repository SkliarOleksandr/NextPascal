unit func_ccreg_ret_TSmallSArray6;

interface

implementation

type
  TSmallSArray6 = packed array [0..5] of Uint8;
  
function F(a: TSmallSArray6): TSmallSArray6; external 'CAT' name 'func_ccreg_ret_TSmallSArray6'; 

procedure Test;
var
  a, r: TSmallSArray6;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.