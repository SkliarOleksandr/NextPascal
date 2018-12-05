unit func_ccreg_ret_TSmallSArray4;

interface

implementation

type
  TSmallSArray4 = packed array [0..3] of Uint8;
  
function F(a: TSmallSArray4): TSmallSArray4; external 'CAT' name 'func_ccreg_ret_TSmallSArray4'; 

procedure Test;
var
  a, r: TSmallSArray4;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.