unit func_ccreg_ret_TSmallSArray7;

interface

implementation

type
  TSmallSArray7 = packed array [0..6] of Uint8;
  
function F(a: TSmallSArray7): TSmallSArray7; external 'CAT' name 'func_ccreg_ret_TSmallSArray7'; 

procedure Test;
var
  a, r: TSmallSArray7;
begin
  a[0] := 11; 
  R := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.