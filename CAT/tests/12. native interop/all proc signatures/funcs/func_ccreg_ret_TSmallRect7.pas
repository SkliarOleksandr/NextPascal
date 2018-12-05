unit func_ccreg_ret_TSmallRect7;

interface

implementation

type
  TSmallRect7 = packed record x, y, z, w, q, a, b: UInt8; end;

function F(a: TSmallRect7): TSmallRect7; external 'CAT' name 'func_ccreg_ret_TSmallRect7'; 

var
  a, r: TSmallRect7;
  
procedure Test;
begin
  a.x := 12;
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.