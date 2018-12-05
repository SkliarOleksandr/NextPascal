unit func_ccreg_ret_TSmallRect8;

interface

implementation

type
  TSmallRect8 = packed record x, y, z, w, q, a, b, c: UInt8; end;

function F(a: TSmallRect8): TSmallRect8; external 'CAT' name 'func_ccreg_ret_TSmallRect8'; 

var
  a, r: TSmallRect8;
  
procedure Test;
begin
  r.x := 11;
  a.x := 12;
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.