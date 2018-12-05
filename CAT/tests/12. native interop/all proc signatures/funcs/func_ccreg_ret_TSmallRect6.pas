unit func_ccreg_ret_TSmallRect6;

interface

implementation

type
  TSmallRect6 = packed record x, y, z, w, q, a: UInt8; end;

function F(a: TSmallRect6): TSmallRect6; external 'CAT' name 'func_ccreg_ret_TSmallRect6'; 

var
  a, r: TSmallRect6;
  
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