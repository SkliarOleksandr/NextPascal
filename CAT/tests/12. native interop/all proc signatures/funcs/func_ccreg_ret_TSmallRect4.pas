unit func_ccreg_ret_TSmallRect4;

interface

implementation

type
  TSmallRect4 = packed record x, y, z, w: UInt8; end;

function F(a: TSmallRect4): TSmallRect4; external 'CAT' name 'func_ccreg_ret_TSmallRect4'; 

var
  a, r: TSmallRect4;
  
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