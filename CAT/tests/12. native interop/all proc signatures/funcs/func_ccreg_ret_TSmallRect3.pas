unit func_ccreg_ret_TSmallRect3;

interface

implementation

type
  TSmallRect3 = packed record x, y, z: UInt8; end;

function F(a: TSmallRect3): TSmallRect3; external 'CAT' name 'func_ccreg_ret_TSmallRect3'; 

var
  a, r: TSmallRect3;
  
procedure Test;
begin
  a.x := 12;
  a.y := 13;
  a.z := 14;
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.