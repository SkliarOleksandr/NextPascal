unit func_ccreg_ret_TSmallRect5;

interface

implementation

type
  TSmallRect5 = packed record x, y, z, w, q: UInt8; end;

function F(a: TSmallRect5): TSmallRect5; external 'CAT' name 'func_ccreg_ret_TSmallRect5'; 

var
  a, r: TSmallRect5;
  
procedure Test;
begin
  a.x := 10;
  a.y := 11;
  a.z := 12;
  a.w := 13;
  a.q := 14;        
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.