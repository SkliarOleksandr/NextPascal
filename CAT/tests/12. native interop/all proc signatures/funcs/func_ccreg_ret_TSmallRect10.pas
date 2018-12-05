unit func_ccreg_ret_TSmallRect10;

interface
implementation

type
  TSmallRect10 = packed record x, y: Int32; z: UInt16; end;

function F(a: TSmallRect10): TSmallRect10; external 'CAT' name 'func_ccreg_ret_TSmallRect10'; 

var
  a, r: TSmallRect10;
  
procedure Test;
begin
  a.x := 1;
  a.y := 2;
  a.z := 3;
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.