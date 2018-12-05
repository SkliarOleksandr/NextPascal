unit func_ccreg_ret_TSmallRect9;

interface
implementation

type
  TSmallRect9 = packed record x, y: Int32; z: UInt8; end;

function F(a: TSmallRect9): TSmallRect9; external 'CAT' name 'func_ccreg_ret_TSmallRect9'; 

var
  a, r: TSmallRect9;
  
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