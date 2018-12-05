unit func_ccreg_ret_TSmallRect11;

interface
implementation

type
  TSmallRect11 = packed record x, y: Int32; z: UInt16; w: Uint8; end;

function F(a: TSmallRect11): TSmallRect11; external 'CAT' name 'func_ccreg_ret_TSmallRect11'; 

var
  a, r: TSmallRect11;
  
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