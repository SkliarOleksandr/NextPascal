unit func_ccreg_ret_TSmallRect2;

interface

implementation

type
  TSmallRect2 = packed record x, y: UInt8; end;

function F(a: TSmallRect2): TSmallRect2; external 'CAT' name 'func_ccreg_ret_TSmallRect2'; 

var
  a, r: TSmallRect2;
  
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