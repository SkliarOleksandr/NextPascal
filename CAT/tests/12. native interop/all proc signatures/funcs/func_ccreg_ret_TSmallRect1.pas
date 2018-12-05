unit func_ccreg_ret_TSmallRect1;

interface

implementation

type
  TSmallRect1 = packed record x: UInt8; end;

function F(a: TSmallRect1): TSmallRect1; external 'CAT' name 'func_ccreg_ret_TSmallRect1'; 

var
  a, r: TSmallRect1;
  
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