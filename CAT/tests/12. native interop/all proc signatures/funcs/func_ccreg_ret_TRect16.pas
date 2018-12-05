unit func_ccreg_ret_TRect16;

interface

implementation

type
  TRect16 = record x, y, z, w: Int32; end;

function F(a: TRect16): TRect16; external 'CAT' name 'func_ccreg_ret_TRect16'; 

var
  a, r: TRect16;
  
procedure Test;
begin
  r.x := 111;
  a.x := 222;
  r := F(a);
  Assert(a = r);  
end;

initialization
  Test();

finalization

end.