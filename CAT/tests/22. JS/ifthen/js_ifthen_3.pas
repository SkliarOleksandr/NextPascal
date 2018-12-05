unit js_ifthen_3;

interface

implementation

uses sys.console;

procedure Test(a, b: int32);
begin
  if (a > 0) and (b < 0) then
    log(1);       
end;

initialization
  Test(1, -2);

finalization

end.