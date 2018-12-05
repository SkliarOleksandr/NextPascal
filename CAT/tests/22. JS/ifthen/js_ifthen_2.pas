unit js_ifthen_2;

interface

implementation

uses sys.console;

procedure Test(a: int32);
begin
  if a > 0 then
    log(1)
  else            
    log(2);       
end;

initialization
  Test(1);
  
finalization

end.