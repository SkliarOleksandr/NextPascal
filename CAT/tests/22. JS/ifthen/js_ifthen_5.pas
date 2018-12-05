unit js_ifthen_5;

interface

implementation

uses sys.console;

procedure Test(a, b: int32);
begin
  if a > 0 then
  begin
    log(1);
    if b > 0 then
      log(2);
    log(3);  
  end;         
end;

initialization
  Test(1, 2);


finalization

end.