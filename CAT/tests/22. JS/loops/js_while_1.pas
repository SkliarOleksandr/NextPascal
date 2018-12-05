unit js_while_1;

interface

implementation

uses sys.console;

procedure Test;
var
  a: int32;
begin
  a := 0;
  while a < 3 do 
  begin
    inc(a);
    log('5'); 
  end;   
end;

initialization
  Test();

finalization

end.