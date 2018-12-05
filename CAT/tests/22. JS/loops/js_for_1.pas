unit js_for_1;

interface

implementation

uses sys.console;

procedure Test;
var
  i: integer;
begin
  for i := 0 to 10 do
    log(3);
end;

initialization
  Test();

finalization

end.