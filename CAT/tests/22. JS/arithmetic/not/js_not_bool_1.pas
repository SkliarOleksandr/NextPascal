unit js_not_bool_1;

interface

implementation

uses sys.console;

procedure Test;
begin
  var a := False;
  a := not a;
  log(a);
end;

initialization
  Test();

finalization

end.