unit js_not_int_1;

interface

implementation

uses sys.console;

procedure Test;
begin
  var a := 5;
  a := not a;
  log(a);
end;

initialization
  Test();

finalization

end.