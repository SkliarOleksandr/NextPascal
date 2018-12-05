unit js_gvar_1;

#target js;

interface

implementation

uses sys.console;

var a: int32;

procedure Test;
begin
  a := 5;  
end;

initialization
  Test();
  log(a);

finalization

end.