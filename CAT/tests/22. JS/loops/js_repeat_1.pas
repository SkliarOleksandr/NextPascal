unit js_repeat_1;

interface

implementation

uses sys.console;

procedure Test;
begin
  var i := 0;
  repeat
    inc(i); 
    log(11);
  until i > 10;
end;

initialization
  Test();

finalization

end.