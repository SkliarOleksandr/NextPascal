unit js_while_2;

interface

implementation

uses sys.console;

procedure Test;
begin
  var g := 0;
  var a := 0;
  while a < 10 do
  begin
    inc(g);
    var b := 0;
    while b < 10 do
    begin
      inc(b);
      inc(g);
    end;
    inc(a); 
  end;
  log(g);
end;

initialization
  Test();

finalization

end.