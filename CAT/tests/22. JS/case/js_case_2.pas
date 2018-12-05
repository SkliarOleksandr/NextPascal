unit js_case_2;

interface

implementation

uses sys.console;

var G: int32;

procedure Test;
begin
  var a := 2;
  case a of
    1: g := 10;
    2: g := 20;
    3: g := 30;     
  end; 
  inc(g);
end;

initialization
  Test();
  log(g);

finalization

end.