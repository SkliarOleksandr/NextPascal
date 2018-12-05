unit js_proc_exit_1;

interface

implementation

uses sys.console;

function Test: Int32;
begin
  Exit(15);
end;

initialization
  log(Test());

finalization

end.