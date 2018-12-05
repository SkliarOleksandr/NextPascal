unit callstack_1;

interface

uses sys.utils;

implementation

var CS: TCallStack;

procedure Test;
begin
  CS := GetCallStack();
end;

initialization
  Test();

finalization

end.