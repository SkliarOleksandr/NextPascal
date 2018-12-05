unit sysutils_getcallstack_1;

interface

implementation

uses sys.utils;

var CS: TCallStack;

procedure Test;
begin
  CS := GetCallStack();
end;

initialization    
  Test();

finalization
  Assert(Length(CS) = 2);
end.