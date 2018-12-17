unit pointers_proc_1;

interface

implementation

var P: Pointer; 
               
procedure Test;
begin
  P := @Test;
end;

initialization
  Test();

finalization
  Assert(Assigned(P));
end.