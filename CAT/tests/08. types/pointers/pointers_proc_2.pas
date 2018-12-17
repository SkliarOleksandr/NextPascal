unit pointers_proc_2;

interface

implementation

procedure Test;
begin

end;

var P: Pointer = @Test;

initialization
  Test();

finalization
  Assert(Assigned(P));
end.