unit pointers_proc_explicit_1;

interface

implementation

type
  TProc = procedure;

var 
  P: Pointer;
  G: Boolean;

procedure SetG;
begin
  G := True;
end;

procedure Test;
begin
  P := @SetG;
  TProc(P)();
end;

initialization
  Test();

finalization
  Assert(G);

end.