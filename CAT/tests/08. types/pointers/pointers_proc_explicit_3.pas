unit pointers_proc_explicit_3;

interface

implementation

type
  TProc = function(a, b: Int32): Int32;

var 
  P: Pointer;
  G: Int32;

function SetG(a, b: Int32): Int32;
begin
  Result := a + b;  
end;

procedure Test;
begin
  P := @SetG;
  G := TProc(P)(44, 33);
end;

initialization
  Test();

finalization
  Assert(G = 77);
end.