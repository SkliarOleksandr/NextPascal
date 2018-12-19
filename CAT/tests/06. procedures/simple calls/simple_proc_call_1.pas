unit simple_proc_call_1;

interface

implementation

var G: Int32; 

procedure Test;
begin
  G := 3;
end;

initialization
  Test;

finalization
  Assert(G = 3);
end.