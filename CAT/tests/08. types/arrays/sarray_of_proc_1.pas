unit sarray_of_proc_1;

interface

implementation

type
  TProc = procedure;
  
var
  A: array [3] of TProc;
  G: Int32;  

procedure IncG;
begin
  Inc(G);
end;

procedure Test;
begin
  A[0] := IncG;
  A[1] := IncG;
  A[2] := IncG;    
  
  A[0]();
  A[1]();
  A[2]();    
end;

initialization
  Test();

finalization
  Assert(G = 3);
end.