unit sarray_of_proc_1;

interface

implementation

type
  TProc = procedure;
  
  
procedure Proc1;
begin
end;

procedure Proc2;
begin
end;  
  
//var AProcs: array [2] of TProc = (Proc1, Proc2); 
  
procedure Test;
begin

end;

initialization
  Test();

finalization

end.