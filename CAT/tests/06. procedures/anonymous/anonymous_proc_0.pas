unit anonymous_proc_0;

interface

implementation

type
  TProc = procedure;

var P: TProc;
    G: Int32; 
    
procedure Test;
begin
  P := procedure begin
         G := 444; 
       end;
  P();
end;  

initialization
  Test();

finalization
  Assert(G = 444);
end.