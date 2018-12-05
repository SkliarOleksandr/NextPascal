unit anonymous_proc_1;

interface

var
  G1, G2: Int32;

implementation

type
  TProc1 = procedure;  
  TProc2 = procedure(a, b: int32);    
  
procedure RunProc1(p: TProc1);
begin
  p();
end;

procedure RunProc2(p: TProc2);
begin
  p(120, 4);
end;

procedure Test;
var
  p1: TProc1;
  p2: TProc2;
begin
  p1 := procedure begin
          G1 := 123; 
        end;
  RunProc1(p1);
    
  p2 := procedure(a, b: int32) begin
          G2 := a + b;
        end;
  RunProc2(p2);  
end;

initialization
  Test();
  
finalization  
  Assert(G1 = 123);
  Assert(G2 = 124); 

end.