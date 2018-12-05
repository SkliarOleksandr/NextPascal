unit anonymous_proc_2;

interface

implementation

var
  G1, G2: Int32;

procedure RunProc1(p: procedure);
begin
  P();
end;

procedure RunProc2(p: procedure(a: Int32));
begin
  p(222);
end;

procedure Test;
begin
  RunProc1(procedure begin G1 := 111; end);
  RunProc2(procedure(b: Int32) begin G2 := b; end);  
end;

initialization
  Test();
  
finalization  
  Assert(G1 = 111);
  Assert(G2 = 222);  

end.