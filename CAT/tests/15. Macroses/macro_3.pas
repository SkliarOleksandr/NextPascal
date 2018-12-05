unit macro_3;

interface

implementation

var
  G: Int32;
  S: string;
  F: float64;

#macro ADD(V1, V2);
begin
  V1 := V1 + V2;  
end; 

procedure Test;
begin
  S := 'AB';
  ADD(S, 'XX');
  
  G := 2;
  ADD(G, 8);
  
  F := 1.5;
  ADD(F, 2.5);      
end;

initialization
  Test();

finalization
  Assert(S = 'ABXX');
  Assert(G = 10);
  Assert(F = 4);    
end.