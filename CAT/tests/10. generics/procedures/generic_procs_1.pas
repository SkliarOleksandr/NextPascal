unit generic_procs_1;

interface

implementation

procedure Inc<T>(var Value: T);
begin
  Value := Value + 1;
end;

var
  G: Int32;
  F: float32;
  
procedure TestI;
begin
  G := 1;
  Inc(G);  
  Inc<Int32>(G);
end;

procedure TestF;
begin
  F := 1.5;
  Inc(F);
  Inc<float32>(F);     
end;

initialization
  TestI();
  TestF();
  
finalization
  Assert(G = 3);
  Assert(F = 3.5);  
end.