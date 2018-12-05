unit Unit1;

interface

var G: Int32;

implementation

procedure Test;
var
  a: int32;
  b: int32 absolute a;
begin
  a := 12; 
  G := b;
end; 

initialization
  Test();
  
finalization
  assert(G = 12);
  
end.