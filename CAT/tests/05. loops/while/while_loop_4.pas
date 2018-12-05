unit while_loop_4;

interface

implementation

var G: Int32;

procedure Test;
var
  a: Int32;
begin
  a := 0;
  while a + 1 < 10 do
  begin
    inc(G);
    inc(a);
  end; 
  
  a := 0;
  while a * 2 < 10 do
  begin
    inc(G);  
    inc(a);
  end;   
end;

initialization
  Test();

finalization
  Assert(G = 14);
end.