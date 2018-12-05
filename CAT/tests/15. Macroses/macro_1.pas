unit macro_1;

interface

implementation

var
  G: Int32;

#macro M;
begin
  G := 1;
  G := G + 1;     
end;

procedure Test;
begin 
  M();
end;

initialization
  Test();

finalization
  Assert(G = 2);
end.