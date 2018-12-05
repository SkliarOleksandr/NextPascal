unit macro_2;

interface

implementation

var
  G: Int32;

#macro INC(V);
begin
  V := V + 1; 
end;

procedure Test;
begin
  G := 0;
  INC(G);
  INC(G);  
end;

initialization
  Test();

finalization
  Assert(G = 2);

end.