unit macro_5;

interface

implementation

var G: Int32;

#macro INIT(V1);
begin
  #emit V1 + ' := 1;';
  #emit V1 + ' := ' + V1 + ' + 1;';      
end;

procedure Test;
begin
  INIT(G);
end;

initialization
  Test();

finalization
  Assert(G = 2);

end.