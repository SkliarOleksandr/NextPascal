unit memset_0;

interface

implementation

procedure Test;
var
  V: Int64;
begin
  memset(V, 0);
  Assert(V = 0); 
end;

initialization
  Test();

finalization

end.