unit generic_procs_4;

interface

implementation

function F<T>(Value: T): T;
begin
  Result := Value + 1;
end;

var G: Int32;

procedure Test;
begin
  G := F(1);
  G := F<Int32>(1);
end;

initialization
  Test();

finalization
  Assert(G = 2);
  
end.