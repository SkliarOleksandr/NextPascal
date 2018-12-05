unit generic_procs_2;

interface

implementation

function Inc1<T>(Value: T): T;
begin
  Result := Value + 1;
end;

var G: Float32;
    I: Int32;

procedure Test;
begin
  I := 1;
  G := Inc1<Float32>(I);  
end;

initialization
  Test();

finalization
  Assert(G = 2);
  
end.