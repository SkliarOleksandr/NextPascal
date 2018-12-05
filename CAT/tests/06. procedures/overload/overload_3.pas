unit overload_3;

interface

implementation

function Get(A: Int32; B: Float32): Int32; 
begin
  Result := 12; 
end;

function Get(A, B: Int32): Int32; overload;
begin
  Result := 11; 
end;

var  G: Int32;

procedure Test;
begin
  G := Get(1, 2); 
end;

initialization
  Test();

finalization
  Assert(G = 11);
end.