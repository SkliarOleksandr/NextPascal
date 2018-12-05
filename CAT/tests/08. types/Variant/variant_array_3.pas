unit variant_array_3;

interface

implementation

var G, Y, Z: Int32;

procedure AD(X: Int32; const YZ: array of Variant);
begin
  Y := YZ[0];
  Z := YZ[1];   
  G := X + Y + Z;  
end;

procedure Test;
begin
  AD(5, [2, 3]);
end;

initialization
  Test();

finalization
  Assert(G = 10);
end.