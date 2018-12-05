unit lambda_static_6;

interface

implementation

var p: function(a, b: int32): Int32;
    G: Int32;

procedure Test;
begin
  p := ~[](a, b: int32): Int32 Result := a + b;  
  G := p(11, 44);
end;

initialization
  Test();

finalization
  Assert(G = 55);
end.