unit variant_0;

interface

implementation

var 
  V: Variant;

procedure Test;
begin
  V := SizeOF(Variant);
  Assert(V = 16);  
end;

initialization
  Test();

finalization

end.