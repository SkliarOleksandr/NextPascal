unit nativeint_1;

interface

implementation
          
var
  NInt: NativeInt;
  G: Int32;          
          
procedure Test;
begin
  NInt := 123456;
  G := SizeOF(NInt);
end;

initialization
  Test();

finalization
  Assert(NInt = 123456);
  Assert(G = SizeOF(NInt));  

end.