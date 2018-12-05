unit casting_to_int_1;

interface

implementation

var
  P: Pointer;
  V1: NativeInt;
  V2: NativeUInt; 
   
procedure Test;
begin
  P := nil;
  V1 := NativeInt(P);
  V2 := NativeUInt(P);   
end;

initialization
  Test();

finalization

end.