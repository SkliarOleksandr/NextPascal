unit casting_to_int_2;

interface

implementation

type TRec = record 
       a, b: int32;
     end;

var  R: TRec;
     P: ^TRec;
    V1:  NativeInt;
    V2:  NativeUInt;     

procedure Test;
begin
  P := @R; 
  V1 := NativeInt(P);
  V2 := NativeUInt(P);    
end;

initialization
  Test();

finalization

end.