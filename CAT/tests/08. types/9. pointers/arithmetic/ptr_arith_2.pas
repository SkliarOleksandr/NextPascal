unit ptr_arith_1;

interface

implementation

var 
  P: Pointer;
  G1, G2:  NativeInt; 

procedure Test;
begin
  P := @P;
  G1 := NativeInt(P);
  Dec(P); 
  G2 := NativeInt(P);   
end;

initialization
  Test();

finalization
  Assert(G1 > G2);

end.