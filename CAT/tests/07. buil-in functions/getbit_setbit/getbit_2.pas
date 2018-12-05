unit getbit_2;

interface

implementation

var
  V: array[4] of UInt8; 
  B: Boolean;    

procedure Test;
begin
  V[3] := $80;
  B := getbit(V, 31);  
end;

initialization
  Test();

finalization
  Assert(B);
end.