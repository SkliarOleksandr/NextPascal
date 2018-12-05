unit getbit_1;

interface

implementation

var
  V: array[1] of UInt32; 
  B: Boolean;    

procedure Test;
begin
  V[0] := $80;
  B := getbit(V, 7);  
end;

initialization
  Test();

finalization
  Assert(B);
end.