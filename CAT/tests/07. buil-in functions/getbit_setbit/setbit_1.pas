unit setbit_1;

interface

implementation

var
  V: array [4] of UInt8; 

procedure Test;
begin 
  setbit(V, 7 + 8*2, True);  
end;

initialization
  Test();

finalization
  Assert(V[2] = 128);
end.