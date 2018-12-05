unit setbit_2;

interface

implementation

var
  V: Int64; 

procedure Test;
begin 
  setbit(V, 63, True);  
end;

initialization
  Test();

finalization
  Assert(V = Low(Int64));
end.