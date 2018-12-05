unit setbit_0;

interface

implementation

var
  V: Int32; 

procedure Test;
begin 
  setbit(V, 1, True);  
end;

initialization
  Test();

finalization
  Assert(V = 2);
end.