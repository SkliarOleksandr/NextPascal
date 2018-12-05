unit macro_4;

interface

implementation

var AR: array [2] of Int32;

#macro ADD(V1, V2);
begin
  V1 := V1 + V2;  
end;

procedure Test;
begin
  AR[0] := 1;
  AR[1] := 2; 
  ADD(AR[0], AR[1]);
end;

initialization
  Test();

finalization
  Assert(AR[0] = 3);

end.