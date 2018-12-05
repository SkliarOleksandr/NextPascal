unit dtc_sarray_1;

interface

implementation

type
  TR = 0..1;

var        
  a1: array [TR] of int32 = (55, 66);
  a2: array [TR] of int32;

procedure Test;
begin
  a2 := a1;
end;

initialization
  Test();

finalization
  Assert(a2[0] = a1[0]);
  Assert(a2[1] = a1[1]); 
end.