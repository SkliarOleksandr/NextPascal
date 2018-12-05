unit dtc_darray_1;

interface

implementation

var
  a1: array of int32;
  a2: array of int32;

procedure Test;
begin
  a1 := [1, 2, 3];
  a2 := a1;
end;

initialization
  Test();

finalization
  Assert(a2[2] = a1[2]);
end.