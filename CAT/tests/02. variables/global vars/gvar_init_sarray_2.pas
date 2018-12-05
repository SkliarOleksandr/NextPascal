unit gvar_init_sarray_2;

interface

implementation

var Arr: array [2, 2] of Int32 = ((1, 2), (3, 4));

procedure Test;
begin

end;

initialization
  Test();

finalization
  Assert(Arr[0, 0] = 1);
  Assert(Arr[1, 1] = 4);  

end.