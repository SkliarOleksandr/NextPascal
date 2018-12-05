unit gvar_init_sarray_1;

interface

implementation

var Arr: array [3] of Int32 = (1, 2, 3);

initialization
  Assert(Arr[0] = 1);
  Assert(Arr[1] = 2);  
  Assert(Arr[2] = 3);
finalization
  
end.