unit gvar_init_sarray_5;

interface

implementation

var Arr: array [Boolean, 4] of Int32 = 
  ( 
    (1, 2, 3, 4),
    (1, 2, 3, 4)    
  );

initialization

finalization
  Assert(Arr[False, 2] = 3);
  Assert(Arr[True, 2] = 3);  
end.