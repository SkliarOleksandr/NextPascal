unit gvar_init_sarray_6;

interface

implementation

type
  TRec = record
    a, b: int32;
  end;

var Arr: array [Boolean, 4] of TRec = 
  ( 
    ([1, 1], [2, 2], [3, 3], [4, 4]),
    ([1, 1], [2, 2], [3, 3], [4, 4])    
  );

initialization

finalization
  Assert(Arr[False, 2].a = 3);
  Assert(Arr[True, 3].b = 4);  
end.