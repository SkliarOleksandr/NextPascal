unit gvar_init_sarray_3;

interface

implementation

var Arr: array [2, 2, 2] of Int32 = 
 (
   (
     (1, 2), (3, 4)
   ),   
   (
     (5, 6), (7, 8)
   )
 );

initialization
  Assert(Arr[1, 1, 0] = 7);
  Assert(Arr[1, 1, 1] = 8);

finalization

end.