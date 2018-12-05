unit pointers_arrays_2;

interface

implementation

var A: array [3] of Int32;



procedure Test;
var 
  P: ^Int32;
begin
  P := @A[0];
  P^ := 11;
  
  P := @A[1];
  P^ := 12;
  
  P := @A[2];
  P^ := 13;       
end;

initialization
  Test();

finalization   
  Assert(A[0] = 11);
  Assert(A[1] = 12);
  Assert(A[2] = 13);    
 

end.