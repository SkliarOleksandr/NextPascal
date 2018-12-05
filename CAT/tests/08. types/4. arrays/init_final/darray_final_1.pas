unit darray_final_1;

interface

implementation

type
  TA = array of string;

var 
  A: TA;
  
procedure Test;
begin
  SetLength(A, 3);
  A[0] := copy('str');
  A[1] := copy('str');
  A[2] := copy('str');    
end;

initialization
  Test();

finalization

end.