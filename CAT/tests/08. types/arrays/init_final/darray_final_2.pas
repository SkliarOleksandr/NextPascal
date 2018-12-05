unit darray_final_2;

interface

implementation

type
  TA = array of TObject;

var 
  A: TA;
  
procedure Test;
begin
  SetLength(A, 3);
  A[0] := TObject.Create();
  A[1] := TObject.Create();
  A[2] := TObject.Create();    
end;

initialization
  Test();

finalization

end.