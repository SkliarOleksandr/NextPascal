unit darray_init_2;

interface

implementation

type TArr = array of string;

var A: TArr;
    S: string;

procedure Test;
begin
  SetLength(A, 2);
  A[0] := S;
  A[1] := S;     
end;

initialization
  S := copy('string');
  Test();

finalization
  Assert(A[0] = 'string');
  Assert(A[1] = 'string');    
end.