unit sarray_new_1;

interface

implementation

type
  TA = array [3] of string;

var
  A: ^TA;

procedure Test;
begin
//  New(A);
//  Free(A);
end;

initialization
  Test();

finalization

end.