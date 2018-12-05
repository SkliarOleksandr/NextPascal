unit sarray_local_str_1;

interface

implementation

type
  TArr = array [2] of string;

procedure Test;
var
  A: TArr;
begin
  A[0] := 'asdf';
  A[1] := 'fdsa';
  Assert(A[0] = 'asdf');
  Assert(A[1] = 'fdsa');  
end;

initialization
  Test();

finalization

end.