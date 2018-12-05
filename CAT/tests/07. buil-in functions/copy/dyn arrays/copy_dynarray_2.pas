unit copy_dynarray_2;

interface

implementation

var A1, A2: array of Int32;

procedure Test;
var
  i: Int32;
begin
  A1 := [3, 2, 1];
  i := 1;
  A2 := copy(A1, i, 2);
end;

initialization
  Test();

finalization
  Assert(A2[0] = 2);
  Assert(A2[1] = 1);  

end.