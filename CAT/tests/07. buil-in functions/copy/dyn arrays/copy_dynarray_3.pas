unit copy_dynarray_3;

interface

implementation

var A1, A2: array of Int32;

procedure Test;
var
  i, c: Int32;
begin
  A1 := [3, 2, 3];
  i := 0;
  c := 3;
  A2 := copy(A1, i, c);
end;

initialization
  Test();

finalization
  Assert(A2[0] = 3);
  Assert(A2[1] = 2);
  Assert(A2[2] = 3);   

end.