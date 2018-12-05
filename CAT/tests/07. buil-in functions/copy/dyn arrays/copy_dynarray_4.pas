unit copy_dynarray_4;

interface

implementation

var A1, A2: array of Int32;

procedure Test;
begin
  A1 := [1, 2, 3, 4];
  A2 := copy(A1);
end;

initialization
  Test();

finalization
  Assert(Length(A2) = 4);
  Assert(A2[3] = 4);  

end.