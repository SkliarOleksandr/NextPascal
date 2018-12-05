unit copy_dynarray_5;

interface

implementation

var A: array of Int32;

procedure Test;
begin
//  A := copy([1, 2, 3, 4]);
  #warning 'array need to cast';
end;

initialization
  Test();


finalization
//  Assert(Length(A) = 4);
//  Assert(A[3] = 4);  

end.