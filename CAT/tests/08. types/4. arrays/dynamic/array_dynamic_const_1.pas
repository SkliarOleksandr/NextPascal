unit array_dynamic_const_1;

interface

implementation

var Len: Int32;

procedure PutLen(const A: array of int32);
begin
  Len := Length(A);
end;

procedure Test;
begin
  PutLen([1, 2, 3, 4, 5]);
end;

initialization
  Test();

finalization
  Assert(Len = 5);


end.