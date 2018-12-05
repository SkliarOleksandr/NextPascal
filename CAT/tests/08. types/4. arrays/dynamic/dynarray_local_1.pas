unit dynarray_local_1;

interface

implementation

procedure Test;
var
  arr: array of int32;
begin
  SetLength(arr, 2);
  arr[0] := 12;
  arr[1] := 13;  
end;

initialization
  Test();

finalization

end.