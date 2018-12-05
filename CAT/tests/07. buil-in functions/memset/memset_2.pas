unit memset_2;

interface

implementation

var 
  Arr: array [10] of Int8;

procedure Test;
begin
  memset(Arr, 55);
end;

initialization
  Test();

finalization
  Assert(Arr[0] = 55);
  Assert(Arr[9] = 55);

end.