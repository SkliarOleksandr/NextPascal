unit memset_1;

interface

implementation

procedure Test;
var 
  Arr: array [10] of Int8;
begin
  memset(Arr);
  Assert(Arr[0] = 0);
  Assert(Arr[9] = 0);
end;

initialization
  Test();

finalization

end.