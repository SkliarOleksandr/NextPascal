unit memset_4;

interface

implementation

var Arr: array of UInt32;

procedure Test;
begin
  setlength(Arr, 10);
  memset(Arr, $FF);
end;

initialization
  Test();

finalization
  Assert(Arr[0] = $FFFFFFFF);
  Assert(Arr[9] = $FFFFFFFF);  
end.