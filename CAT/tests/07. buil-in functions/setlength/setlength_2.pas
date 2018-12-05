unit setlength_2;

interface

implementation

var Arr: array of int32;

procedure Test;
begin
  SetLength(Arr, 1);
  Arr[0] := 10;
  SetLength(Arr, 2);
  Arr[1] := 20;
  SetLength(Arr, 3);
  Arr[2] := 30;        
end;

initialization
  Test();

finalization
  Assert(Arr[0] = 10);
  Assert(Arr[1] = 20);
  Assert(Arr[2] = 30);    
end.