unit variant_array_1;

interface

implementation

var
  Arr: array of Variant;

procedure Test;
begin
  SetLength(Arr, 3);
  Arr[0] := 1;
  Arr[1] := copy('variant');
  Arr[2] := 5.7;   
end;

initialization  
  Test();

finalization
  Assert(Arr[0] = 1);
  Assert(Arr[1] = 'variant');
  Assert(Arr[2] = 5.7);      

end.