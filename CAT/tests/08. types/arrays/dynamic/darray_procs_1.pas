unit darray_procs_1;

interface

implementation

var G: Int32;

procedure IncG;
begin
  Inc(G);
end;

procedure Test;
var
  Arr: array of procedure;
begin
  SetLength(Arr, 10);
  for var i := 0 to Length(Arr) - 1 do
    Arr[i] := IncG;    
    
  for var i := 0 to Length(Arr) - 1 do
    Arr[i]();       
end;

initialization
  Test();

finalization
  Assert(G = 10);
end.