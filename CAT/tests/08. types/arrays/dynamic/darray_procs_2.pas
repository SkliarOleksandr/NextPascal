unit darray_procs_2;

interface

implementation

var G: Int32;

procedure IncG(Delta: Int32);
begin
  Inc(G, Delta);
end;

procedure DecG(Delta: Int32);
begin
  Dec(G, Delta);
end;

procedure Test;
var
  Arr: array of procedure(Delta: Int32);
begin
  SetLength(Arr, 2);
  Arr[0] := IncG;
  Arr[1] := DecG;
  
  Arr[0](5);
  Arr[1](6);    
end;

initialization
  Test();

finalization
  Assert(G = -1);
end.