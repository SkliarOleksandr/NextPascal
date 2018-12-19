unit simple_proc_call_2;

interface

implementation

var G: Int32;

function GetFive: Int32;
begin
  Result := 6;
  G := Result;
end;

initialization
  GetFive;

finalization
  Assert(G = 6);
end.