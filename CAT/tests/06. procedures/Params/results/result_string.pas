unit result_string;

interface

implementation

function Get: string;
begin
  SetLength(Result, 3);
  Result[0] := 'A';
  Result[1] := 'B';
  Result[2] := 'C';     
end;

procedure Test;
var 
  A: string;
begin
  A := Get();
  Assert(A = 'ABC');
end;

initialization
  Test();

finalization

end.