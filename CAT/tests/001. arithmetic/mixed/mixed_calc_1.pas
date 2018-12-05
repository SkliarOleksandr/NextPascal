unit mixed_calc_1;

interface

implementation

function GetStr(const B: string; I, C: Int32): string;
begin
  Result := B + 'VALUE';  
end;

var A, B: string;

procedure Test;
var
  i: Int32;
begin
  i := 0;
  B := 'NEW';   
  A := B + GetStr(B, 1 + i, i);  
end;

initialization
  Test();

finalization
  Assert(A = 'NEWNEWVALUE');
end.