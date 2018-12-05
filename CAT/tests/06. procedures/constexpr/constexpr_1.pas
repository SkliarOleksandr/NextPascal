unit constexpr_1;

interface

implementation

function GetDigit: Int32; pure;
begin
  Result := 6;
end;

var V: Int32;

procedure Test;
begin
  V := GetDigit();
end;

initialization
  Test();

finalization
  Assert(V = 6);
end.