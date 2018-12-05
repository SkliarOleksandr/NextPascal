unit f64_const_1;

interface


function Test: float64; export;

implementation

uses System;

function Test: float64;
begin
  Result := float64(0.000002432352465346000000000000000000000000000000000000004);
end;

initialization
  Test();

finalization

end.