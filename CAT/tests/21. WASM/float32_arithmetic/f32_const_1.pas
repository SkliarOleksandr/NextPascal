unit if32_const_1;

interface

function Test: float32; export;

implementation

uses System;

function Test: float32;
begin
  Result := 0.0003;
end;

initialization
  Test();

finalization

end.