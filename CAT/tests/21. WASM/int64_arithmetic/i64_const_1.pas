unit i64_const_1;

interface

function Test: Int64; export;

implementation

uses System;

function Test: Int64;
begin
  Result := MaxInt64;
end;

initialization
  Test();

finalization

end.