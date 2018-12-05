unit wasm_1;

interface

function Test(a: Int32): Int32; export;

implementation

function Test(a: Int32): Int32;
begin
  Result := a + 1;
end;

initialization

finalization

end.