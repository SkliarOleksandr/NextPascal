unit wasm_2;

interface

implementation

function Test(a, b: integer): integer;
begin
  Result := (a+1)*(b-1);
end;

initialization
  Test(1, 2);

finalization

end.