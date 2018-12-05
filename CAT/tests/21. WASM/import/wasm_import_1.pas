#skip
unit wasm_import_1;

interface

procedure Log(a: integer); external 'env' name 'xlog';

procedure Test(a: Integer); export;

implementation

procedure Test(a: Integer);
begin
  Log(a);
end;

initialization

finalization

end.