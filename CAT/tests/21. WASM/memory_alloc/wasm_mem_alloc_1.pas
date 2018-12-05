unit wasm_mem_alloc_1;

interface

function Test: Pointer; export;

implementation

function Test: Pointer;
type
  TRec = record end;
var
  P: ^TRec;
begin
  new(p);
end;

initialization

finalization

end.