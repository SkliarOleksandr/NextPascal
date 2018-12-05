unit proc_1;

interface

function f1: Int32; export;
function f2: Int32; export;

implementation

function f1: Int32;
begin
  Result := 42;
end;

function f2: Int32;
begin
  Result := 43;
end;


initialization

finalization

end.