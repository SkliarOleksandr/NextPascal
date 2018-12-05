unit ifthen_1;

interface

function F1(a: int32): Int32; export;

implementation

function F1(a: int32): Int32;
begin
  if a >= 0 then
    Result := 11
  else
    Result := 12;  
end;

initialization

finalization

end.