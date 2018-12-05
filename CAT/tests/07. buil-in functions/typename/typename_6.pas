unit typename_6;

interface

implementation

function GetTypeName<T>: string;
begin
  result := typename(T);
end;

var S: string;

procedure Test;
begin
  S := GetTypeName<Int32>();
end;

initialization
  Test();

finalization
  Assert(S = 'Int32');
end.