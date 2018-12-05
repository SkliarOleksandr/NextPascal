unit typename_0;

interface

implementation

var s: string;

procedure Test;
begin
  s := typename(string);
end;

initialization
  Test();

finalization
  Assert(s = 'String');
end.