unit str_params_1;

interface


implementation

var S: string;

procedure Test<T>(const Str: T);
begin
  S := Str;
end;

initialization
  Test('aaa');

finalization
  Assert(S = 'aaa');
end.