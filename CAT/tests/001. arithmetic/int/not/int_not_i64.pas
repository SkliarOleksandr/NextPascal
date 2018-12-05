unit int_not_i64;

interface

implementation

var I: Int64;

procedure Test;
begin
  I := 8000000000000;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = -8000000000001);

end.