unit int_not_i16;

interface

implementation

var I: Int16;

procedure Test;
begin
  I := 16000;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = -16001);

end.