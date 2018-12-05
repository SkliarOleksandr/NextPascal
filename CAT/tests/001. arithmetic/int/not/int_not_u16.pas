unit int_not_u16;

interface

implementation

var I: UInt16;

procedure Test;
begin
  I := 65534;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = 1);

end.