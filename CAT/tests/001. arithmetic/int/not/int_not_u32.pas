unit int_not_u32;

interface

implementation

var I: UInt32;

procedure Test;
begin
  I := 1;
  I := not I;
end;

initialization
  Test();

finalization
  Assert(I = High(UInt32) - 1);
end.