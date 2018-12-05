unit int_not_u64;

interface

implementation

var I: UInt64;

procedure Test;
begin
  I := Low(UInt64);
  I := not I;
end;

initialization
  Test();

finalization
  // Assert(I = High(UInt64));  // не работает UInt64 нормально

end.