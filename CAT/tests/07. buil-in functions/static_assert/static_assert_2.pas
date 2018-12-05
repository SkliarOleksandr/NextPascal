unit static_assert_2;

interface

implementation

procedure Test;
begin
  StaticAssert(SizeOf(Int32) = 4, 'Int32 must be 4 byte size');
end;

initialization
  Test();

finalization

end.