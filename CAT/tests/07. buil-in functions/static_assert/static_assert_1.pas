unit static_assert_1;

interface

implementation

procedure Test;
begin
  StaticAssert(SizeOf(Int32) = 4);
end;

initialization
  Test();

finalization

end.