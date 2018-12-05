unit rtti_ordinal_2;

interface

implementation

uses
  System, sys.rtti;

var
  ti: TRTTIOrdinal;

procedure Test_int64;
begin
  ti := TypeInfo(Int64) as TRTTIOrdinal;
  Assert(ti.Name = 'Int64');
  Assert(ti.TypeSize = 8);  
  Assert(ti.Signed = True);
  Assert(ti.LoBound = MinInt64);
  Assert(ti.HiBound = MaxInt64);
end;

procedure Test_uint64;
begin
  ti := TypeInfo(UInt64) as TRTTIOrdinal;
  Assert(ti.Name = 'UInt64');
  Assert(ti.TypeSize = 8);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = MinUInt64);
  Assert(ti.HiBound = MaxUInt64);
end;

initialization
  Test_int64();
  Test_uint64();

finalization

end.