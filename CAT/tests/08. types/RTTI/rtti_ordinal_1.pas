unit rtti_ordinal_1;

interface

implementation

uses
  System, sys.rtti;

var
  ti: TRTTIOrdinal;
  
procedure Test_int8;
begin
  ti := TypeInfo(Int8) as TRTTIOrdinal;
  Assert(ti.Name = 'Int8');
  Assert(ti.TypeSize = 1);  
  Assert(ti.Signed = True);
  Assert(ti.LoBound = MinInt8);
  Assert(ti.HiBound = MaxInt8);
end;

procedure Test_uint8;
begin
  ti := TypeInfo(UInt8) as TRTTIOrdinal;
  Assert(ti.Name = 'UInt8');
  Assert(ti.TypeSize = 1);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = MinUInt8);
  Assert(ti.HiBound = MaxUInt8);
end;

procedure Test_int16;
begin
  ti := TypeInfo(Int16) as TRTTIOrdinal;
  Assert(ti.Name = 'Int16');
  Assert(ti.TypeSize = 2);  
  Assert(ti.Signed = True);
  Assert(ti.LoBound = MinInt16);
  Assert(ti.HiBound = MaxInt16);
end;

procedure Test_uint16;
begin
  ti := TypeInfo(UInt16) as TRTTIOrdinal;
  Assert(ti.Name = 'UInt16');
  Assert(ti.TypeSize = 2);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = MinUInt16);
  Assert(ti.HiBound = MaxUInt16);
end;

procedure Test_int32;
begin
  ti := TypeInfo(Int32) as TRTTIOrdinal;
  Assert(ti.Name = 'Int32');
  Assert(ti.TypeSize = 4);  
  Assert(ti.Signed = True);
  Assert(ti.LoBound = MinInt32);
  Assert(ti.HiBound = MaxInt32);
end;

procedure Test_uint32;
begin
  ti := TypeInfo(UInt32) as TRTTIOrdinal;
  Assert(ti.Name = 'UInt32');
  Assert(ti.TypeSize = 4);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = MinUInt32);
  Assert(ti.HiBound = MaxUInt32);
end;

initialization
  Test_int8();
  Test_uint8();
  Test_int16();
  Test_uint16();      
  Test_int32();
  Test_uint32();   

finalization

end.