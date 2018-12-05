unit rtti_ordinal_4;

interface

implementation

uses
  System, sys.rtti;

var
  ti: TRTTIOrdinal;

procedure Test_nint;
begin
  ti := TypeInfo(NativeInt) as TRTTIOrdinal;
  Assert(ti.Name = 'NativeInt');
  Assert(ti.TypeSize = sizeof(Pointer));  
  Assert(ti.Signed = True);
//  Assert(ti.LoBound = Low(NativeInt));
//  Assert(ti.HiBound = High(NativeInt));
end;

procedure Test_nuint;
begin
  ti := TypeInfo(NativeUInt) as TRTTIOrdinal;
  Assert(ti.Name = 'NativeUInt');
  Assert(ti.TypeSize = sizeof(Pointer));  
  Assert(ti.Signed = False);
//  Assert(ti.LoBound = Low(NativeUInt));
//  Assert(ti.HiBound = High(NativeUInt));
end;

initialization
  Test_nint();
  Test_nuint();

finalization

end.