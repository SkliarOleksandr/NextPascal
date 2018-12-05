unit sizeof_1;

interface

implementation

procedure Test;
begin
  Assert(SizeOf(Int8) = 1);
  Assert(SizeOf(Int16) = 2);
  Assert(SizeOf(Int32) = 4);
  Assert(SizeOf(Int64) = 8);

  Assert(SizeOf(UInt8) = 1);
  Assert(SizeOf(UInt16) = 2);
  Assert(SizeOf(UInt32) = 4);
  Assert(SizeOf(UInt64) = 8); 

  Assert(SizeOf(NativeInt) = SizeOf(Pointer));
  Assert(SizeOf(NativeUInt) = SizeOf(Pointer));
  Assert(SizeOf(Float32) = 4);
  Assert(SizeOf(Float64) = 8);  

  Assert(SizeOf(Char) = 2);
  Assert(SizeOf(AnsiChar) = 1);
  Assert(SizeOf(Boolean) = 1);    

  Assert(SizeOf(String) = SizeOf(Pointer));
  Assert(SizeOf(AnsiString) = SizeOf(Pointer));  

  Assert(SizeOf(TGUID) = 16);
  Assert(SizeOf(Pointer) >= 4);          
end;

initialization
  Test();

finalization

end.