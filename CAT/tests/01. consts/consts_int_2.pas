unit consts_int_2;

interface

implementation

uses System;

procedure Test;
begin
  Assert(MaxInt8 > 0);
  Assert(MaxInt16 > MaxInt8);
  Assert(MaxInt32 > MaxInt16);
  Assert(MaxInt64 > MaxInt32);
  
  Assert(MaxUInt8 = High(UInt8));  
  Assert(MaxUInt16 = High(UInt16));  
  Assert(MaxUInt32 = High(UInt32));
  Assert(MaxUInt64 = High(UInt64));     
end;

initialization
  Test();

finalization

end.