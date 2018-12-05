unit rtti_2;

interface

implementation

uses sys.rtti;

var
  ti: TRTTIOrdinal;
  s: string;
  ds: Integer;
  lb, hb: Int64; 

procedure Test;
begin
  ti := TypeInfo(UInt32) as TRTTIOrdinal;
  s := ti.Name;
  ds := ti.TypeSize;
  lb := ti.LoBound;
  hb := ti.HiBound;    
  Assert(ti.TypeSize = 4);  
  Assert(ti.Signed = False);
  Assert(ti.LoBound = 0);
  Assert(ti.HiBound = MaxUInt32);
end; 

initialization
  Test();

finalization

end.