unit rtti_1;

interface

uses
  System, sys.rtti;

var
  P: TObject;
  ti: TRTTIOrdinal;
  Signed: Boolean;
  MinI8, MaxI8: Int64;

implementation

procedure Test;
begin
  P := TypeInfo(Int8);
  ti := P as TRTTIOrdinal;
  Signed := ti.Signed;
  MinI8 := ti.LoBound;
  MaxI8 := ti.HiBound;    
end;

initialization
  Test();

finalization
  Assert(Signed);
  Assert(MinI8 = Low(Int8)); 
  Assert(MaxI8 = High(Int8));  

end.