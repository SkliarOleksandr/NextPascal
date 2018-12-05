unit rtti_getcurrentunit_1;

interface

implementation

uses sys.rtti;

var
  U: TRTTIUnit; 
  S: string;
  RC: Integer;
  RCID: TRTTIClassID;

procedure Test;
begin
  U := GetCurrentUnit();
  S := U.Name;
  RC := refcount(U);
  RCID := U.ClassID;
end;

initialization
  Test();

finalization
  Assert(S = 'rtti_getcurrentunit_1');
  Assert(RC = -1);
  Assert(RCID = TRTTIClassID.rttiUnit);   
end.