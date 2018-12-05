unit rtti_sarray_1;

interface

implementation

uses
  System, sys.rtti;

type
  TA1 = array [5] of Int32;
  TA2 = array [5, 5] of Int32;  

var
  ti: TRTTIArray;

procedure Test;
begin
  ti := TypeInfo(TA1) as TRTTIArray;
  Assert(ti.Name = 'TA1');
  Assert(ti.DimsCount = 1); 
  
  ti := TypeInfo(TA2) as TRTTIArray;
  Assert(ti.Name = 'TA2');
  Assert(ti.DimsCount = 2);   
end;

initialization
  Test();

finalization

end.