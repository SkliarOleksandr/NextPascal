unit rtti_getunitslist_1;

interface

implementation

uses sys.rtti;

var Units: TRTTIUnits;
    U0, U1, U2: string;

procedure Test;
begin
  Units := GetUnitsList();
  U0 := Units[0].Name;
  U1 := Units[1].Name;
  U2 := Units[2].name;     
end;

initialization
  Test();

finalization
  Assert(Length(Units) = 3);
  Assert(U0 = 'system');
  Assert(U1 = 'sys.rtti'); 
  Assert(U2 = 'rtti_getunitslist_1');       
end.