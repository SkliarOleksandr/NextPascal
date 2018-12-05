unit rtti_unittypes_1;

interface

implementation

uses sys.rtti;

type
  TT = record
    a, b: int32; 
  end;

var
  U: TRTTIUnit; 
  Types: TRTTITypes;
  A: TT;

procedure Test;
begin
  U := GetCurrentUnit();
  Types := U.Types;
end;

initialization
  Test();

finalization
  Assert(U.TypesCount = 1);
  Assert(Types[0].Name = 'TT');  
end.