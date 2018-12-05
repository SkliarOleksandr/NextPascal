unit rtti_record_1;

interface

implementation

uses sys.rtti;

type
  TRec1 = record
    a, b: Boolean;
  end;
  TRec2 = packed record
    a, b: Boolean;
  end;  
var
  ti: TRTTIRecord;

procedure Test;
begin
  ti := typeinfo(TRec1) as TRTTIRecord;
  Assert(ti.Name = 'TRec1');
  Assert(ti.TypeSize = 8);
  Assert(ti.TypePacked = False);
  Assert(ti.FieldsCount = 2);
    
  ti := typeinfo(TRec2) as TRTTIRecord;
  Assert(ti.Name = 'TRec2');
  Assert(ti.TypeSize = 2);
  Assert(ti.TypePacked = True);    
  Assert(ti.FieldsCount = 2);         
end;

initialization
  Test();

finalization

end.