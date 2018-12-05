unit typeid_1;

interface

implementation

var
  id: TDataTypeID; 
  
procedure Test;
begin
  id := typeid(id);
end;

initialization
  Test();

finalization
  Assert(id = dtEnum);
end.