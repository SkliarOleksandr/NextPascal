unit assigned_1;

interface

implementation

type
  Pint32 = ^Int32;
  
var
  p: PInt32;
  B: Boolean;
  
procedure Test;
begin
  p := nil;
  B := assigned(p);
end;

initialization
  Test();

finalization  
  Assert(B = False);
end.