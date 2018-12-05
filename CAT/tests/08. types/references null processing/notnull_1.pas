unit notnull_1;

interface

implementation

var 
  ptr_notnull: Pointer!;   // not null
  ptr_nullable: Pointer?;  // nullable 

procedure Test;
begin
  ptr_nullable := nil;
  ptr_notnull := @ptr_nullable;
end;

initialization
  Test();

finalization
  Assert(Assigned(ptr_notnull));
  Assert(not Assigned(ptr_nullable));  
end.