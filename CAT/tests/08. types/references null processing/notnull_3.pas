unit notnull_3;

interface

implementation

var
  GP1, GP2: Pointer;

procedure SetPtr(P1: Pointer!; P2: Pointer?);
begin
  GP1 := P1;
  GP2 := P2;
end;

procedure Test;
begin
  // SetPtr(nil, nil); // compile error!
  // SetPtr(GP1, nil); // compile error!  
  SetPtr(@GP1, nil); // compile error!  
end;

initialization
  Test();

finalization
  Assert(Assigned(GP1));
  Assert(not Assigned(GP2));  
end.