unit pointers_1;

interface

implementation

var P: Pointer;

procedure Test;
begin
  P := nil;  
end;

initialization
  Test();

finalization
  Assert(P = nil);

end.