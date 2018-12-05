unit assert_1;

interface

implementation

var
  B:  Boolean;

initialization
  B := True;
  Assert(B);

finalization
  Assert(True);
  
end.