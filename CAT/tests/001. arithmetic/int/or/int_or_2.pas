unit int_or_2;

interface

implementation

var
  A: Int32 = 0;
  B: Int32 = 4; 

procedure Test;
begin
  A := B or 2; 
end;

initialization
  Test();

finalization
  Assert(A = 6);

end.