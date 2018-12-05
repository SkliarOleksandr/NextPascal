unit int_and_2;

interface

implementation

var 
  A: Int32 = 0;
  B: Int32 = 7;
 

procedure Test;
begin
  A := B and 4;
end;

initialization
  Test();

finalization
  Assert(A = 4);
end.