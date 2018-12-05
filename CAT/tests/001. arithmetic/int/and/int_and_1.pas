unit int_and_1;

interface

implementation

var
  A: Int32 = 0; 
  B: Int32 = 1; 
  C: Int32 = 3;

procedure Test;
begin
  A := B and C;
end;

initialization
  Test();

finalization
  Assert(A = 1);
end.