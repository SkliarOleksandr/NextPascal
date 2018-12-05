unit int_or_1;

interface

implementation

var
  A: Int32 = 0;
  B: Int32 = 1;
  C: Int32 = 2; 

procedure Test;
begin
  A := B or C; 
end;

initialization
  Test();

finalization
  Assert(A = 3);
end.