unit int_and_3;

interface

implementation

var
  A1, A2: Int32 = 0; 
  B: Int32 = 4; 
  C: Int32 = 5;
  D: Int32 = 7;

procedure Test;
begin
  A1 := B and C and D and 15;
  A2 := B and C and D and 16;  
end;

initialization
  Test();

finalization
  Assert(A1 = 4);
  Assert(A2 = 0);
  
end.
