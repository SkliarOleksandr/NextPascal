unit int_or_3;

interface

implementation

var
  A1, A2: Int32 = 0; 
  B: Int32 = 4; 
  C: Int32 = 5;
  D: Int32 = 2;

procedure Test;
begin
  A1 := B or C or D or 8;
  A2 := B or C or D or 0;  
end;

initialization
  Test();

finalization
  Assert(A1 = 15);
  Assert(A2 = 7);

end.