unit post_inc_1;

interface

implementation

var 
  A: int32 = 0;
  B: int32 = 0; 
  C: int32 = 0;

procedure Test;
begin
  B := A--;
  C := A--;
end;

initialization
  Test();

finalization
   Assert(A = -2);
   Assert(B = 0);
   Assert(C = -1);      
end.