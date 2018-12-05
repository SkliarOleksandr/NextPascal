unit move_2;

interface

implementation

var A: array [3] of Int16 = (100, 200, 300);
    B: array [3] of Int16; 

procedure Test;
begin
  Move(A, 0, B, 0, 3); 
end;

initialization
  Test();

finalization
  Assert(B[0] = 100);   
  Assert(B[1] = 200);
  Assert(B[2] = 300);    

end.