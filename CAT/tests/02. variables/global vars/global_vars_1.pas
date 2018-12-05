unit Unit1;

interface

var
  A0, B0, C0, D0: Int32;
  A1, B1, C1, D1: Int32;
implementation

procedure Test;
begin
  A0 := 1;
  B0 := 2;
  C1 := 3;
  D1 := 4;
  
  A1 := A0;
  B1 := B0;
  C0 := C1;
  D0 := D1;   
end;

initialization
  Test();
  
finalization  
  assert(A0 = 1, 'A0 <> 1');
  assert(B0 = 2, 'B0 <> 2');
  assert(C0 = 3, 'C0 <> 3');
  assert(D0 = 4, 'D0 <> 4');
  assert(A1 = 1, 'A1 <> 1');
  assert(B1 = 2, 'A2 <> 2');
  assert(C1 = 3, 'A3 <> 3');
  assert(D1 = 4, 'A4 <> 4');
 
end.