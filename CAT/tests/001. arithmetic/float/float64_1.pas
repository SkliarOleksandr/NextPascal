unit float64_1;

interface

implementation

var A, B, C: Float64;

procedure Test_Add;
begin
  A := 1.1;
  B := 1.2;
  C := A + B;
  Assert(C >= 2.3);  
end;

procedure Test_Sub;
begin
  A := 1.1;
  B := 1.2;
  C := B - A;
  Assert(C >= 0.1);  
end;

procedure Test_Mul;
begin
  A := 10.1;
  B := 2.0;
  C := A * B;
  Assert(C >= 20.2);  
end;

procedure Test_Div;
begin
  A := 10.2;
  B := 2.0;
  C := A / B;
  Assert(C >= 5.1);  
end;

initialization
  Test_Add();
  Test_Sub();
  Test_Mul();
  Test_Div();

finalization

end.