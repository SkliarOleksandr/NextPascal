unit float32_2;

interface

implementation

var 
  A, B, C: Float32;

procedure Test_Add(var A, B, C: Float32);
begin
  A := 1.1;
  B := 1.2;
  C := A + B;
  Assert(C >= 2.3);  
end;

procedure Test_Sub(var A, B, C: Float32);
begin
  A := 1.1;
  B := 1.2;
  C := B - A;
  Assert(C >= 0.1);  
end;

procedure Test_Mul(var A, B, C: Float32);
begin
  A := 10.1;
  B := 2.0;
  C := A * B;
  Assert(C >= 20.2);  
end;

procedure Test_Div(var A, B, C: Float32);
begin
  A := 10.2;
  B := 2.0;
  C := A / B;
  Assert(C >= 5.1);  
end;

initialization
  Test_Add(A, B, C);
  Test_Sub(A, B, C);
  Test_Mul(A, B, C);
  Test_Div(A, B, C);

finalization

end.