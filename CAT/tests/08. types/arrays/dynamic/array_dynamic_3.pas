unit array_dynamic_3;

interface

implementation

type
  TIntArray = array of int32;
  
function ConcatArray(const A1, A2: TIntArray): TIntArray;
begin
  SetLength(Result, Length(A1) + Length(A2)); 
end;  

var
  A, B, R: TIntArray;

procedure Test;
begin
  SetLength(A, 5);
  SetLength(B, 10);  
  R := ConcatArray(A, B);  
end;

initialization
  Test();

finalization
  Assert(Length(R) = 15);
end.