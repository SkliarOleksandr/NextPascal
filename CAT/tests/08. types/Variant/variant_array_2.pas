unit variant_array_2;

interface

implementation
  
var
  I: int32;
  S: string;
  F: float64;  
                
procedure P(const A: array of variant);
begin
  I := A[0];
  S := A[1];
  F := A[2];      
end;                
                
procedure Test;
begin
  P([11, 'abcdef', 4.6]);
end;

initialization
  Test();

finalization
  Assert(I = 11);
  Assert(S = 'abcdef');
  Assert(F = 4.6);    
end.