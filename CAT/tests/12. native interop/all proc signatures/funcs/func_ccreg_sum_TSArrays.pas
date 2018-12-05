unit func_ccreg_sum_TSArrays;

interface

implementation

type
  TSArray = array[0..9] of Int32;
  TIntArray = array of Int32;

function func(a, b: TSArray): TIntArray; external 'CAT' name 'func_ccreg_sum_TSArrays'; 

var
  R: TIntArray;  
  A, B: TSArray;

procedure Test;
var
  i: Int32;
begin
  for i := 0 to 9 do begin
    A[i] := i;
    B[i] := i;    
  end;
  R := func(a, b);  
  Assert(R[19] = 9);
end;


initialization
  Test();

finalization

end.