unit sort_bubble_2;

interface

implementation

type
  TIntArray = array of int32;

var A: TIntArray;

procedure BubbleSort(Arr: TIntArray);
var 
  i, j, k, l: Int32; 
begin
  l := High(Arr);
  i := 0;
  while i < l do
  begin
    j := 0;
    while j < l - i do
    begin     
      if Arr[j] > Arr[j+1] then 
      begin
        k := Arr[j];
        Arr[j] := Arr[j+1];
        Arr[j+1] := k;
      end;
      j := j + 1;       
    end;  
    i := i + 1;     
  end;    
end;

procedure Test;
begin
  A := [4, 7, 1 , 6, 2, 0, 3, 5, 9, 8];
  BubbleSort(A); 
end;

initialization
  Test();

finalization
  Assert(A[0] = 0);
  Assert(A[9] = 9);  
end.