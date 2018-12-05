unit sort_quick_1;

interface

implementation

type TIntArray = array of int32;
var A: TIntArray;

function Compare(L, R: Int32): Int32;
begin
  if L < R then
    Result := -1
  else if L > R then
    Result := 1
  else
    Result := 0;
end;

procedure QuickSort(Values: TIntArray; L, R: Int32);
var
  I, J: Int32;
  pivot, temp: Int32;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Compare(Values[I], pivot) < 0 do
        Inc(I);
      while Compare(Values[J], pivot) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Values, L, J);
    L := I;
  until I >= R;
end;

procedure Test;
begin
  A := [4, 7, 1, 6, 2, 0, 3, 5, 9, 8];
  QuickSort(A, Low(A), High(A));
end;

initialization
  Test();

finalization
  Assert(A[0] = 0);
  Assert(A[9] = 9);  
end.