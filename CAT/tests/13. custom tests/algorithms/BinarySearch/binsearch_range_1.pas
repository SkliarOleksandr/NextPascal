unit binsearch_range_1;

interface

implementation

type
  TIntArray = array of Integer;

function SearchInRange(const Values: TIntArray; const Value: Integer; out FoundIndex: Integer): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
  LastState: (ssNone, sLess, sGreater);
begin
  Result := False;
  L := 0;
  H := Length(Values) - 1;
  LastState := ssNone;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := Values[mid] - Value;
    if cmp < 0 then
    begin
      if L = H then
      begin
        FoundIndex := L;
        Exit;
      end;
      L := mid + 1;
      LastState := sGreater;
    end else
    if cmp = 0 then
    begin
      FoundIndex := mid;
      Result := True;
      Exit;
    end else
    begin
      if (L = H) and (LastState = sGreater) then
      begin
        FoundIndex := L - 1;
        Exit;
      end;
      H := mid - 1;
      LastState := sLess;
    end;
  end;
  FoundIndex := H;
end;

var 
  A: TIntArray;
  I: Integer;
  B: Boolean;
  
procedure Test1;
begin          
  A := [];
  B := SearchInRange(A, 5, I);
  Assert(I = -1);
end;  
  
procedure Test2;
begin          
  A := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  B := SearchInRange(A, 5, I);
  Assert(I = 5);
end;

procedure Test3;
begin          
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 3, I);
  Assert(I = 1);
end;

procedure Test4;
begin          
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 5, I);
  Assert(I = 2);
end;

procedure Test5;
begin          
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 9, I);
  Assert(I = 4);
end;

procedure Test6;
begin          
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 10, I);
  Assert(I = 5);
end;

procedure Test7;
begin          
  A := [0, 2, 4, 6, 8, 10];
  B := SearchInRange(A, 11, I);
  Assert(I = 5);
end;

initialization    
  Test1();
  Test2();
  Test3();
  Test4();
  Test5();
  Test6();
  Test7();          

finalization

end.