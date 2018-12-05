unit pos_func;

interface

implementation


function Pos(const SubStr, Str: string): Int32;
var
  I, LIterCnt, L, J: Int32;
begin
  L := Length(SubStr);
  LIterCnt := Length(Str) - L;

  if (LIterCnt >= 0) and (L > 0) then
  begin
    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        if Str[I + J] = SubStr[J] then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(I);
    end;
  end;
  Result := -1;
end;

procedure Test;
var 
  p: Int32;
begin
  p := Pos('PAD', 'PADDING');
  Assert(p = 0);
  
  p := Pos('AD', 'PADDING');
  Assert(p = 1);
  
  p := Pos('D', 'PADDING');
  Assert(p = 2);
  
  p := Pos('ING', 'PADDING');
  Assert(p = 4);
  
  p := Pos('NG', 'PADDING');
  Assert(p = 5);
  
  p := Pos('G', 'PADDING');
  Assert(p = 6);         
  
  p := Pos('X', 'PADDING');
  Assert(p = -1);        
end;

initialization
  Test();

finalization

end.