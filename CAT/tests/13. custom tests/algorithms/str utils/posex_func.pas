unit posex_func;

interface

implementation

function Pos(const SubStr, Str: string; Offset: Int32): Int32;
var
  I, LIterCnt, L, J: Int32;
begin
  L := Length(SubStr);
  LIterCnt := Length(Str) - L - Offset;

  if (Offset >= 0) and (LIterCnt >= 0) and (L > 0) then
  begin
    for I := 0 to LIterCnt do
    begin
      J := 0;
      while (J >= 0) and (J < L) do
      begin
        if Str[Offset + I + J] = SubStr[J] then
          Inc(J)
        else
          J := -1;
      end;
      if J >= L then
        Exit(Offset + I);
    end;
  end;
  Result := -1;
end;

  var 
  p: Int32;

procedure Test;

begin
  p := Pos('PAD', 'PADDING', 0);
  Assert(p = 0);
  
  p := Pos('AD', 'PADDING', 1);
  Assert(p = 1);
  
  p := Pos('D', 'PADDING', 3);
  Assert(p = 3);
  
  p := Pos('ING', 'PADDING', 3);
  Assert(p = 4);
  
  p := Pos('NG', 'PADDING', 1);
  Assert(p = 5);
  
  p := Pos('G', 'PADDING', 0);
  Assert(p = 6);
  
  p := Pos(';', '111;222', 0);
  Assert(p = 3);
  
  p := Pos('33', '111;222', 0);
  Assert(p = -1);              
end;

initialization
  Test();

finalization

end.