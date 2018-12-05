unit string_seg_count;

interface

implementation

type Integer = Int32;

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

function StringSegCount(const Str: string; const Separator: string): Integer;
var
  SPos, StrLen: integer;
begin
  SPos := -1;
  Result := 0;
  StrLen := Length(Str);
  repeat
    Inc(Result);
    Inc(SPos);    
    SPos := Pos(separator, Str, SPos);
  until (SPos = -1) or (SPos >= StrLen);
end;

var G: Int32;

procedure Test;
begin
  G := StringSegCount('111;222', ';');
end;

initialization
  Test();

finalization
  Assert(G = 2);
  
end.