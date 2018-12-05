unit string_segment;

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

function StringSegCount(const Str: string; const Separator: string = ','): Integer;
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

function StringSegment(const Str: string; Idx: Integer; const Separator: string = ','): string;
var
  SPos, NPos, StrLen: integer;
begin
  Result := '';
  SPos := -1;
  StrLen := Length(Str);
  repeat
    Dec(Idx);
    Inc(SPos);    
    NPos := Pos(Separator, Str, SPos);
    if Idx <= 0 then begin
      if NPos = -1 then
        NPos := StrLen;
      Result := Copy(Str, SPos, NPos - SPos);
      Exit;   
    end;
    SPos := NPos;
  until (SPos = -1) or (SPos >= StrLen); 
end;

var 
  Res: string;
  Str: string;

procedure Test;
begin
  Str := '111,222,,5'; 
  Res := StringSegment(Str, 0);
  Assert(Res = '111');    

  Res := StringSegment(Str, 1);
  Assert(Res = '111');

  Res := StringSegment(Str, 2);
  Assert(Res = '222');         
  
  Res := StringSegment(Str, 3);
  Assert(Res = '');  

  Res := StringSegment(Str, 4);
  Assert(Res = '5');      
  
  Res := StringSegment(Str, 6);
  Assert(Res = '');    
end;

initialization
  Test();

finalization

end.