unit add_string_seg_2;

interface

implementation

function AddStringSegment(const Str, Seg: string; const Separator: string): string;
var
  sl: int32;
begin  
  sl := length(Separator);
  if (Str <> '') and 
     (Seg <> '') and  
     (copy(Str, length(Str) - sl, sl) <> Separator) and
     (copy(Seg, 0, sl) <> Separator) then
    Result := Str + Separator + Seg
  else  
    Result := Str + Seg;
end;

var S: string;

procedure Test;
begin
  S := AddStringSegment(S, 'X', ',');
  S := AddStringSegment(S, 'YY', ',');
  S := AddStringSegment(S, 'ZZZ', ',');    
end;

initialization
  Test();

finalization
  Assert(S = 'X,YY,ZZZ');
end.