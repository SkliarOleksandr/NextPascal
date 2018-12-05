unit add_string_seg_1;

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
  S := 'X0';
  S := AddStringSegment(S, 'X1', ',');
  Assert(S = 'X0,X1');
  S := AddStringSegment(S, 'X2', ',');
  Assert(S = 'X0,X1,X2');
  S := AddStringSegment(S, '', ',');
  Assert(S = 'X0,X1,X2');     
end;

initialization
  Test();

finalization

end.