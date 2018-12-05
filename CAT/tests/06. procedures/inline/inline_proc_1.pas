unit inline_proc_1;

interface

var
  G1: Int32;
  G2: Int32;

implementation
  
procedure InlineProc(V: int32); inline;
begin
  G1 := V - 1;
end;

function InlineFunc(V: int32): Int32; inline;
begin
  Result := V + 1;
end;

procedure Test;
begin
  InlineProc(5);
  G2 := InlineFunc(5);
end;

initialization
  Test();

finalization  
  Assert(G1 = 4);
  Assert(G2 = 6);  

end.