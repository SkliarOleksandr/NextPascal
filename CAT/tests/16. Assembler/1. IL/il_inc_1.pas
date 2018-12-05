unit il_inc_1;

interface

implementation

var G: Int32;

procedure Test;
begin
  asm
    inc G;  
  end;
end;

initialization
  Test();

finalization
  Assert(G = 1);
end.