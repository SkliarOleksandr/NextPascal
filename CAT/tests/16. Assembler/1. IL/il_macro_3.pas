unit il_macro_3;

interface

implementation

var
  G: Pointer;

procedure Test;
begin
  asm
    macro 'getunitslist', G;
  end;
end;

initialization
  Test();

finalization

end.