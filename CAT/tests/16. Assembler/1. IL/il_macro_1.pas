unit il_macro_1;

interface

implementation

var
  G: Pointer;

procedure Test;
begin
  asm
    macro 'getcallstack', G;
  end;
end;

initialization
  Test();

finalization

end.