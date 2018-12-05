unit il_macro_2;

interface

implementation

var
  G: Pointer;

procedure Test;
begin
  asm
    macro 'getcurrentunit', G;
  end;
end;

initialization
  Test();

finalization

end.