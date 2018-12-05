#skip
unit guid_3;

interface

implementation

const 
  C: TGUID = '{B72055C7-0F02-4D4C-88E2-4708A57FD56C}';

var
  G: TGUID;

procedure Test;
begin
  G := C;
end;

initialization
  Test();

finalization


end.