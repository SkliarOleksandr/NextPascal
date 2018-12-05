unit guid_2;

interface

implementation

type
  IIntf = interface
    ['{B72055C7-0F02-4D4C-88E2-4708A57FD56C}']
  end;

var
  G1, G2: TGUID;

procedure Test;
begin
  G1 := IIntf;
  G2 := IIntf;  
end;

initialization
  Test();

finalization
  Assert(G1 = G2);

end.